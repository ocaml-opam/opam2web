(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  Opam is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open Js_of_ocaml

(* Code from ocp-jslib in TryOCaml *)
let doc = Dom_html.document
let win = Dom_html.window
let _s = Js.string

let get_element_by_id id =
  Js.Opt.to_option (doc##getElementById (Js.string id))

let from_option opt =
  match Js.Opt.to_option opt with
  | None -> assert false
  | Some t -> t

(* Column position in the HTML table *)
let by_name = 0
let by_descr = 2

(* Where to search  *)
type scope =
  | In_package (* name, synopsis, and tags *)
  | Deps       (* the "depends" field only *)

let scope_of_string = function
  | "deps" -> Deps
  | _ -> In_package

(* Hide the row [tr] of a table element *)
let hide tr =
  tr##.style##.display := _s "none"

(* Make visible the row [tr] of a table element *)
let show tr =
  tr##.style##.display := _s ""

let attribute elt name =
  Js.Opt.case (elt##getAttribute (_s name)) (fun () -> "") Js.to_string

let cell tr i =
  Js.Opt.case (tr##.cells##item (i)) (fun () -> "")
    (fun td -> Js.to_string td##.innerHTML)

(* The scope currently selected in the search box is the active entry of the
   dropdown menu: no need to keep a copy of it on our side *)
let current_scope () =
  Js.Opt.case
    (doc##querySelector (_s "#search-scope-menu li.active > a[data-scope]"))
    (fun () -> In_package)
    (fun item -> scope_of_string (attribute item "data-scope"))

(* Filter the string [str] from the table [tbl], looking at the fields
   selected by [scope] *)
let filter ~scope str tbl =
  let re = Regexp.regexp_string_case_fold (Js.to_string str) in
  let matches s = None <> Regexp.search re s 0 in
  for i = 1 to tbl##.rows##.length do
    Js.Opt.iter (tbl##.rows##item (i)) @@ fun tr ->
    let searched = match scope with
      (* The dependencies are not displayed in the table: they are carried by
         the [data-deps] attribute of the row *)
      | Deps       -> [attribute tr "data-deps"]
      (* Name column (position 0) and description column (position 2) *)
      | In_package -> [cell tr by_name; cell tr by_descr]
    in
    if List.exists matches searched
    then show tr
    else hide tr
  done

(* The entries of the dropdown menu used to select the scope *)
let scope_items () =
  match get_element_by_id "search-scope-menu" with
  | None -> []
  | Some menu ->
    let nodes = menu##querySelectorAll (_s "a[data-scope]") in
    let rec aux acc i =
      if i < 0 then acc else
        aux
          (Js.Opt.case (nodes##item (i)) (fun () -> acc) @@ fun node ->
           Js.Opt.case (Dom_html.CoerceTo.element node) (fun () -> acc)
             (fun e -> e :: acc))
          (i - 1)
    in
    aux [] (nodes##.length - 1)

(* Select [item] in the dropdown menu: mark it as the active entry and update
   the label of the search button *)
let select_scope items item =
  List.iter (fun i ->
      Js.Opt.iter (i##.parentNode) @@ fun parent ->
      Js.Opt.iter (Dom_html.CoerceTo.element parent) @@ fun li ->
      li##.className := _s (if i == item then "active" else ""))
    items;
  match get_element_by_id "search-scope-label" with
  | None -> ()
  | Some label -> label##.innerHTML := _s (attribute item "data-label")

let filter_tag tag tbl =
  let count = ref 0 in
  for i = 1 to tbl##.rows##.length do
    Js.Opt.iter (tbl##.rows##item (i)) @@ fun tr ->
    (* [data-tags] is a space-separated list of percent-encoded tags, as a
       tag can itself contain spaces *)
    let tags =
      Js.Opt.case (tr##getAttribute (_s "data-tags"))
        (fun () -> [])
        (fun s ->
           List.map
             (fun t -> Js.to_string (Js.decodeURIComponent (_s t)))
             (String.split_on_char ' ' (Js.to_string s)))
    in
    if List.mem (Js.to_string tag) tags
    then (show tr; incr count)
    else hide tr
  done;
  !count

(* Line above the table with the number of packages. Hidden while no tag filter is active *)
let show_tag_count tag n =
  match get_element_by_id "tag-count" with
  | None -> ()
  | Some p ->
    let count =
      if n = 1 then "1 package has tag "
      else string_of_int n ^ " packages have tag "
    in
    let b = doc##createElement (_s "b") in
    b##.textContent := Js.some tag;
    (* Assigning [textContent] also drops the previous children *)
    p##.textContent := Js.some (_s count);
    Dom.appendChild p b;
    Dom.appendChild p (doc##createTextNode (_s "."));
    p##.style##.display := _s ""

let hide_tag_count () =
  match get_element_by_id "tag-count" with
  | None -> ()
  | Some p -> p##.style##.display := _s "none"

let close_tags_list () =
  match get_element_by_id "tags-list" with
  | None -> ()
  | Some details -> details##removeAttribute (_s "open")

(* Clickable list of tags above the table *)
let tag_links () =
  let nodes = doc##querySelectorAll (_s "#tags-list a[data-tag]") in
  let rec aux acc i =
    if i < 0 then acc else
      aux
        (Js.Opt.case (nodes##item (i)) (fun () -> acc) @@ fun node ->
         Js.Opt.case (Dom_html.CoerceTo.element node) (fun () -> acc)
           (fun e -> e :: acc))
        (i - 1)
  in
  aux [] (nodes##.length - 1)

let ( >>= ) = Js.Opt.bind

let _ =
  doc##getElementById (Js.string "search") >>= Dom_html.CoerceTo.input
  >>= fun search ->
  doc##getElementById (Js.string "packages") >>= Dom_html.CoerceTo.table
  >>= fun tbl ->
  let refresh () = filter ~scope:(current_scope ()) search##.value tbl in
  let handler = Dom_html.handler (fun _ ->
        hide_tag_count (); refresh (); Js._false) in
  search##.onkeyup := handler;
  let items = scope_items () in
  List.iter (fun item ->
      item##.onclick :=
        Dom_html.handler (fun _ -> select_scope items item; refresh (); Js._false))
    items;
  List.iter (fun link ->
      link##.onclick :=
        Dom_html.handler (fun _ ->
            Js.Opt.iter (link##getAttribute (_s "data-tag")) (fun tag ->
                (* The tag filter replaces whatever search was in progress *)
                search##.value := _s "";
                show_tag_count tag (filter_tag tag tbl);
                close_tags_list ();
                (* Record the tag in the URL for bookmarking. Not done by
                   following the "#<tag>" href: navigating to an anchor
                   inside the <details> would fold it back open. Best-effort:
                   some environments forbid replaceState on file:// pages *)
                try
                  win##.history##replaceState Js.null (_s "")
                    (Js.some ((_s "#")##concat (Js.encodeURIComponent tag)))
                with _ -> ());
            Js._false))
    (tag_links ());
  (* Arriving with a "#<tag>" fragment (a bookmark, or a tag link on a
     package page): filter by that exact tag *)
  let hash = win##.location##.hash##substring_toEnd 1 in
  if hash##.length > 0 then begin
    let tag = Js.decodeURIComponent hash in
    show_tag_count tag (filter_tag tag tbl)
  end
  else if search##.value##.length > 0 then refresh ();
  Js.some handler
