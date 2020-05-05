(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
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

(* Hide the row [tr] of a table element *)
let hide tr =
  tr##.style##.display := _s "none"

(* Make visible the row [tr] of a table element *)
let show tr =
  tr##.style##.display := _s ""

(* Filter the string [str] from the table [tbl] by looking in the column
   name (position 0) and the description (position 2) *)
let filter str tbl =
  for i = 1 to tbl##.rows##.length do
    Js.Opt.iter (tbl##.rows##item (i)) @@ fun tr ->
    (* Get the [td] corresponding to the name column *)
    let name = tr##.cells##item (by_name) in
    (* Get the [td] corresponding to the description column *)
    let descr = tr##.cells##item (by_descr) in
    let matches str elt =
      Js.Opt.case elt (fun () -> false) @@ fun e ->
      None <>
      Regexp.search (Regexp.regexp_string_case_fold (Js.to_string str))
        (Js.to_string e##.innerHTML) 0
    in
    (* Filter name or column column of the table *)
    if matches str name || matches str descr
    then show tr
    else hide tr
  done

let ( >>= ) = Js.Opt.bind

let _ =
  doc##getElementById (Js.string "search") >>= Dom_html.CoerceTo.input
  >>= fun search ->
  doc##getElementById (Js.string "packages") >>= Dom_html.CoerceTo.table
  >>= fun tbl ->
  let handler =
    Dom_html.handler (fun _ -> filter search##.value tbl; Js._false)
  in
  search##.onkeyup := handler;
  let hash = win##.location##.hash##substring_toEnd 1 in
  if hash##.length > 0 then search##.value := hash;
  if search##.value##.length > 0 then filter search##.value tbl;
  Js.some handler
