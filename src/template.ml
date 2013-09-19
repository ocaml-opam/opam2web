(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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

type param_prop = Default of Cow.Xml.signal list | Mandatory
type field_prop = Optional | Required

type t = {
  path : string;
  fields: (string * (param_prop * field_prop)) list;
}

let xmlns = "http://ocaml.org/xmlns/template#"

let serialize xml =
  let rec aux acc = function
    | `Data d -> (`Data d)::acc
    | `El (tag,contents) ->
      `El_end::(List.fold_left aux ((`El_start tag)::acc) contents)
  in
  List.rev (List.fold_left aux [] xml)

let default html = Default (serialize html)
let mandatory () = Mandatory

(* TODO: cache template *)
let generate content_dir template parameters =
  let tt = template.fields in
  let required = List.fold_left (fun acc -> function
    | (n,(_,Required)) -> n::acc
    | _ -> acc
  ) [] tt in
  let filename = Filename.concat content_dir template.path in
  let ic = open_in filename in
  let xml_stream = Cow.Xml.make_input (`Channel ic) in
  let input () =
    try
      Cow.Xml.input xml_stream
    with
      | Xmlm.Error ((line,char),e) ->
        Printf.eprintf "XML parse error at %s:%d:%d:\n%!" filename line char;
        prerr_endline (Xmlm.error_message e);
        exit 1 in
  let signals = ref [] in
  let output signal = signals := signal :: !signals in

  let rsub s v = v - s in
  let consumep = function 0::_ -> true | _ -> false in
  (* sume is stack of depth triggers *)
  let rec run sume req =
    if Cow.Xml.eoi xml_stream then req
    else match input () with
    | `El_start ((ns,el),attrs) when ns=xmlns ->
      handle (List.map ((+) 1) sume) req attrs el
    | `El_start _ as signal ->
      output signal;
      run (List.map ((+) 1) sume) req
    | `El_end when consumep sume -> run (List.map (rsub 1) (List.tl sume)) req
    | `El_end ->
      output `El_end;
      run (List.map (rsub 1) sume) req
    | `Dtd _ -> run sume req
    | `Data _ as signal -> output signal; run sume req
  and handle sume req attrs = function
    | "insert" ->
      let name =
        try List.assoc ("","name") attrs
        with Not_found -> failwith "bad template: insert without @name"
      in

      let (param_prop,_) =
        try List.assoc name tt
        with Not_found -> failwith ("bad template: unknown name '"^name^"'")
      in

      let fragments =
        try List.assoc name parameters
        with Not_found -> begin match param_prop with
        | Default s -> s
        | Mandatory ->
          failwith ("bad template: parameter '"^name^"' mandatory")
        end
      in

      List.iter output fragments;
      run (0::sume) (List.filter ((<>) name) req)
    | "seq" -> run (0::sume) req
    | el -> failwith ("bad template: unknown element '"^el^"'")
  in
  let leftover = run [] required in
  if leftover <> [] then
    let missing = String.concat ", " leftover in
    failwith ("bad template application: fields not found: "^missing)
  else close_in ic;
  List.rev !signals
