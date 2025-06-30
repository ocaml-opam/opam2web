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


(* Field names *)
let field_package = "package"
let field_dl = "downloads"
let field_m_dl = "month_downloads"
let field_tm = "date"
let field_name = "name"
let field_version = "version"

(* Basic json functions *)
let json_package pkg = field_package, `String (OpamPackage.to_string pkg)
let json_name pkg = field_name, `String (OpamPackage.Name.to_string pkg)
let json_version pkg = field_version, `String (OpamPackage.Version.to_string pkg)
let json_downloads dl = field_dl, `Int (Int64.to_int dl)
let json_month_downloads dl = field_m_dl,  `Int (Int64.to_int dl)
let json_timestamp tm = field_tm, `String (O2wMisc.string_of_timestamp tm)

(* Options functions *)
let json_opt json_f n = function
  | Some js -> json_f js
  | None -> n, `Null
let json_downloads_opt = json_opt json_downloads field_dl
let json_month_downloads_opt = json_opt json_month_downloads field_m_dl
let json_timestamp_opt = json_opt json_timestamp field_tm

let json_dir = "json"
let write file json =
  let dir = OpamFilename.Dir.of_string json_dir in
  (if not (OpamFilename.exists_dir dir) then
     OpamFilename.mkdir dir);
  let file = OpamFilename.(to_string Op.(dir // (file ^ ".json"))) in
  Yojson.to_file file json
