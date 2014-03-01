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

open O2wTypes

(* The list contains elements with this syntax :
   <string> without extension -> A menu title
   <string> with 'md' extension -> A markdown page in content/doc
   empty <string> -> A menu divider
 *)
let documentation_pages = [
  "Primer";
  "Quick_Install.md";
  "Basic_Usage.md";
  "Specifying_Solver_Preferences.md";
  "FAQ.md";
  "";
  "Go Further";
  "Advanced_Install.md";
  "Advanced_Usage.md";
  "Developing.md";
  "";
  "For Packagers";
  "Packaging.md"
]

let default_log_filter =
  let log_custom entry =
    match entry.log_request with
    | Archive_req _ | Update_req -> true
    | Html_req _ | Unknown_req _ -> false
  in {
    filter_name = "default";
    log_per_ip = false;
    log_start_time = 0.;
    log_end_time = Unix.time ();
    log_custom;
  }
