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


(* Retrieve the 'ntop' number of packages with the higher (or lower)
   value associated *)
let top_packages ?ntop ?(reverse = true) stats packages =
  let compare_pkg (_, n1) (_, n2) =
    if reverse
    then compare n2 n1
    else compare n1 n2
  in
  let pkgs = OpamPackage.Set.elements packages in
  let pkg_stats = List.rev_map (fun pkg -> pkg, stats pkg) pkgs in
  let sorted_pkg = List.sort compare_pkg pkg_stats in
  match ntop with
  | None      -> sorted_pkg
  | Some nmax -> O2wMisc.first_n nmax sorted_pkg
