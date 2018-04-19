(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012 INRIA                                                *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2013 David Sheets                                         *)
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

type 'a pkg = {
  name : string;
  version : string;
  descr : 'a;
  synopsis : string;
  href : Uri.t;
  title : string;
  published : float option;
  url : OpamFile.URL.t option;
}

type pkg_idx = (OpamTypes.repository_name * string option) OpamTypes.package_map

type repository = string

type pred =
| Tag of string
| Depopt
| Not of pred
| Repo of string
| Pkg of string

type index = Index_pred | Index_all

type pred_dnf = pred OpamFormula.dnf

type 'a t = {
  repos : OpamTypes.repository OpamTypes.repository_name_map;
  preds : pred_dnf;
  index : index;
  pkg_idx : pkg_idx;
  versions : OpamTypes.version_set OpamTypes.name_map;
  max_packages : OpamTypes.package_set;
  max_versions : OpamTypes.version OpamTypes.name_map;
  rev_depends : OpamfuFormula.version_dnf OpamTypes.name_map OpamTypes.package_map;
  rev_depopts : OpamfuFormula.version_dnf OpamTypes.name_map OpamTypes.package_map;
  pkgs_infos : 'a pkg OpamTypes.package_map;
  pkgs_opams : OpamFile.OPAM.t OpamTypes.package_map;
  pkgs_dates : float OpamTypes.package_map;
}

module Repo : sig val links : OpamTypes.repository -> OpamFile.Repo.t end

module Pkg : sig
  val to_repo : 'a t -> OpamPackage.t -> OpamTypes.repository
  val are_preds_satisfied :
    OpamFile.OPAM.t OpamTypes.package_map -> pkg_idx -> pred_dnf ->
    OpamPackage.t -> bool
  val href :
    ?href_base:Uri.t ->
    OpamPackage.Name.t -> OpamPackage.Version.t -> Uri.t
  val get_info :
    dates:float OpamPackage.Map.t ->
    OpamTypes.repository ->
    string option -> OpamTypes.package -> (string * string) pkg
end

val pred_sep : char

val repository_ns_sep : char

val string_of_repository : repository -> string

(** Can raise Not_found if string is unparseable *)
val repository_of_string : string -> repository

val index_by_repo :
  pkg_idx -> (string option OpamTypes.package_map) OpamTypes.repository_name_map

val is_base_package : OpamTypes.package -> bool

val remove_base_packages : 'a OpamTypes.package_map -> 'a OpamTypes.package_map

val of_repositories :
  ?preds:pred_dnf -> index -> repository list -> (string * string) t

val map : ('a -> 'b) -> 'a t -> 'b t
