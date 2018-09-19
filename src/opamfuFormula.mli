(*
 * Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** Simplified boolean expression type *)
type 'a expr = Atom of 'a | And of 'a expr list | Or of 'a expr list

type version = OpamPackage.Version.t
type version_set = OpamPackage.Version.Set.t
type version_dnf = OpamFormula.version_constraint OpamFormula.dnf
type version_expr = OpamFormula.version_constraint expr option
type t = (OpamPackage.Name.t * version_expr) expr option

val eval : ('a -> bool) -> 'a expr option -> bool

(** [interpret and_op or_op atom_op zero t] will interpret [t] with
    the provided operators.
*)
val interpret :
  ('z -> 'z -> 'z) -> ('z -> 'z -> 'z) -> ('x -> 'z) ->
  'z -> ('x expr option) -> 'z

val map : ('a -> 'b) -> 'a expr -> 'b expr

val to_opam_formula : 'a expr option -> 'a OpamFormula.formula
val of_opam_formula : OpamFormula.t -> t

val dnf_of_expr : 'a expr option -> 'a expr option

val expr_of_version_dnf : version_dnf -> version_expr

val simplify_expr : 'a expr option -> 'a expr option
val simplify : t -> t

val compare : acompare:('a -> 'a -> int) -> 'a expr -> 'a expr -> int
val sort : ('a expr -> 'a expr -> int) -> 'a expr option -> 'a expr option
val sort_formula :
  ?ncompare:(OpamPackage.Name.t -> OpamPackage.Name.t -> int) ->
  ?vcompare:(OpamPackage.Version.t -> OpamPackage.Version.t -> int) ->
  t -> t

val max_depth : t -> int

val count_width : ('a -> int) -> int -> 'a expr -> int
val expr_width : 'a expr -> int
val width : t -> int

val filter_versions : version_expr -> version_set -> version_set

val extremum_of_version_constraint :
  version_set -> OpamFormula.version_constraint -> version option

val dnf_of_version_subset : version_set -> version_set -> version_dnf

val could_satisfy : version_set OpamTypes.name_map -> t -> bool
