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

module F = OpamFormula

type 'a expr =
| Atom of 'a
| And of 'a expr list
| Or of 'a expr list

type version = OpamPackage.Version.t
type version_set = OpamPackage.Version.Set.t
type version_dnf = OpamFormula.version_constraint OpamFormula.dnf
type version_expr = OpamFormula.version_constraint expr option
type package_expr = (OpamPackage.Name.t * version_expr) expr
type t = package_expr option

let eval atom =
  let rec eval = function
    | Atom x -> atom x
    | And cl -> List.for_all eval cl
    | Or dl  -> List.exists eval dl
  in function None -> true | Some ex -> eval ex

let interpret op_and op_or atom z =
  let rec interpret = function
    | Atom x -> atom x
    | And cl -> List.fold_right (fun c i -> op_and (interpret c) i) cl z
    | Or dl  -> List.fold_right (fun c i -> op_or (interpret c) i) dl z
  in function None -> z | Some ex -> interpret ex

(* TODO: these should probably be inverses *)
let rec to_opam_formula = function
  | None | Some (And []) | Some (Or []) -> F.Empty
  | Some (Atom x) -> F.Atom x
  | Some (And (c::cs)) ->
    F.And (to_opam_formula (Some c), to_opam_formula (Some (And cs)))
  | Some (Or (d::ds)) ->
    F.Or (to_opam_formula (Some d), to_opam_formula (Some (Or ds)))

let of_opam_formula f =
  let expr_of_opamf atom =
    let rec collect_and f = match f with
      | F.Empty -> []
      | F.Atom x -> [atom x]
      | F.Block x -> collect_and x
      | F.And (x,y) -> (collect_and x) @ (collect_and y)
      | F.Or _ -> [Or (collect_or f)]
    and collect_or f = match f with
      | F.Empty -> []
      | F.Atom x -> [atom x]
      | F.Block x -> collect_or x
      | F.And _ -> [And (collect_and f)]
      | F.Or (x,y) -> (collect_or x) @ (collect_or y)
    in
    let rec convert f = match f with
      | F.Empty   -> None
      | F.Atom x  -> Some (atom x)
      | F.Block x -> convert x
      | F.And _   -> Some (And (collect_and f))
      | F.Or _    -> Some (Or (collect_or f))
    in
    convert
  in
  expr_of_opamf (fun (name,vf) ->
    Atom (name, expr_of_opamf (fun x -> Atom x) vf)
  ) f

(* convert an expression to DNF *)
let dnf_of_expr t =
  let rec mk_and c = function
    | Or dl -> Or (List.rev_map (mk_and c) dl)
    | And cl -> And (c::cl)
    | Atom x -> And [c; Atom x]
  in
  let rec mk_distr = function
    | []           -> Or []
    | (Or dl)::tl  -> Or (List.rev_map (fun d -> mk_distr (d::tl)) dl)
    | c::cl        -> List.fold_left mk_and c cl
  in
  let rec mk = function
    | Atom x -> Atom x
    | Or dl  -> Or (List.rev_map mk dl)
    | And cl -> mk_distr (List.rev_map mk cl)
  in
  match t with None -> None | Some t -> Some (mk t)

let (&&&) x y = match x,y with
  | None, x | x, None -> x
  | Some x, Some y -> Some (And [x; y])

let (|||) x y = match x,y with
  | None, x | x, None -> x
  | Some x, Some y -> Some (Or [x; y])

let expr_of_version_dnf vdnf = List.fold_right
  (fun conjl t -> match conjl with
  | []  -> t
  | xs -> (List.fold_right (fun h t -> (Some (Atom h)) &&& t) xs None) ||| t
  ) vdnf None

let rec fix fn x =
  let x' = fn x in
  if x = x' then x else fix fn x'

let simplify_expr ex =
  (* fold_right is important to converge *)
  let rec step = function
    | None -> None
    | Some (Atom x) -> Some (Atom x)
    | Some (And []) | Some (Or []) -> None
    | Some (And [x]) | Some (Or [x]) -> Some x
    | Some (And xs) -> Some (And (List.fold_right (fun x n ->
      match fix step (Some x) with
      | None -> n
      | Some (And conjl) -> conjl @ n
      | Some ex -> ex::n
    ) xs []))
    | Some (Or xs) -> Some (Or (List.fold_right (fun x n ->
      match fix step (Some x) with
      | None -> n
      | Some (Or disjl) -> disjl @ n
      | Some ex -> ex::n
    ) xs []))
  in
  fix step ex

let simplify ex =
  let open OpamPackage.Name in
  let rec consolidate = function
    | Atom x -> Atom x
    | And conjl ->
      let map, list = List.fold_left (fun (map,list) -> function
        | Atom (name,vdnf) -> begin
          try let vf = Map.find name map in
              Map.add name (vdnf &&& vf) map
          with Not_found -> Map.add name vdnf map
        end, list
        | ex -> map, (consolidate ex)::list
      ) (Map.empty,[]) conjl in
      And (List.fold_left (fun l (name,vf) ->
        (Atom (name, simplify_expr vf))::l
      ) list (Map.bindings map))
    | Or disjl ->
      let map, list = List.fold_left (fun (map,list) -> function
        | Atom (name,vdnf) ->
          begin
          try let vf = Map.find name map in
              Map.add name (vdnf ||| vf) map
          with Not_found -> Map.add name vdnf map
        end, list
        | ex -> map, (consolidate ex)::list
      ) (Map.empty,[]) disjl in
      Or (List.fold_left (fun l (name,vf) ->
        (Atom (name, simplify_expr vf))::l
      ) list (Map.bindings map))
  in
  match simplify_expr ex with
  | None -> None
  | Some ex -> simplify_expr (Some (consolidate ex))

let rec compare_list compare lst lst' = match lst, lst' with
  | [], [] -> 0
  | [], _::_ -> 1
  | _::_, [] -> -1
  | h::t, h'::t' ->
    let c = compare h h' in
    if c = 0 then compare_list compare t t' else c

let rec compare ~acompare t t' =
  let cmp l l' = compare_list (compare ~acompare) l l' in
  match t, t' with
  | Atom a, Atom a' -> acompare a a'
  | Atom _, _ -> -1
  | _, Atom _ -> 1
  | And conjl, And conjl' -> cmp conjl conjl'
  | And _, _ -> -1
  | _, And _ -> 1
  | Or disjl, Or disjl' -> cmp disjl disjl'

let sort cmp t =
  let rec sort_flist fl = List.sort cmp (List.rev_map sort_expr fl)
  and sort_expr = function
    | Atom x    -> Atom x
    | And conjl -> And (sort_flist conjl)
    | Or disjl  -> Or (sort_flist disjl)
  in
  match t with
  | None -> None
  | Some e -> Some (sort_expr e)

let rec map f = function
  | Atom x -> Atom (f x)
  | And cl -> And (List.map (map f) cl)
  | Or dl  -> Or  (List.map (map f) dl)

let sort_formula
    ?(ncompare=OpamPackage.Name.compare)
    ?(vcompare=OpamPackage.Version.compare)
    = function None -> None
    | Some t -> sort
      (compare ~acompare:(fun (n,_) (n',_) -> ncompare n n'))
      (Some (map (fun (n,vc) ->
        (n, sort (compare ~acompare:(fun (_,v) (_,v') -> vcompare v v')) vc)
       ) t))

let max_depth =
  let rec loop adepth = function
    | Atom a -> adepth a
    | And xs | Or xs ->
      1 + List.fold_left (fun m x -> max (loop adepth x) m) 0 xs
  in
  function None -> 0 | Some t ->
    loop (function (_,None) -> 1 | (_,Some vf) -> 1 + loop (fun _ -> 1) vf) t

let rec count_width awidth w = function
  | Atom a -> w + (awidth a)
  | And xs | Or xs -> List.fold_left (count_width awidth) w xs

let expr_width ex = count_width (fun _ -> 1) 0 ex

let width = function None -> 0 | Some t ->
  count_width (function
  | (_,None) -> 1
  | (_,Some vf) -> max 1 (expr_width vf)
  ) 0 t

(*
let filter_versions vf vset = OpamPackage.Version.Set.(
  interpret inter union (fun set (relop,v') ->
    filter (fun v -> OpamFormula.eval_relop relop v v') set
  ) vset vf
)
*)

let filter_versions vf = OpamPackage.Version.Set.filter (fun v ->
  eval (fun (relop,v') -> OpamFormula.eval_relop relop v v') vf
)

let extremum_of_version_constraint versions (relop,v) =
  let open OpamPackage.Version.Set in
  match relop with
  | `Eq | `Neq -> if mem v versions then Some v else None
  | `Geq | `Gt -> begin
    try
      Some (min_elt (filter_versions (Some (Atom (`Geq,v))) versions))
    with Not_found -> None end
  | `Leq | `Lt -> begin
    try
      Some (max_elt (filter_versions (Some (Atom (`Leq,v))) versions))
    with Not_found -> None end

let dnf_of_version_subset versions vset =
  let open OpamPackage.Version in
  let vs = List.sort compare (Set.elements versions) in
  let rec one f = function
    | [] -> f
    | h::t when Set.mem h vset -> range f h t
    | _::t -> one f t
  and range f v = function
    | [] -> [`Geq,v]::f
    | h::t when Set.mem h vset -> range f v t
    | h::t -> one ([`Geq, v; `Lt, h]::f) t
  in
  let rec zero f = function
    | [] -> f
    | h::t when Set.mem h vset -> zero f t
    | h::t -> one ([`Lt, h]::f) t
  in
  match vs with
  | [] -> []
  | h::t when Set.mem h vset -> List.rev (zero [] t)
  | _::t -> List.rev (one [] t)

let could_satisfy nvsetmap t =
  let maybe op x y = match x,y with
    | None, None   -> None
    | Some x, None | None, Some x -> Some x
    | Some x, Some y -> Some (op x y)
  in
  let known = interpret (maybe (&&)) (maybe (||)) (fun (n,vf) ->
    try
      let vset = OpamPackage.Name.Map.find n nvsetmap in
      Some (eval (fun (relop,v) ->
        OpamPackage.Version.Set.for_all (fun version ->
          OpamFormula.eval_relop relop version v
        ) vset) vf)
    with Not_found -> None
  ) None t in
  match known with None -> true | Some x -> x
