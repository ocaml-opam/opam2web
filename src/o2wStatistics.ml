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

let empty_stats = {
  pkg_stats    = OpamPackage.Map.empty;
  global_stats = Int64.zero;
  update_stats = Int64.zero;
  users_stats  = Int64.zero;
}

let empty_stats_set = {
  alltime_stats        = empty_stats;
  day_stats            = empty_stats;
  week_stats           = empty_stats;
  month_stats          = empty_stats;
  month_leaf_pkg_stats = OpamPackage.Map.empty;
}

let timestamp_regexp =
  Re.Str.regexp "\\([0-9]+\\)/\\([A-Z][a-z]+\\)/\\([0-9]+\\):\\([0-9]+\\):\
                 \\([0-9]+\\):\\([0-9]+\\) [-+][0-9]+"

let timestamp_of_entry e =
  let open Unix in
  let open Logentry in
  if Re.Str.string_match timestamp_regexp e.date 0 then
    fst (Unix.mktime {
        tm_mday = int_of_string (Re.Str.matched_group 1 e.date);
        tm_mon  = O2wMisc.month_of_string (Re.Str.matched_group 2 e.date);
        tm_year = int_of_string (Re.Str.matched_group 3 e.date) - 1900;
        tm_hour = int_of_string (Re.Str.matched_group 4 e.date);
        tm_min  = int_of_string (Re.Str.matched_group 5 e.date);
        tm_sec  = int_of_string (Re.Str.matched_group 6 e.date);
        (* Initial dummy values *)
        tm_wday  = 0;
        tm_yday  = 0;
        tm_isdst = false;
      })
  else 0.

let request_of_entry e =
  let html_regexp =
    Re.Str.regexp "GET /\\(.+\\)\\.html HTTP/[.0-9]+" in
  let archive_regexp =
    Re.Str.regexp "GET /archives/\\(.+\\)\\+opam\\.tar\\.gz HTTP/[.0-9]+" in
  let update_regexp =
    Re.Str.regexp "GET /urls\\.txt HTTP/[.0-9]+" in
  let open Logentry in
  let package_of_string str =
    try OpamPackage.of_string (Filename.basename str)
    with OpamStd.Sys.Exit e ->
      failwith ("opam exit with code " ^ string_of_int e)
  in
  try
    if Re.Str.string_match html_regexp e.request 0 then
      Html_req (Re.Str.matched_group 1 e.request)
    else if Re.Str.string_match archive_regexp e.request 0 then
      Archive_req (package_of_string (Re.Str.matched_group 1 e.request))
    else if Re.Str.string_match update_regexp e.request 0 then
      Update_req
    else
      Unknown_req e.request
  with Failure _ -> Unknown_req e.request

let internal_regexp =
  Re.Str.regexp "https?://opam\\.ocaml\\(\\.org\\|pro\\.com\\)/\\(.*\\)/?"

let referrer_of_entry e =
  let open Logentry in
  match e.referrer with
  | "-" -> No_ref
  | s when Re.Str.string_match internal_regexp e.referrer 0 ->
    Internal_ref (Re.Str.matched_group 2 e.referrer)
  | s -> External_ref s

let browser_regexp =
  Re.Str.regexp "\\(MSIE\\|Chrome\\|Firefox\\|Safari\\)"
let os_regexp =
  Re.Str.regexp "\\(Windows\\|Macintosh\\|iPad\\|iPhone\\|\
                 Android\\|Linux\\|FreeBSD\\)"

let client_of_entry e =
  let open Logentry in
  (* TODO: refine client string parsing (versions, more browsers...)  *)
  let match_browser =
    try Re.Str.search_forward browser_regexp e.client 0
    with Not_found -> (-1) in
  let browser =
    if match_browser >= 0 then
      let browser_str =
        try Re.Str.matched_group 1 e.client
        with Not_found -> "" in
      match browser_str with
      | "Chrome" -> Chrome ""
      | "Firefox" -> Firefox ""
      | "MSIE" -> Internet_explorer ""
      | "Safari" -> Safari ""
      | s -> Unknown_browser s
    else
      Unknown_browser "" in
  let match_os =
    try Re.Str.search_forward os_regexp e.client 0
    with Not_found -> (-1) in
  let os =
    if match_os >= 0 then
      let os_str =
        try Re.Str.matched_group 1 e.client
        with Not_found -> "" in
      match os_str with
      | "Windows" -> Windows ""
      | "Macintosh" | "iPad" | "iPhone" -> Mac_osx ""
      | "Linux" | "FreeBSD" -> Unix ""
      | s -> Unknown_os s
    else
      Unknown_os "" in
  os, browser

let mk_entry e = {
  log_request   = request_of_entry e;
  log_timestamp = timestamp_of_entry e;
  log_referrer  = referrer_of_entry e;
  log_client    = client_of_entry e;
  log_host      = e.Logentry.host;
}

open Readcombinedlog

let now = O2wGlobals.default_log_filter.log_end_time
let one_day = 3600. *. 24.
let one_month = one_day *. 30.
let one_day_ago = now -. one_day
let one_week_ago = now -. (one_day *. 7.)
let one_month_ago = now -. (one_day *. 30.)
let two_months_ago = now -. (one_day *. 60.)

let a_quarter = 15. *. 60.
let ten_min = 10. *. 60.
let five_min = 5. *. 60.
let two_min = 120.

module StrM = OpamStd.String.Map
module StrS = OpamStd.String.Set
module IntM = OpamStd.IntMap
module OPM = OpamPackage.Map
module FloM =
  OpamStd.Map.Make (struct
    type t = float
    let compare a b = int_of_float (a -. b)
    let to_string = string_of_float
    let to_json _ = `Null
  end)

(* Memory cache: IntMap, containing a day_mcache for each day (0..29, backward) *)
type day_mcache = {
  pkgs : float list StrM.t OPM.t;
  updates : int64 StrM.t;
  users : StrS.t;
}

type mcache = day_mcache IntM.t

let empty_day_mcache =
  {
    pkgs = OPM.empty;
    updates = StrM.empty;
    users = StrS.empty;
  }

let empty_mcache = IntM.empty

(* Increment the counter corresponding to a key in a (int64 StringMap) *)
let incr_strmap key map =
  let n =
    try StrM.find key map
    with Not_found -> Int64.zero in
  StrM.add key (Int64.succ n) map

(* Add download timestamp corresponding to a package and a host *)
let add_ts_pkgmap pkg host ts map =
  let m =
    try OPM.find pkg map
    with Not_found -> StrM.empty
  in
  let n =
    try StrM.find host m
    with Not_found -> []
  in
  OPM.add pkg (StrM.add host (ts::n) m) map

let which_day ?(log_filter = O2wGlobals.default_log_filter) ?(from) entry =
  let from = OpamStd.Option.default log_filter.log_end_time from in
  if log_filter.log_custom entry then
    let ago = from -. entry.log_timestamp in
    if ago > one_month then None
    else Some ago
  else None

let add_mcache_entry mcache entry =
  let now = O2wGlobals.default_log_filter.log_end_time in
  match which_day ~from:now entry with
  | Some ago ->
    let d = int_of_float (ago /. one_day) in
    let dmcache = try IntM.find d mcache with Not_found -> empty_day_mcache in
    let users = StrS.add entry.log_host dmcache.users in
    let dmcache = { dmcache with users } in
    let dmcache =
      match entry.log_request with
      | Update_req ->
        let updates = incr_strmap entry.log_host dmcache.updates in
        { dmcache with updates }
      | Archive_req pkg ->
        let pkgs =
          add_ts_pkgmap pkg entry.log_host entry.log_timestamp dmcache.pkgs
        in
        { dmcache with pkgs}
      | _ -> dmcache
    in
    IntM.add d dmcache mcache
  | None -> mcache

let compute_stats ?(unique=false) mcache repos =
  (* timestamps: Compute once a map of packages to keep, by user, detect leaf
      package given download timestamps: adjacent downloads < 5 min  *)
  let month_leaf_pkg_stats =
    let flat =
      IntM.fold
        (fun _ dmc map -> OPM.union (StrM.union List.append) dmc.pkgs map)
        mcache OPM.empty
    in
    (* construct timsetamps maps from [dmcache.pkgs]s *)
    let timestamps =
      OPM.fold (fun pkg strm map1 ->
          StrM.fold (fun str tsl map2 ->
              let map = try StrM.find str map2 with Not_found -> FloM.empty in
              let tsm = List.fold_left
                  (fun map3 ts -> FloM.update ts (fun l -> pkg::l) []  map3)
                  map tsl in
              StrM.add str tsm map2
            ) strm map1
        ) flat StrM.empty
    in
    let st = O2wUniverse.load_opam_state repos in
    let get_package_deps env p =
      let n = OpamPackage.name p in
      try env, OpamPackage.Name.Map.find n env
      with Not_found ->
        let deps =
          match OpamSwitchState.opam_opt st p with
          | Some op ->
            OpamFormula.fold_left (fun acc (n,_) -> n::acc) []
              (OpamFile.OPAM.depends op)
          | None -> []
        in (OpamPackage.Name.Map.add n deps env), deps
    in
    let get_dependencies_set adjacent env =
      let module OPS = OpamPackage.Name.Set in
      FloM.fold (fun _ p (env, deps) ->
          let n_env, ldeps =
            List.fold_left
              (fun (acc_env, acc_d) pi ->
                 let e,d = get_package_deps acc_env pi in
                 e,d::acc_d) (env,[]) p
          in
          n_env, OPS.union deps (OPS.of_list (List.flatten ldeps)))
        adjacent (env, OPS.empty)
    in
    (* split map into first adjacent set (first dead window of 2 min)
       and rest *)
    let get_adjacent map =
      let rec aux max lst =
        match lst with
        | [] -> max
        | (next, _)::rest ->
          if next -. max < five_min then
            aux next rest
          else max
      in
      let split_ts =
        match FloM.bindings map with
        | [] -> assert false (* already checked that map is not empty *)
        | (ts,_)::tl -> aux ts tl
      in
      match FloM.split split_ts map with
      | m1, Some pl, m2 -> (FloM.add split_ts pl m1), m2
      | m1, None, m2 -> assert false (* we are sure that the element exists*)
    in
    let _, pkg_to_keep =
      (* Create leaf packages map, by user *)
      StrM.fold (fun h tsm (env, tsm_acc) ->
          let rec aux tsm (env, n_tsm_acc) =
            if FloM.is_empty tsm then (env, n_tsm_acc)
            else
            (* Get adjacent download map, and filter by package
               dependencies: keep leaf only *)
            let adjacent, others = get_adjacent tsm in
            let n_env, all_deps = get_dependencies_set adjacent env in
            let n_tsm =
              FloM.union List.append n_tsm_acc @@
              FloM.fold (fun ts pl acc ->
                  let flt =
                    List.filter
                      (fun p ->
                         not (OpamPackage.Name.Set.mem
                                (OpamPackage.name p) all_deps)) pl
                  in
                  if flt <> [] then FloM.add ts flt acc else acc)
                adjacent FloM.empty
            in
            aux others (n_env,n_tsm)
          in
          let n_env, new_binding = aux tsm (env, FloM.empty) in
          n_env, StrM.add h new_binding tsm_acc
        ) timestamps (OpamPackage.Name.Map.empty, StrM.empty)
    in
    let flat_pkgs_map =
      OPM.mapi (fun pkg strm ->
          StrM.mapi (fun host tsl ->
              let pkg_user =
                try StrM.find host pkg_to_keep
                with Not_found -> assert false (* these maps have same content *)
              in
              let lst =
                List.filter (fun ts ->
                    try List.mem pkg (FloM.find ts pkg_user)
                    with Not_found -> false
                  ) tsl
              in
              Int64.of_int (List.length lst))
            strm)
        flat
    in
    let add _ n acc =
      if unique then
        if n <> 0L then Int64.succ acc else acc
      else Int64.add n acc
    in
    OPM.map (fun str -> StrM.fold add str Int64.zero) flat_pkgs_map
  in
  (* Compute stats given in interval set *)
  let compute_in interval =
    let users_stats =
      let flat_users_set =
        IntM.fold
          (fun _ dmc set -> StrS.union dmc.users set)
          interval StrS.empty
      in
      Int64.of_int (StrS.cardinal flat_users_set)
    in
    let update_stats =
      let flat_updates_map =
        IntM.fold
          (fun _ dmc map -> StrM.union Int64.add dmc.updates map)
          interval StrM.empty
      in
      if unique then
        Int64.of_int (StrM.cardinal flat_updates_map)
      else
        StrM.fold (fun _ n acc -> Int64.add n acc) flat_updates_map Int64.zero
    in
    let pkg_stats =
      let flat_pkgs_map =
        IntM.fold
          (fun _ dmc map -> OPM.union (StrM.union List.append) dmc.pkgs map)
          interval OPM.empty
      in
      let add _ n acc =
        if unique then
          if n <> [] then Int64.succ acc else acc
        else Int64.add (Int64.of_int (List.length n)) acc
      in
      OPM.map (fun str -> StrM.fold add str Int64.zero) flat_pkgs_map
    in
    let global_stats =
      OPM.fold (fun _ n acc -> Int64.add n acc) pkg_stats Int64.zero
    in
    { pkg_stats; global_stats; update_stats; users_stats }
  in
  (* safety check *)
  if IntM.is_empty mcache then empty_stats_set
  else
  let a_day = 0 in
  let a_week = 6 in
  let day_stats =
    try compute_in (IntM.singleton a_day (IntM.find a_day mcache))
    with Not_found -> empty_stats
  in
  let week_stats =
    let map =
      match IntM.split a_week mcache with
      | m , Some e, _ ->  IntM.add a_week e m
      | m, None, _ -> m
    in
    if IntM.is_empty map then empty_stats
    else compute_in map
  in
  let month_stats = compute_in mcache in
  (* do not compute altime stats *)
  let alltime_stats = empty_stats in
  { alltime_stats; day_stats; week_stats; month_stats; month_leaf_pkg_stats }

let add_mcache =
  IntM.union
    (fun dmc1 dmc2 ->
       let pkgs = OPM.union (StrM.union List.append) dmc1.pkgs dmc2.pkgs in
       let updates = StrM.union Int64.add dmc1.updates dmc2.updates in
       let users = StrS.union dmc1.users dmc2.users in
       { pkgs; updates; users })

(* Cache management *)
type cache_elt = {
  cache_size: int;
  cache_hash: Digest.t; (* first 10k only *)
  cache_month_map: mcache;
  cache_only_since: float;
}
type cache = cache_elt OpamFilename.Map.t
let cache_file = OpamFilename.of_string "~/.cache/opam2web2/stats_cache"
let cache_format_version = 2
let version_id =
  Digest.string (OpamVersion.(to_string (full ())) ^" "^
                 string_of_int cache_format_version)
let write_cache (cache: cache) =
  OpamFilename.mkdir (OpamFilename.dirname cache_file);
  let oc = open_out_bin (OpamFilename.to_string cache_file) in
  Digest.output oc version_id;
  Marshal.to_channel oc cache [];
  close_out oc
let read_cache () : cache =
  try
    let ic = open_in_bin (OpamFilename.to_string cache_file) in
    let cache =
      if Digest.input ic = version_id then
        (Printf.printf "Reading logs cache from %s... %!"
           (OpamFilename.to_string cache_file);
         let c = Marshal.from_channel ic in
         Printf.printf "done.\n%!";
         c)
      else
        (Printf.printf "Skipping mismatching logs cache %s\n%!"
           (OpamFilename.to_string cache_file);
         OpamFilename.Map.empty)
    in
    close_in ic;
    cache
  with _ -> OpamFilename.Map.empty
let partial_digest ic len =
  seek_in ic 0;
  let len = max 10_000 len in
  try Digest.channel ic len with End_of_file ->
    Digest.channel ic (-1)

let statistics_set files repos =
  let cache = read_cache () in
  let skip_before = two_months_ago in
  let cache =
    OpamFilename.Map.filter (fun f _ -> List.mem f files) cache
  in
  match files with
  | [] -> None
  | files ->
    let mc_and_logs =
      List.rev_map (fun f ->
          let ic = OpamFilename.open_in f in
          let mcache, offset =
            try
              let partial = OpamFilename.Map.find f cache in
              let len = in_channel_length ic in
              if
                len >= partial.cache_size &&
                partial_digest ic partial.cache_size = partial.cache_hash &&
                partial.cache_only_since <= skip_before
              then
                (Printf.printf "%s: cache found (%d out of %d KB new)\n%!"
                   (OpamFilename.to_string f)
                   ((len - partial.cache_size)/1024) (len/1024);
                 partial.cache_month_map, partial.cache_size)
              else
                (Printf.printf "%s: dropping invalid cache\n%!"
                   (OpamFilename.to_string f);
                 empty_mcache, 0)
            with Not_found ->
              Printf.printf "%s: no cache found\n"
                (OpamFilename.to_string f);
              empty_mcache, 0
          in
          close_in ic;
          let reader =
            Readcombinedlog.create
              (fun e -> timestamp_of_entry e > skip_before)
              (OpamFilename.to_string f)
          in
          seek_in reader.Readcombinedlog.ic offset;
          let reader =
            { reader with
              Readcombinedlog.size = reader.Readcombinedlog.size - offset }
          in
          f, (mcache, reader)
      ) files in
    let chunk_size = 10_000 in
    let rec read_and_mcache mcache l =
      let percent = if l.size = 0 then 100 else 100 * l.reads / l.size in
      Printf.printf "\rReading new entries from %s: %3d%%%!" l.name percent;
      let mcache =
        List.fold_left
          (fun mcache line -> add_mcache_entry mcache (mk_entry line))
          mcache (Readcombinedlog.read l chunk_size)
      in
      if Readcombinedlog.is_empty l then
        (Printf.printf "\rReading new entries from %s: %3d%%\n%!" l.name 100;
         mcache)
      else
        read_and_mcache mcache l
    in
    let cache,mcache =
      List.fold_left (fun (cache, sbglob) (file,(mcache, log)) ->
          let mcache = read_and_mcache mcache log in
          let ic = open_in (OpamFilename.to_string file) in
          let cache_size = in_channel_length ic in
          let cache_hash = partial_digest ic cache_size in
          close_in ic;
          let cache =
            OpamFilename.Map.add file
              { cache_size; cache_hash;
                cache_only_since = skip_before;
                cache_month_map = mcache }
              cache
          in
          cache, add_mcache mcache sbglob)
        (cache, empty_mcache)
        mc_and_logs
    in
    write_cache cache;
    let stats = compute_stats ~unique:O2wGlobals.default_log_filter.log_per_ip mcache repos in
    Some stats

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

let to_csv popularity file =
  let oc = open_out file in
  Printf.fprintf oc "Name, Version, Downloads\n";
  OpamPackage.Map.iter (fun pkg count ->
    Printf.fprintf oc "%S, %S, %Ld\n"
      (OpamPackage.Name.to_string (OpamPackage.name pkg))
      (OpamPackage.Version.to_string (OpamPackage.version pkg))
      count
  ) popularity;
  close_out oc

let to_json popularity file =
  let oc = open_out file in
  Printf.fprintf oc "[";
  let first = ref true in
  OpamPackage.Map.iter (fun pkg count ->
    if !first then
      first := false
    else
      Printf.fprintf oc ",";
    Printf.fprintf oc "{\n  \"name\": %S,\n  \"version\": %S,  \n  \"downloads\": %Ld\n}"
      (OpamPackage.Name.to_string (OpamPackage.name pkg))
      (OpamPackage.Version.to_string (OpamPackage.version pkg))
      count
  ) popularity;
  Printf.fprintf oc "\n]";
  close_out oc
