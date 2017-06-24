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
  alltime_stats = empty_stats;
  day_stats     = empty_stats;
  week_stats    = empty_stats;
  month_stats   = empty_stats;
}

module StringMap = struct
  include Map.Make (String)
  let fold f t acc =
    let acc = ref acc in
    iter (fun k v ->
      acc := f k v !acc
    ) t;
    !acc
end

let timestamp_regexp =
  Re_str.regexp "\\([0-9]+\\)/\\([A-Z][a-z]+\\)/\\([0-9]+\\):\\([0-9]+\\):\
                 \\([0-9]+\\):\\([0-9]+\\) [-+][0-9]+"

let timestamp_of_entry e =
  let open Unix in
  let open Logentry in
  if Re_str.string_match timestamp_regexp e.date 0 then
    fst (Unix.mktime {
        tm_mday = int_of_string (Re_str.matched_group 1 e.date);
        tm_mon  = O2wMisc.month_of_string (Re_str.matched_group 2 e.date);
        tm_year = int_of_string (Re_str.matched_group 3 e.date) - 1900;
        tm_hour = int_of_string (Re_str.matched_group 4 e.date);
        tm_min  = int_of_string (Re_str.matched_group 5 e.date);
        tm_sec  = int_of_string (Re_str.matched_group 6 e.date);
        (* Initial dummy values *)
        tm_wday  = 0;
        tm_yday  = 0;
        tm_isdst = false;
      })
  else 0.

let request_of_entry e =
  let html_regexp =
    Re_str.regexp "GET /\\(.+\\)\\.html HTTP/[.0-9]+" in
  let archive_regexp =
    Re_str.regexp "GET /archives/\\(.+\\)\\+opam\\.tar\\.gz HTTP/[.0-9]+" in
  let update_regexp =
    Re_str.regexp "GET /urls\\.txt HTTP/[.0-9]+" in
  let open Logentry in
  let package_of_string str =
    try OpamPackage.of_string (Filename.basename str)
    with OpamStd.Sys.Exit e ->
      failwith ("opam exit with code " ^ string_of_int e)
  in
  try
    if Re_str.string_match html_regexp e.request 0 then
      Html_req (Re_str.matched_group 1 e.request)
    else if Re_str.string_match archive_regexp e.request 0 then
      Archive_req (package_of_string (Re_str.matched_group 1 e.request))
    else if Re_str.string_match update_regexp e.request 0 then
      Update_req
    else
      Unknown_req e.request
  with Failure _ -> Unknown_req e.request

let internal_regexp =
  Re_str.regexp "https?://opam\\.ocaml\\(\\.org\\|pro\\.com\\)/\\(.*\\)/?"

let referrer_of_entry e =
  let open Logentry in
  match e.referrer with
  | "-" -> No_ref
  | s when Re_str.string_match internal_regexp e.referrer 0 ->
    Internal_ref (Re_str.matched_group 2 e.referrer)
  | s -> External_ref s

let browser_regexp =
  Re_str.regexp "\\(MSIE\\|Chrome\\|Firefox\\|Safari\\)"
let os_regexp =
  Re_str.regexp "\\(Windows\\|Macintosh\\|iPad\\|iPhone\\|\
                 Android\\|Linux\\|FreeBSD\\)"

let client_of_entry e =
  let open Logentry in
  (* TODO: refine client string parsing (versions, more browsers...)  *)
  let match_browser =
    try Re_str.search_forward browser_regexp e.client 0
    with Not_found -> (-1) in
  let browser =
    if match_browser >= 0 then
      let browser_str =
        try Re_str.matched_group 1 e.client
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
    try Re_str.search_forward os_regexp e.client 0
    with Not_found -> (-1) in
  let os =
    if match_os >= 0 then
      let os_str =
        try Re_str.matched_group 1 e.client
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

(* Sum the values of a (int64 StringMap), possibly reducing the value to one
   unique count per string key if the 'unique' optional argument is true *)
let sum_strmap ?(unique = false) map =
  if unique then
    Int64.of_int (StringMap.cardinal map)
  else
    StringMap.fold (fun _ n acc -> Int64.add n acc) map Int64.zero

(* Increment the counter corresponding to a key in a (int64 StringMap) *)
let incr_strmap key map =
  let n =
    try StringMap.find key map
    with Not_found -> Int64.zero in
  StringMap.add key (Int64.succ n) map

let incr_hostmap pkg host map =
  let m =
    try OpamPackage.Map.find pkg map
    with Not_found -> StringMap.empty in
  OpamPackage.Map.add pkg (incr_strmap host m) map

let apply_log_filter log_filter e =
  e.log_timestamp >= log_filter.log_start_time
  && e.log_timestamp <= log_filter.log_end_time
  && log_filter.log_custom e

let apply_log_filters log_filter es =
  List.filter (apply_log_filter log_filter) es

(* Count the number of update requests in a list of entries *)
let count_updates ?(log_filter = O2wGlobals.default_log_filter) entries =
  let count_map =
    List.fold_left (fun acc e -> match e.log_request with
        | Update_req -> incr_strmap e.log_host acc
        | _ -> acc)
      StringMap.empty entries
  in
  sum_strmap ~unique:log_filter.log_per_ip count_map

(* Count the number of downloads for each OPAM archive *)
let count_archive_downloads log_filter entries =
  OpamPackage.Map.map
    (sum_strmap ~unique:log_filter.log_per_ip)
    (List.fold_left (fun stats entry ->
         match entry.log_request with
         | Archive_req pkg -> incr_hostmap pkg entry.log_host stats
         | _ -> stats)
        OpamPackage.Map.empty entries)

(* Count hosts with at least 10 requests within the last week *)
let count_users entries =
  let users =
    List.fold_left
      (fun map elt -> incr_strmap elt.log_host map)
      StringMap.empty entries
  in
  Int64.of_int (StringMap.cardinal users)

(* Generate basic statistics on log entries *)
let stats_of_entries log_filter entries =
  let entries = apply_log_filters log_filter entries in
  let pkg_stats = count_archive_downloads log_filter entries in
  let global_stats =
    OpamPackage.Map.fold (fun _ n acc -> Int64.add n acc) pkg_stats Int64.zero in
  let update_stats = count_updates ~log_filter entries in
  let users_stats = count_users entries in
  {
    pkg_stats;
    global_stats;
    update_stats;
    users_stats;
  }

module FloatMap = Map.Make(struct type t = float let compare = compare end)

type entry_map = log_entry list FloatMap.t (** indexed by timestamp *)
let add_entry e m =
  let existing = try FloatMap.find e.log_timestamp m with Not_found -> [] in
  FloatMap.add e.log_timestamp (e::existing) m
let entries_after ts m = match FloatMap.split ts m with
  | _, Some e, m -> FloatMap.add ts e m
  | _, None, m -> m
let entries_list m =
  FloatMap.fold (fun _ -> List.rev_append) m []

let now = O2wGlobals.default_log_filter.log_end_time
let one_day = 3600. *. 24.
let one_day_ago = now -. one_day
let one_week_ago = now -. (one_day *. 7.)
let one_month_ago = now -. (one_day *. 30.)
let two_months_ago = now -. (one_day *. 60.)

let stats_set_of_entries entries =
  let month_entries = entries_after one_month_ago entries in
  let week_entries =  entries_after one_week_ago month_entries in
  let day_entries = entries_after one_day_ago week_entries in
  let filter filter_name log_start_time entries =
    stats_of_entries
      { O2wGlobals.default_log_filter with log_start_time; filter_name }
      (entries_list entries)
  in
  {
    day_stats     = filter "day" one_day_ago day_entries;
    week_stats    = filter "week" one_week_ago week_entries;
    month_stats   = filter "month" one_month_ago month_entries;
    alltime_stats = empty_stats;
  }

let add_stats s1 s2 = {
  pkg_stats    = OpamPackage.Map.union Int64.add s1.pkg_stats s2.pkg_stats;
  global_stats = Int64.add s1.global_stats s2.global_stats;
  update_stats = Int64.add s1.update_stats s2.update_stats;
  users_stats  = Int64.add s1.users_stats s2.users_stats;
}

let add_stats_set s1 s2 = {
  alltime_stats = empty_stats (* add_stats s1.alltime_stats s2.alltime_stats *);
  day_stats     = add_stats s1.day_stats s2.day_stats;
  week_stats    = add_stats s1.week_stats s2.week_stats;
  month_stats   = add_stats s1.month_stats s2.month_stats;
}

type cache_elt = {
  cache_size: int;
  cache_hash: Digest.t; (* first 10k only *)
  cache_entries: entry_map;
  cache_only_since: float;
}
type cache = cache_elt OpamFilename.Map.t
let cache_file = OpamFilename.of_string "~/.cache/opam2web/stats_cache"
let cache_format_version = 1
let version_id =
  Digest.string (OpamVersion.(to_string (full ())) ^" "^
                 string_of_int cache_format_version)
let write_cache (cache: cache) =
  OpamFilename.mkdir (OpamFilename.dirname cache_file);
  let oc = open_out_bin (OpamFilename.to_string cache_file) in
  Digest.output oc version_id;
  Marshal.to_channel oc cache [Marshal.No_sharing];
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

let statistics_set files =
  let cache = read_cache () in
  let skip_before = two_months_ago in
  match files with
  | [] -> None
  | files ->
    let cache_and_logs =
      List.rev_map (fun f ->
          let ic = OpamFilename.open_in f in
          let cached, offset =
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
                partial.cache_entries, partial.cache_size)
              else
                (Printf.printf "%s: dropping invalid cache\n%!"
                   (OpamFilename.to_string f);
                 FloatMap.empty, 0)
            with Not_found ->
              Printf.printf "%s: no cache found\n"
                (OpamFilename.to_string f);
              FloatMap.empty, 0
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
          f, (cached, reader)
      ) files in
    let chunk_size = 10_000 in
    let rec read entries l =
      let percent = if l.size = 0 then 100 else 100 * l.reads / l.size in
      Printf.printf "\rReading new entries from %s: %3d%%%!" l.name percent;
      let entries =
        List.fold_left (fun entries line -> add_entry (mk_entry line) entries)
          entries (Readcombinedlog.read l chunk_size)
      in
      if Readcombinedlog.is_empty l then
        (Printf.printf "\rReading new entries from %s: %3d%%\n%!" l.name 100;
         entries)
      else
        read entries l
    in
    let cache, stats =
      List.fold_left (fun (cache, stats) (file,(cached_entries,log)) ->
          let entries = read cached_entries log in
          let ic = open_in (OpamFilename.to_string file) in
          let cache_size = in_channel_length ic in
          let cache_hash = partial_digest ic cache_size in
          close_in ic;
          let cache =
            OpamFilename.Map.add file
              { cache_size; cache_hash;
                cache_only_since = skip_before;
                cache_entries = entries_after skip_before entries }
              cache
          in
          let stats = add_stats_set stats (stats_set_of_entries entries) in
          cache, stats)
        (cache, empty_stats_set)
        cache_and_logs
    in
    write_cache cache;
    Some stats

let aggregate_package_popularity pkg_stats pkg_idx =
  OpamPackage.Map.fold (fun pkg pkg_count acc ->
    if not (OpamPackage.Map.mem pkg pkg_idx) then
      (* This can happen when some packages are deleted *)
      acc
    else (
      let pkg_name = OpamPackage.name pkg in
      if not (OpamPackage.Name.Map.mem pkg_name acc) then
        OpamPackage.Name.Map.add pkg_name pkg_count acc
      else (
        let last_count = OpamPackage.Name.Map.find pkg_name acc in
        OpamPackage.Name.Map.add pkg_name
          (Int64.add pkg_count last_count)
          (OpamPackage.Name.Map.remove pkg_name acc)
      )
    )
  ) pkg_stats OpamPackage.Name.Map.empty

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
