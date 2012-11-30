(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

open O2wTypes

module StringMap = Map.Make (String)

(* Retrieve log entries of an apache access.log file *)
let entries_of_logfile init filename =
  Printf.printf "Parsing %s\n%!" filename;
  let file = OpamFilename.of_string filename in
  let html_regexp =
    Re_str.regexp "GET /\\(.+\\)\\.html HTTP/[.0-9]+"
  in
  let archive_regexp =
    Re_str.regexp "GET /archives/\\(.+\\)\\+opam\\.tar\\.gz HTTP/[.0-9]+"
  in
  let update_regexp =
    Re_str.regexp "GET /urls\\.txt HTTP/[.0-9]+"
  in
  let timestamp_regexp =
    Re_str.regexp "\\([0-9]+\\)/\\([A-Z][a-z]+\\)/\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) [-+][0-9]+"
  in
  let internal_regexp =
    Re_str.regexp "http://opam.ocamlpro.com/\\(.*\\)/?"
  in
  let browser_regexp =
    Re_str.regexp "\\(MSIE\\|Chrome\\|Firefox\\|Safari\\)"
  in
  let os_regexp =
    Re_str.regexp "\\(Windows\\|Macintosh\\|iPad\\|iPhone\\|Android\\|Linux\\|FreeBSD\\)"
  in

  let mk_entry e =
    let open Logentry in
    let request =
      if Re_str.string_match html_regexp e.request 0 then
        Html_req (Re_str.matched_group 1 e.request)
      else if Re_str.string_match archive_regexp e.request 0 then
        Archive_req (OpamPackage.of_string (Re_str.matched_group 1 e.request))
      else if Re_str.string_match update_regexp e.request 0 then
        Update_req
      else
        Unknown_req e.request
    in

    let timestamp =
      let open Unix in
      if Re_str.string_match timestamp_regexp e.date 0 then
        fst (Unix.mktime
               {
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
    in

    let referrer = match e.referrer with
      | "-" -> No_ref
      | s when Re_str.string_match internal_regexp e.referrer 0 ->
        Internal_ref (Re_str.matched_group 1 e.referrer)
      | s -> External_ref s
    in

    let client =
      (* TODO: refine client string parsing (versions, more browsers...)  *)
      let match_browser =
        try Re_str.search_forward browser_regexp e.client 0
        with Not_found -> (-1)
      in
      let browser =
        if match_browser >= 0 then
          let browser_str =
            try Re_str.matched_group 1 e.client
            with Not_found -> ""
          in
          match browser_str with
          | "Chrome" -> Chrome ""
          | "Firefox" -> Firefox ""
          | "MSIE" -> Internet_explorer ""
          | "Safari" -> Safari ""
          | s -> Unknown_browser s
        else
          Unknown_browser ""
      in
      let match_os =
        try Re_str.search_forward os_regexp e.client 0
        with Not_found -> (-1)
      in
      let os =
        if match_os >= 0 then
          let os_str =
            try Re_str.matched_group 1 e.client
            with Not_found -> ""
          in
          match os_str with
          | "Windows" -> Windows ""
          | "Macintosh" | "iPad" | "iPhone" -> Mac_osx ""
          | "Linux" | "FreeBSD" -> Unix ""
          | s -> Unknown_os s
        else
          Unknown_os ""
      in
      os, browser
    in

    {
      log_timestamp = timestamp;
      log_host = e.host;
      log_request = request;
      log_referrer = referrer;
      log_client = client;
    }
  in

  let parse_log () =
    let log_entries: Logentry.entry_t list =
      Readcombinedlog.readlog filename (fun _ -> true)
    in
    List.fold_left (fun acc e -> mk_entry e :: acc) init log_entries
  in
  if OpamFilename.exists file then (
    Printf.printf "Parsing web server log file '%s'...\n" filename;
    parse_log ()
  ) else (
    Printf.printf "No web server log file found.\n";
    init
  )

let entries_of_logfiles logfiles =
  List.fold_left entries_of_logfile [] logfiles

(* Sum the values of a (int64 StringMap), possibly reducing the value to one
   unique count per string key if the 'unique' optional argument is true *)
let sum_strmap ?(unique = false) map =
  if unique then
    StringMap.fold (fun _ _ acc -> Int64.succ acc) map Int64.zero
  else
    StringMap.fold (fun _ n acc -> Int64.add n acc) map Int64.zero

(* Increment the counter corresponding to a key in a (int64 StringMap) *)
let incr_strmap key map =
  let n =
    try StringMap.find key map
    with Not_found -> Int64.zero in
  StringMap.add key (Int64.succ n) (StringMap.remove key map)

let incr_hostmap pkg host map =
  let m =
    try OpamPackage.Map.find pkg map
    with Not_found -> StringMap.empty in
  OpamPackage.Map.add pkg (incr_strmap host m) (OpamPackage.Map.remove pkg map)

let apply_log_filter log_filter entries =
  List.filter (fun e ->
      if e.log_timestamp >= log_filter.log_start_time
          && e.log_timestamp <= log_filter.log_end_time
          && log_filter.log_custom e then
        true
      else false)
    entries

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
let count_archive_downloads ?(log_filter = O2wGlobals.default_log_filter) entries =
  let rec aux stats = function
    | []       -> stats
    | hd :: tl ->
      match hd.log_request with
      | Archive_req pkg -> aux (incr_hostmap pkg hd.log_host stats) tl
      |_                -> aux stats tl
  in
  OpamPackage.Map.map
    (sum_strmap ~unique:log_filter.log_per_ip)
    (aux OpamPackage.Map.empty entries)

(* Count hosts with at least 10 requests within the last week *)
let count_users entries =
  let users =
    List.fold_left
      (fun map elt -> incr_strmap elt.log_host map)
      StringMap.empty entries
  in
  StringMap.fold (fun _ _ n -> Int64.succ n) users 0L

(* Generate basic statistics on log entries *)
let basic_stats_of_entries log_filter entries =
  (* TODO: factorize filtering of entries in count_updates and count_archive
     downloads *)
  Printf.printf "Basic statistics (%s): %!" log_filter.filter_name;
  let entries = apply_log_filter log_filter entries in
  Printf.printf "%d entries\n%!" (List.length entries);
  let pkg_stats = count_archive_downloads ~log_filter entries in
  let global_stats =
    OpamPackage.Map.fold
      (fun _ n acc -> Int64.add n acc)
      pkg_stats Int64.zero
  in
  let update_stats = count_updates ~log_filter entries in
  let users_stats = count_users entries in
  {
    pkg_stats;
    global_stats;
    update_stats;
    users_stats;
  }

(* Read log entries from log files and generate basic statistics *)
let basic_stats_of_logfiles log_filter logfiles =
  let entries = entries_of_logfiles logfiles in
  match entries with
  | []           -> None
  | some_entries -> Some (basic_stats_of_entries log_filter some_entries)

let basic_statistics_set logfiles =
  let entries = entries_of_logfiles logfiles in
  match entries with
  | []           -> None
  | some_entries ->
    let now = O2wGlobals.default_log_filter.log_end_time in
    let one_day = 3600. *. 24. in
    let one_day_ago = now -. one_day in
    let one_week_ago = now -. (one_day *. 7.) in
    let one_month_ago = now -. (one_day *. 30.) in

    let alltime_stats = basic_stats_of_entries
      { O2wGlobals.default_log_filter with filter_name = "alltime" }
      some_entries
    in
    let day_stats = basic_stats_of_entries
      { O2wGlobals.default_log_filter with log_start_time = one_day_ago; filter_name = "day" }
      some_entries
    in
    let week_stats = basic_stats_of_entries
      { O2wGlobals.default_log_filter with log_start_time = one_week_ago; filter_name = "week" }
      some_entries
    in
    let month_stats = basic_stats_of_entries
      { O2wGlobals.default_log_filter with log_start_time = one_month_ago; filter_name = "month" }
      some_entries
    in

    Some {
      day_stats = day_stats;
      week_stats = week_stats;
      month_stats = month_stats;
      alltime_stats = alltime_stats;
    }

let aggregate_package_popularity pkg_stats packages =
  OpamPackage.Map.fold (fun pkg pkg_count acc ->
    if not (OpamPackage.Set.mem pkg packages) then
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
  let pkg_stats = List.map (fun pkg -> pkg, stats pkg) pkgs in
  let sorted_pkg = List.sort compare_pkg pkg_stats in
  match ntop with
  | None      -> sorted_pkg
  | Some nmax -> O2wMisc.first_n nmax sorted_pkg

(* Retrieve the 'ntop' number of maintainers with the higher (or lower) number
   of associated packages *)
let top_maintainers ?ntop ?(reverse = true) repository =
  let packages = OpamPackage.to_map (OpamRepository.packages repository) in
  let all_maintainers =
    OpamPackage.Name.Map.fold (fun name versions acc ->
      OpamPackage.Version.Set.fold
        (fun version acc ->
          let pkg = OpamPackage.create name version in
          let opam_file = OpamFile.OPAM.read (OpamPath.Repository.opam repository pkg) in
          let maintainer = OpamFile.OPAM.maintainer opam_file in
          if OpamMisc.StringMap.mem maintainer acc then
            let count = OpamMisc.StringMap.find maintainer acc in
            OpamMisc.StringMap.add maintainer (count+1) (OpamMisc.StringMap.remove maintainer acc)
          else
            OpamMisc.StringMap.add maintainer 1 acc)
        versions acc
    ) packages OpamMisc.StringMap.empty in
  let compare_maintainers (_,n1) (_,n2) = n1 - n2 in
  let sorted_maintainers =
    List.sort compare_maintainers (OpamMisc.StringMap.bindings all_maintainers)
  in
  match ntop with
  | None      -> sorted_maintainers
  | Some nmax -> O2wMisc.first_n nmax sorted_maintainers

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

