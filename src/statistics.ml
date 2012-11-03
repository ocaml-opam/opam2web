open Unix

open Logentry
open O2w_common

module StringMap = Map.Make (String)

(* Retrieve log entries of an apache access.log file *)
let entries_of_logfile (init: log_entry list) (filename: string)
    : log_entry list =
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
      if Re_str.string_match timestamp_regexp e.date 0 then
        fst (Unix.mktime
          {
            tm_mday = int_of_string (Re_str.matched_group 1 e.date);
            tm_mon = month_of_string (Re_str.matched_group 2 e.date);
            tm_year = int_of_string (Re_str.matched_group 3 e.date) - 1900;
            tm_hour = int_of_string (Re_str.matched_group 4 e.date);
            tm_min = int_of_string (Re_str.matched_group 5 e.date);
            tm_sec = int_of_string (Re_str.matched_group 6 e.date);
            (* Initial dummy values *)
            tm_wday = 0;
            tm_yday = 0;
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
      let match_browser = try
          Re_str.search_forward browser_regexp e.client 0
        with
          Not_found -> (-1)
      in
      let browser =
        if match_browser >= 0 then
          let browser_str =
            try
              Re_str.matched_group 1 e.client
            with
              Not_found -> ""
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
      let match_os = try
          Re_str.search_forward os_regexp e.client 0
        with
          Not_found -> (-1)
      in
      let os =
        if match_os >= 0 then
          let os_str =
            try
              Re_str.matched_group 1 e.client
            with
              Not_found -> ""
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
  if OpamFilename.exists file then
    begin
      Printf.printf "Parsing web server log file '%s'...\n" filename;
      parse_log ()
    end
  else
    begin
      Printf.printf "No web server log file found.\n";
      init
    end

let entries_of_logfiles (logfiles: string list): log_entry list =
  List.fold_left entries_of_logfile [] logfiles

(* Sum the values of a (int64 StringMap), possibly reducing the value to one
   unique count per string key if the 'unique' optional argument is true *)
let sum_strmap ?(unique = false) (map: int64 StringMap.t): int64 =
  if unique then
    StringMap.fold (fun _ _ acc -> Int64.succ acc) map Int64.zero
  else
    StringMap.fold (fun _ n acc -> Int64.add n acc) map Int64.zero

(* Increment the counter corresponding to a key in a (int64 StringMap) *)
let incr_strmap (key: string) (map: int64 StringMap.t)
    : int64 StringMap.t =
  let n =
    try
      StringMap.find key map
    with
      Not_found -> Int64.zero
  in
  StringMap.add key (Int64.succ n) (StringMap.remove key map)

let incr_hostmap (pkg: OpamPackage.t) (host: string) (map: int64 StringMap.t OpamPackage.Map.t) =
  let m =
    try OpamPackage.Map.find pkg map
    with Not_found -> StringMap.empty in
  OpamPackage.Map.add pkg (incr_strmap host m) (OpamPackage.Map.remove pkg map)

let apply_log_filter log_filter entries =
  List.filter (fun e ->
      if e.log_timestamp >= log_filter.log_start_time
          && e.log_timestamp <= log_filter.log_end_time then
        true
      else false)
    entries

(* Count the number of update requests in a list of entries *)
let count_updates ?(log_filter = default_log_filter) (entries: log_entry list): int64 =
  let count_map =
    List.fold_left (fun acc e -> match e.log_request with
        | Update_req -> incr_strmap e.log_host acc
        | _ -> acc)
      StringMap.empty entries
  in
  sum_strmap ~unique:log_filter.log_per_ip count_map

(* Count the number of downloads for each OPAM archive *)
let count_archive_downloads ?(log_filter = default_log_filter) (entries: log_entry list)
    : (OpamPackage.t * int64) list =
  let rec aux stats = function
    | []       -> stats
    | hd :: tl -> match hd.log_request with
      | Archive_req pkg -> aux (incr_hostmap pkg hd.log_host stats) tl
      |_                -> aux stats tl
  in
  OpamPackage.Map.fold (fun pkg host_map acc ->
    (pkg, sum_strmap ~unique:log_filter.log_per_ip host_map) :: acc
  ) (aux OpamPackage.Map.empty entries) []

(* Count hosts with at least 10 requests within the last week *)
let count_users (now: float) (entries: log_entry list): int =
  let one_day = 3600. *. 24. in
  let one_week_ago = now -. (one_day *. 7.) in
  let filter_week = { default_log_filter with
      log_start_time = one_week_ago;
      log_end_time = now;
      filter_name = "week";
    }
  in
  Printf.printf "Counting users (%s)\n%!" filter_week.filter_name;
  let filtered_entries = apply_log_filter filter_week entries in

  let users =
    List.fold_left
      (fun map elt -> incr_strmap elt.log_host map)
      StringMap.empty filtered_entries in

  StringMap.fold (fun _ i n -> if i > 10L then n+1 else n) users 0

(* Generate basic statistics on log entries *)
let basic_stats_of_entries log_filter (entries: log_entry list): statistics =
  (* TODO: factorize filtering of entries in count_updates and count_archive
     downloads *)
  Printf.printf "Basic statistics (%s)\n%!" log_filter.filter_name;
  let filtered_entries = apply_log_filter log_filter entries in
  let pkg_stats = count_archive_downloads ~log_filter entries in
  let global_stats =
    List.fold_left (fun acc (_, n) -> Int64.add n acc)
        Int64.zero pkg_stats
  in
  let update_stats = count_updates ~log_filter entries in
  {
    pkg_stats;
    global_stats;
    update_stats;
  }

(* Read log entries from log files and generate basic statistics *)
let basic_stats_of_logfiles log_filter (logfiles: string list): statistics option =
  let entries = entries_of_logfiles logfiles in
  match entries with
  | [] -> None
  | some_entries -> Some (basic_stats_of_entries log_filter some_entries)

let basic_statistics_set (logfiles: string list): (statistics_set * int) option =
  let entries = entries_of_logfiles logfiles in
  match entries with
  | [] -> None
  | some_entries ->
      let now = default_log_filter.log_end_time in
      let one_day = 3600. *. 24. in
      let one_day_ago = now -. one_day in
      let one_week_ago = now -. (one_day *. 7.) in
      let one_month_ago = now -. (one_day *. 30.) in
      let one_year_ago = now -. (one_day *. 365.) in

      let alltime_stats = basic_stats_of_entries
          { default_log_filter with filter_name = "alltime" }
          some_entries
      in
      let day_stats = basic_stats_of_entries
          { default_log_filter with log_start_time = one_day_ago; filter_name = "day" }
          some_entries
      in
      let week_stats = basic_stats_of_entries
          { default_log_filter with log_start_time = one_week_ago; filter_name = "week" }
          some_entries
      in
      let month_stats = basic_stats_of_entries
          { default_log_filter with log_start_time = one_month_ago; filter_name = "month" }
          some_entries
      in
      let year_stats = basic_stats_of_entries
          { default_log_filter with log_start_time = one_year_ago; filter_name = "year" }
          some_entries
      in

      let nusers = count_users now some_entries in
      Some ({
        day_stats = day_stats;
        week_stats = week_stats;
        month_stats = month_stats;
        year_stats = year_stats;
        alltime_stats = alltime_stats;
      },
      nusers)

(* Retrieve the 'ntop' number of packages with the higher (or lower) int value
   associated *)
let top_packages ?ntop ?(reverse = true) pkg_stats =
  let compare_pkg (_, n1) (_, n2) =
    if reverse then Int64.compare n2 n1
    else Int64.compare n1 n2
  in
  let sorted_pkg = List.sort compare_pkg pkg_stats in
  match ntop with
  | None -> sorted_pkg
  | Some nmax -> first_n nmax sorted_pkg

(* Retrieve the 'ntop' number of maintainers with the higher (or lower) number
   of associated packages *)
let top_maintainers ?ntop ?(reverse = true) repository
    : (string * int) list =
  let packages = Repository.get_packages repository in
  let all_maintainers = List.map (fun pkg ->
      let opam_file =
        OpamFile.OPAM.read (OpamPath.Repository.opam repository pkg)
      in
      OpamFile.OPAM.maintainer opam_file)
    packages
  in
  let gathered_maintainers = List.sort String.compare all_maintainers in
  let prepend_m acc (m, ct) =
    if ct = 0 then acc else (m, ct) :: acc
  in
  let compare_maintainers (_, n1) (_, n2) =
    if reverse then compare n2 n1
    else compare n1 n2
  in
  let maintainer_stats =
    let rec count_aux acc (m, ct) = function
      | [] -> prepend_m acc (m, ct)
      | hd :: tl when hd = m -> count_aux acc (hd, ct + 1) tl
      | hd :: tl -> count_aux (prepend_m acc (m, ct)) (hd, 1) tl
    in List.rev (count_aux [] ("", 0) gathered_maintainers)
  in
  let sorted_maintainers =
    List.sort compare_maintainers maintainer_stats
  in
  match ntop with
  | None -> sorted_maintainers
  | Some nmax -> first_n nmax sorted_maintainers

