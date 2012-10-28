open Unix

open Logentry
open O2w_common

module StringMap = Map.Make (String)

let month_of_string: string -> int = function
  | "Jan" -> 0
  | "Feb" -> 1
  | "Mar" -> 2
  | "Apr" -> 3
  | "May" -> 4
  | "Jun" -> 5
  | "Jul" -> 6
  | "Aug" -> 7
  | "Sep" -> 8
  | "Oct" -> 9
  | "Nov" -> 10
  | "Dec" -> 11
  | unknown -> failwith ("Unknown month: " ^ unknown)

(* Retrieve log entries of an apache access.log file *)
let entries_of_logfile (init: log_entry list) (filename: string)
    : log_entry list =
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
            tm_year = int_of_string (Re_str.matched_group 3 e.date);
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
  StringMap.add key (Int64.succ n) map

(* Initialize a new (int64 StringMap) with one element *)
let init_strmap (key: string): int64 StringMap.t =
  incr_strmap key StringMap.empty

(* Count the number of update requests in a list of entries *)
let count_updates ?(per_ip = false) (entries: log_entry list): int64 =
  let count_map =
    List.fold_left (fun acc e -> match e.log_request with
        | Update_req -> incr_strmap e.log_host acc
        | _ -> acc)
      StringMap.empty entries
  in
  sum_strmap ~unique:per_ip count_map

(* Count the number of downloads for each OPAM archive *)
let count_archive_downloads ?(per_ip = true) ?(eq_pkg = (=)) (entries: log_entry list)
    : (OpamPackage.t * int64) list =
  let compare_entries e1 e2 = match e1.log_request, e2.log_request with
    | Archive_req p1, Archive_req p2 ->
        String.compare (OpamPackage.to_string p1) (OpamPackage.to_string p2)
    | Archive_req _, _ -> 1
    | _, Archive_req _ -> (-1)
    | _ -> 0
  in
  let sorted_entries = List.sort compare_entries entries in
  let rec aux stats (prev_pkg, host_map) = function
    | [] -> (prev_pkg, sum_strmap ~unique:per_ip host_map) :: stats
    | hd :: tl -> match hd.log_request with
      | Archive_req pkg ->
        if eq_pkg pkg prev_pkg then
          aux stats (pkg, incr_strmap hd.log_host host_map) tl
        else
          aux ((prev_pkg, sum_strmap ~unique:per_ip host_map) :: stats)
              (pkg, init_strmap hd.log_host) tl
      | _ ->
        aux stats (prev_pkg, host_map) tl
  in
  aux [] (OpamPackage.of_string "InitStatDummyPackage.0", StringMap.empty)
      sorted_entries

(* Generate basic statistics on log entries *)
let basic_stats_of_logfiles (logfiles: string list): statistics option =
  let entries = entries_of_logfiles logfiles in
  let pkgver_stats = count_archive_downloads ~per_ip: true entries in
  let eq_pkg p1 p2 = OpamPackage.name p1 = OpamPackage.name p2 in
  let pkg_stats = count_archive_downloads
      ~per_ip: true ~eq_pkg: eq_pkg entries
  in
  let global_stats =
    List.fold_left (fun acc (_, n) -> Int64.add n acc)
        Int64.zero pkg_stats
  in
  let update_stats = count_updates entries in
  if global_stats = Int64.zero && update_stats = Int64.zero then None
  else Some
    {
      pkgver_stats = pkgver_stats;
      pkg_stats = pkg_stats;
      global_stats = global_stats;
      update_stats = update_stats;
    }

(* Retrieve the 'ntop' number of packages with the higher (or lower) int value
   associated *)
let top ?(ntop = 10) ?(reverse = true) pkg_stats =
  let compare_pkg (_, n1) (_, n2) =
    if reverse then Int64.compare n2 n1
    else Int64.compare n1 n2
  in
  let sorted_pkg = List.sort compare_pkg pkg_stats in
  let rec aux acc ct = function
    | hd :: tl when ct > 0 -> aux (hd :: acc) (ct - 1) tl
    | _ -> acc
  in
  List.rev (aux [] ntop sorted_pkg)
