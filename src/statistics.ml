open Logentry

open O2w_common

(* Retrieve log entries of an apache access.log file *)
let entries_of_logfile (init: log_entry list) (filename: string)
    : log_entry list =
  let file = OpamFilename.of_string filename in
  let html_regexp =
    Str.regexp "GET /\\(.+\\)\\.html HTTP/[.0-9]+"
  in
  let archive_regexp =
    Str.regexp "GET /archives/\\(.+\\)\\+opam\\.tar\\.gz HTTP/[.0-9]+"
  in
  let update_regexp =
    Str.regexp "GET /urls\\.txt HTTP/[.0-9]+"
  in
  let timestamp_regexp =
    Str.regexp "[0-9]+/[A-Z][a-z]+/[0-9]+:[0-9]+:[0-9]+:[0-9]+ [-+][0-9]+"
  in
  let mk_entry e =
    (* TODO: parse timestamp, referrer and client *)
    (* Their current values are dummy ones *)
    let request =
      if Str.string_match html_regexp e.request 0 then
        Html_req (Str.matched_group 1 e.request)
      else if Str.string_match archive_regexp e.request 0 then
        Archive_req (OpamPackage.of_string (Str.matched_group 1 e.request))
      else if Str.string_match update_regexp e.request 0 then
        Update_req
      else
        Unknown_req e.request
    in
    {
      log_timestamp = Unix.gmtime (Unix.gettimeofday ());
      log_host = e.host;
      log_request = request;
      log_referrer = No_ref;
      log_client = Unknown_os "", Unknown_browser "";
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

(* Count the number of update requests in a list of entries *)
let count_updates ?(per_ip = false) (entries: log_entry list): int64 =
  List.fold_left (fun acc e -> match e.log_request with
      | Update_req -> Int64.succ acc
      | _ -> acc)
    Int64.zero entries

(* Count the number of downloads for each OPAM archive *)
let count_archive_downloads ?(per_ip = true) (entries: log_entry list)
    : (OpamPackage.t * int64) list =
  let compare_entries e1 e2 = match e1.log_request, e2.log_request with
    | Archive_req p1, Archive_req p2 ->
        String.compare (OpamPackage.to_string p1) (OpamPackage.to_string p2)
    | Archive_req _, _ -> 1
    | _, Archive_req _ -> (-1)
    | _ -> 0
  in
  let sorted_entries = List.sort compare_entries entries in
  let rec aux stats (prev_pkg, ct) = function
    | [] -> (prev_pkg, ct) :: stats
    | hd :: tl -> match hd.log_request with
      | Archive_req pkg ->
        if pkg = prev_pkg then
          aux stats (pkg, (Int64.succ ct)) tl
        else
          aux ((prev_pkg, ct) :: stats) (pkg, Int64.one) tl
      | _ ->
        aux stats (prev_pkg, ct) tl
  in
  aux [] (OpamPackage.of_string "InitStatDummyPackage.0", Int64.zero)
      sorted_entries

(* Generate basic statistics on log entries *)
let basic_stats_of_logfiles (logfiles: string list): statistics option =
  let entries = entries_of_logfiles logfiles in
  let pkg_stats = count_archive_downloads entries in
  let global_stats = List.fold_left (fun acc (_, n) -> Int64.add n acc)
      Int64.zero pkg_stats
  in
  match pkg_stats with
  | [] -> None
  | _ -> Some
    {
      pkg_stats = pkg_stats;
      global_stats = global_stats;
      update_stats = count_updates entries;
    }

