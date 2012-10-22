open Logentry

open O2w_common

(* Retrieve statistics from a filename if it exists, return none otherwise *)
let of_logfile (init: statistics) (filename: string): statistics option =
  let file = OpamFilename.of_string filename in
  let parse_stats () =
    let archive_regexp =
      Str.regexp "GET /archives/\\(.+\\)\\+opam\\.tar\\.gz HTTP/[.0-9]+"
    in
    let update_regexp = Str.regexp "GET /urls\\.txt HTTP/[.0-9]+" in
    let filter (entry: Logentry.entry_t): bool =
      Str.string_match update_regexp entry.request 0 ||
          Str.string_match archive_regexp entry.request 0
    in
    let compare_requests e1 e2 = String.compare e1.request e2.request in
    let stats_of_entries entries =
      let add_pkg_assoc name ct l =
        if String.length name = 0 then l
        else
          (OpamPackage.of_string name, ct) :: l
      in
      let init_pkg_count pkg_stats pkg =
        Int64.add Int64.one
        (try
          List.assoc (OpamPackage.of_string pkg) init.pkg_stats
        with
          Not_found -> Int64.zero)
      in
      let rec aux pkg_stats global_stats update_stats (prev_pkg, ct) = function
        | [] -> add_pkg_assoc prev_pkg ct pkg_stats, global_stats, update_stats
        | hd :: tl ->
            if Str.string_match update_regexp hd.request 0 then
              aux pkg_stats global_stats (Int64.succ update_stats)
                  (prev_pkg, ct) tl
            else
              if Str.string_match archive_regexp hd.request 0 then
                let pkg = Str.matched_group 1 hd.request in
                if pkg = prev_pkg then
                  aux pkg_stats (Int64.succ global_stats) update_stats
                      (pkg, (Int64.succ ct)) tl
                else
                  aux (add_pkg_assoc prev_pkg ct pkg_stats)
                      (Int64.succ global_stats) update_stats
                      (pkg, init_pkg_count pkg_stats pkg) tl
              else
                aux pkg_stats global_stats update_stats (prev_pkg, ct) tl
      in
      aux init.pkg_stats init.global_stats init.update_stats
          ("", Int64.zero) entries
    in
    let log_entries: Logentry.entry_t list =
      Readcombinedlog.readlog filename filter
    in
    let log_entries_sorted = List.sort compare_requests log_entries in
    let pkg_stats, global_stats, update_stats =
      stats_of_entries log_entries_sorted
    in
    {
      pkg_stats = pkg_stats;
      global_stats = global_stats;
      update_stats = update_stats;
    }
  in
  if OpamFilename.exists file then
    begin
      Printf.printf "Parsing web server log file '%s'...\n" filename;
      Some (parse_stats ())
    end
  else
    begin
      Printf.printf "No web server log file found.\n";
      None
    end

let of_logfiles (logfiles: string list): statistics option =
  let init_stats = {
      pkg_stats = [];
      global_stats = Int64.zero;
      update_stats = Int64.zero;
    }
  in
  let stats = List.fold_left (fun acc log ->
      match of_logfile acc log with
      | None -> acc
      | Some s -> s)
    init_stats logfiles
  in
  if stats = init_stats then None
  else Some stats
