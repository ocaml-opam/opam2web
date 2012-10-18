open Logentry

type statistics = {
  (** Individual package download count *)
  pkg_stats: (OpamPackage.t * int64) list;
  (** Global download count (sum of all packages download count) *)
  global_stats: int64;
  (** Update count (number of 'index.tar.gz' downloads *)
  update_stats: int64;
}

(* Retrieve statistics from a filename if it exists, return none otherwise *)
let of_logfile (filename: string): statistics option =
  let file = OpamFilename.of_string filename in
  let parse_stats () =
    let archive_regexp =
      Str.regexp "GET /archives/\\(.+\\)\\+opam\\.tar\\.gz HTTP/[.0-9]+"
    in
    let index_regexp = Str.regexp "GET /index\\.tar\\.gz HTTP/[.0-9]+" in
    let filter (entry: Logentry.entry_t): bool =
      Str.string_match index_regexp entry.request 0 ||
          Str.string_match archive_regexp entry.request 0
    in
    let compare_requests e1 e2 = String.compare e1.request e2.request in
    let stats_of_entries entries =
      let add_pkg_assoc name ct l =
        if String.length name = 0 then l
        else (OpamPackage.of_string name, ct) :: l
      in
      let rec aux pkg_stats global_stats update_stats (prev_pkg, ct) = function
        | [] -> add_pkg_assoc prev_pkg ct pkg_stats, global_stats, update_stats
        | hd :: tl ->
            if Str.string_match index_regexp hd.request 0 then
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
                      (Int64.succ global_stats) update_stats (pkg, Int64.one) tl
              else
                aux pkg_stats global_stats update_stats (prev_pkg, ct) tl
      in
      aux [] Int64.zero Int64.zero ("", Int64.zero) entries
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
