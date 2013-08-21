(*
   written by Oliver Bandel. => Urheber / Copyright Oliver Bandel

   This is free software and it can be redistributed under the
   General Public License (GPL) version 2.
*)

(** The logfile-reader (Apache Common Logfile.
    The filter-function (WHERE-clause) is given as second parameter.

    readlog fn filt  reads filname fn and uses filter filt

    plain files as well as gzip'ed files are now possible to read! :-)
*)

type t = {
  name  : string;
  ic    : in_channel;
  filter: Logentry.t -> bool;
  close : unit -> unit;
}

let create filter filename =
  let opener =
    if Filename.check_suffix filename ".gz" then Compressed.open_gzip_read
    else open_in in
  let closer =
    if Filename.check_suffix filename ".gz" then Compressed.close_gzip_read
    else close_in in
  let ic = opener filename in
  { name = filename;
    ic;
    filter;
    close = fun () -> closer ic }

let size t =
  in_channel_length t.ic

let read t chunk_size =
  let results = ref [] in
  try
    for _i = 1 to chunk_size do
      let line = input_line t.ic in
      let line_size = String.length line in
      let lexbuf = Lexing.from_string line in
      let host     = Lexcombinedlog.host lexbuf in
      let lname    = Lexcombinedlog.token lexbuf in
      let user     = Lexcombinedlog.token lexbuf in
      let date     = Lexcombinedlog.date lexbuf in
      let request  = Lexcombinedlog.token lexbuf in
      let status   = Lexcombinedlog.token lexbuf in
      let size     =
        try Lexcombinedlog.size lexbuf
        with _ ->
          Printf.eprintf "error while parsing:\n%s\n" line;
          "0" in
      let referrer = Lexcombinedlog.token lexbuf in
      let client   = Lexcombinedlog.token lexbuf in
      let entry =
        Logentry.create host lname user date request status size referrer client
          line_size in
      if t.filter entry then
        results := entry :: !results
    done;
    List.rev !results
  with End_of_file ->
    List.rev !results

let close t =
  t.close ()
