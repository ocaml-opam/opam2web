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
let readlog logname filter =
  let recordlist = ref [] in
  let opener = if Filename.check_suffix logname ".gz" then Compressed.open_gzip_read else open_in in
  let closer = if Filename.check_suffix logname ".gz" then Compressed.close_gzip_read else close_in in
  let ic = opener logname in
  try
    while true do
      let line = input_line ic in
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
          Printf.eprintf "error while parsing:\n%s\n%!" line;
          "0" in
      let referrer = Lexcombinedlog.token lexbuf in
      let client   = Lexcombinedlog.token lexbuf in
      let entry = Logentry.create host lname user date request status size referrer client in
      if filter entry then
        recordlist := entry :: !recordlist
    done;
    !recordlist
  with End_of_file ->
    closer ic;
    List.rev !recordlist
