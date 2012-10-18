


(*
   written by Oliver Bandel. => Urheber / Copyright Oliver Bandel

   This is free software and it can be redistributed under the
   General Public License (GPL) version 2.
*)


(** This module handles calls to the logfile-lexer
    and provides a function that handles the logfile completely. *)

(*
APache's Common Logifle format:


HOST - - [DATUM/ZEIT] "KOMMANDO" STATUS SIZE "-" "Browser/Client" 
HOST - - [DATUM/ZEIT] "KOMMANDO" STATUS SIZE "Referrer" "Browser/Client" 

  http://httpd.apache.org/docs/1.3/mod/mod_log_common.html

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
    while true
    do
      let line = input_line ic in
      let lexbuf = Lexing.from_string line in


      let host     = Lexcombinedlog.host lexbuf in
      let lname    = Lexcombinedlog.token lexbuf in
      let user     = Lexcombinedlog.token lexbuf in
      let date     = Lexcombinedlog.date lexbuf in
      let request  = Lexcombinedlog.token lexbuf in
      let status   = Lexcombinedlog.token lexbuf in
      let size     = Lexcombinedlog.size lexbuf in
      let referrer = Lexcombinedlog.token lexbuf in
      let client   = Lexcombinedlog.token lexbuf in

    (*
      old: always insert data :(
      recordlist := Logentry.create host date request status size referrer client :: !recordlist
    *)

      (* create an entry and use the filter to decide if the entry has to be used *)
      let entry = Logentry.create host lname user date request status size referrer client in
      if filter entry then recordlist := entry :: !recordlist else ();

    done; !recordlist (* cheating the typesystem ;-) *)
  with End_of_file -> closer ic;
                     (*
                      prerr_endline "READY!";
                     *)
                      List.rev !recordlist
  
