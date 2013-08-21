
(*
   written by Oliver Bandel. => Urheber / Copyright Oliver Bandel

   This is free software and it can be redistributed under the
   General Public License (GPL) version 2.
*)



(* ------------------------------------------------------------------------------- *)
(* This module offers the following functionality: Open a gzip'ed file for reading *)
(* and giving back a channel on it, as well as closing the channel and checking,   *)
(* if there were any problems in reading it.  It uses zcat to open the file, so it *)
(* must be installed and reachable via PATH for this module to work as expected.   *)
(* ------------------------------------------------------------------------------- *)
open Unix

let open_gzip_read file     = open_process_in ("zcat " ^ file)
let close_gzip_read channel = 
  match (close_process_in channel)
  with   WEXITED ret -> (* Printf.printf "Status: %d\n" ret; *)
    if ret != 0 then prerr_endline ("could not read file <name> !")
       | _ -> prerr_endline ("could not read file !")


