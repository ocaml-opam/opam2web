

(*
   written by Oliver Bandel. => Urheber / Copyright Oliver Bandel

   This is free software and it can be redistributed under the
   General Public License (GPL) version 2.
*)


(** This module contains the typedefinitions of the Log-Entry-Records
    and functions to create such entry-records and to extract the entities
    from such a log-entry record. It also contains functions to create the results
    of a query from the entry-selection.
*)


(** This record stores the data from the logfile *)
type t = {
  host: string; lname: string; user: string; date: string; request: string;
  status: string; size: string; referrer: string; client: string; }

(** This type classifies the selected entity (of the query) *)
type entry_type_t = Host | Lname | User | Date | Cmd | Retcode | Size | Referrer | Client | All


(** This function creates a record from all entrities. *)
let create ~host:h ~lname:l ~user:u ~date:d ~request:request ~status:rc ~size:s ~referrer:rf ~client:cl =
  { host = h; lname = l; user = u; date = d; request = request; status = rc; size = s; referrer = rf; client = cl; }


(* functions to get the contents of the record by the name of it's elements *)
let host r = r.host
let lname r = r.lname
let user r = r.user
let date r = r.date
let request  r = r.request
let status  r = r.status
let size  r = r.size
let referrer r = r.referrer
let client   r = r.client

(** the string-aequivalent for the entity type *)
let name_of_entrytype t =
  match t with
    Host     -> "host"
  | Lname    -> "lname"
  | User     -> "user"
  | Date     -> "date"
  | Cmd      -> "request"
  | Retcode  -> "status"
  | Size     -> "size"
  | Referrer -> "referrer"
  | Client   -> "client"
  | All      -> "*"



(** According to the list of querytypes (from the query-parser) here all
    necessary results are gathered from the record.
    This is collecting the data from the datastructure, preparing them for the printout.
*)
let getval_by_typelist r query_tl =
  let rec aux eatup sample =
    match eatup with
      []     -> List.rev sample
    | hd::tl -> begin
        match hd with
          Host     -> aux tl (host r :: sample)
        | Lname    -> aux tl (lname r :: sample)
        | User     -> aux tl (user r :: sample)
        | Date     -> aux tl (date r :: sample)
        | Cmd      -> aux tl (request r :: sample)
        | Retcode  -> aux tl (status r :: sample)
        | Size     -> aux tl (size r :: sample)
        | Referrer -> aux tl (referrer r :: sample)
        | Client   -> aux tl (client r :: sample)
        | All      -> aux tl ( host r ::
                                 lname r ::
                                 user r ::
                                 date r ::
                                 request r ::
                                 status r ::
                                 size r ::
                                 referrer r ::
                                 client r ::
                                 sample
                             )
      end
  in
  List.rev (aux query_tl [])



let get_lengths_by_typelist r query_tl =
  let sl = String.length in
  let rec aux eatup sample =
    match eatup with
      []     -> List.rev sample
    | hd::tl -> begin
        match hd with
          Host     -> aux tl (sl(host r) :: sample)
        | Lname    -> aux tl (sl(lname r) :: sample)
        | User     -> aux tl (sl(user r) :: sample)
        | Date     -> aux tl (sl(date r) :: sample)
        | Cmd      -> aux tl (sl(request r) :: sample)
        | Retcode  -> aux tl (sl(status r) :: sample)
        | Size     -> aux tl (sl(size r) :: sample)
        | Referrer -> aux tl (sl(referrer r) :: sample)
        | Client   -> aux tl (sl(client r) :: sample)
        | All      -> aux tl (sl( host r) ::
                                sl( lname r) ::
                                sl( user r) ::
                                sl( date r) ::
                                sl( request r) ::
                                sl( status r) ::
                                sl( size r) ::
                                sl( referrer r) ::
                                sl( client r) ::
                                sample
                             )
      end
  in
  List.rev (aux query_tl [])
