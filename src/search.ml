
(* Code from ocp-jslib in TryOCaml *)
let doc = Dom_html.document
let win = Dom_html.window
let _s = Js.string

let get_element_by_id id =
  Js.Opt.get (doc##getElementById (Js.string id)) 
    (fun () -> Firebug.console##log (_s id); assert false)

 (* Column position in the HTML table *)
let by_name = 0  
let by_descr = 2

(* Hide the row [tr] of a table element *)
let hide tr =
  tr##style##display <- _s "none"

(* Make visible the row [tr] of a table element *)
let show tr =
  tr##style##display <- _s ""

(* Filter the string [str] from the table [tbl] by looking in the column
   name (position 0) and the description (position 2) *)
let filter str tbl =
  for i = 1 to (tbl##rows##length) do
    let tr = 
      Js.Optdef.get (tbl##rows##item (i)) (fun () -> assert false) in
    (* Get the [td] corresponding to the name column *)
    let name  = 
      Js.Optdef.get (tr##cells##item (by_name)) (fun () -> assert false) in
    (* Get the [td] corresponding to the description column *)
    let descr = 
      Js.Optdef.get (tr##cells##item (by_descr)) (fun () -> assert false) in
    (* Filter name or column column of the table *)
    if (Regexp.search (Regexp.regexp (String.lowercase (Js.to_string str)))
          (String.lowercase (Js.to_string name##innerHTML)) 0) <> None ||
      (Regexp.search (Regexp.regexp (String.lowercase (Js.to_string str)))
          (String.lowercase (Js.to_string descr##innerHTML)) 0) <> None
    then 
      show tr
    else 
      hide tr
  done

let _= 
  let tbl = get_element_by_id "packages" in
  let tbl =
    match Js.Opt.to_option (Dom_html.CoerceTo.table tbl) with
      | None ->  assert false
      | Some t -> t 
  in
  let search = get_element_by_id "search" in
  let search =
    match Js.Opt.to_option (Dom_html.CoerceTo.input search) with
   | None ->  assert false
   | Some t -> t 
  in
  search##onkeyup <- Dom_html.handler
    (fun _ -> filter search##value tbl; Js._false);
