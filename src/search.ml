
let doc = Dom_html.document

let win = Dom_html.window

let _s = Js.string

let get_element_by_id id =
  Js.Opt.get (doc##getElementById (Js.string id)) 
    (fun () -> Firebug.console##log (_s id); assert false)

let by_name = 0
let by_descr = 2

let hide tr =
  tr##style##display <- _s "none"

let show tr =
  tr##style##display <- _s ""


let filter str tbl =
  for i = 1 to (tbl##rows##length) do
    let tr = Js.Optdef.get (tbl##rows##item (i)) (fun () -> assert false) in
    let name  = Js.Optdef.get (tr##cells##item (by_name)) (fun () -> assert false) in
    let descr = Js.Optdef.get (tr##cells##item (by_descr)) (fun () -> assert false) in
    if (Regexp.search (Regexp.regexp (String.lowercase (Js.to_string str)))
          (String.lowercase (Js.to_string name##innerHTML)) 0) <> None ||
      (Regexp.search (Regexp.regexp (String.lowercase (Js.to_string str)))
          (String.lowercase (Js.to_string descr##innerHTML)) 0) <> None
    then show tr
    else hide tr
  done

let _= ()
  let tbl = get_element_by_id "packages" in
  let tbl =
    match Js.Opt.to_option (Dom_html.CoerceTo.table tbl) with
      | None ->  Firebug.console##log (_s "3\n");assert false
      | Some t -> t in

  let search = get_element_by_id "search" in
  let search =
    match Js.Opt.to_option (Dom_html.CoerceTo.input search) with
   | None ->  Firebug.console##log (_s "4\n");assert false
   | Some t -> t in
  search##onkeyup <- Dom_html.handler
    (fun _ -> filter search##value tbl; Js._false);
