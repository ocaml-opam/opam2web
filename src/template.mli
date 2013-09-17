type param_prop = Default of Cow.Xml.signal list | Mandatory
type field_prop = Optional | Required
type t = {
  path : string;
  fields : (string * (param_prop * field_prop)) list;
}
val xmlns : string
val serialize : Cow.Html.t -> Cow.Xml.signal list
val default : Cow.Html.t -> param_prop
val mandatory : unit -> param_prop
val generate :
  string -> t -> (string * Cow.Xml.signal list) list -> Cow.Xml.signal list
