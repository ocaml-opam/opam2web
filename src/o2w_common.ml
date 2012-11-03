open Unix
open OpamTypes

type menu_item =
  | Internal of int * Cow.Html.t
  | No_menu of int * Cow.Html.t
  | Submenu of ((Cow.Html.link * menu_item) list)
  | Nav_header
  | Divider
  | External

type package_info = {
  pkg_name     : string;
  pkg_version  : string;
  pkg_descr    : Cow.Html.t;
  pkg_synopsis : string;
  pkg_href     : string;
  pkg_title    : string;
}

type statistics = {
  (** Individual package download count *)
  pkg_stats: (package * int64) list;
  (** Global download count (sum of all packages download count) *)
  global_stats: int64;
  (** Update count (number of 'urls.txt' downloads *)
  update_stats: int64;
}

type statistics_set = {
  alltime_stats: statistics;
  day_stats: statistics;
  week_stats: statistics;
  month_stats: statistics;
}

(** Log entry intermediate types *)

type log_client_os =
  | Mac_osx of string
  | Unix of string
  | Windows of string
  | Unknown_os of string

type log_client_browser =
  | Chrome of string
  | Firefox of string
  | Internet_explorer of string
  | Safari of string
  | Unknown_browser of string

type log_client = log_client_os * log_client_browser

(** Different requests made to the repository server *)
type log_request =
  (** Request of type "GET /\\(.+\\)\\.html HTTP/[.0-9]+" *)
  | Html_req of string
  (** Request of type "GET /archives/\\(.+\\)\\+opam\\.tar\\.gz HTTP/[.0-9]+" *)
  | Archive_req of package
  (** Request of type "GET /urls\\.txt HTTP/[.0-9]+" *)
  | Update_req
  | Unknown_req of string

type log_referrer =
  | External_ref of string
  | Internal_ref of string
  | No_ref

(** A high-level, OPAM-repository specific type for a apache log entry *)
type log_entry = {
  log_timestamp: float;
  log_host: string;
  log_request: log_request;
  log_referrer: log_referrer;
  log_client: log_client;
}

type log_filter = {
  filter_name: string;
  log_per_ip: bool;
  log_start_time: float;
  log_end_time: float;
}

(* Global values *)

(* The list contains elements with this syntax :
   <string> without extension -> A menu title
   <string> with 'md' extension -> A markdown page in content/doc
   empty <string> -> A menu divider
 *)
let documentation_pages = [
  "Primer";
  "About.md";
  "Quick_Install.md";
  "Basic_Usage.md";
  "";
  "Go Further";
  "Advanced_Install.md";
  "Advanced_Usage.md";
  "Developing.md";
  "";
  "For Packagers";
  "Packaging.md"
]

(** Statistics related global values *)

let default_log_filter = {
  filter_name = "default";
  log_per_ip = false;
  log_start_time = 0.;
  log_end_time = Unix.time ();
}

(** List related functions *)

(* Retrieve the 'n' first elements of a list *)
let first_n nmax l =
  let rec aux acc n = function
    | hd :: tl when n > 0 -> aux (hd :: acc) (n - 1) tl
    | _ -> acc
  in
  List.rev (aux [] nmax l)

(* Date related functions *)

let month_of_string: string -> int = function
  | "Jan" -> 0
  | "Feb" -> 1
  | "Mar" -> 2
  | "Apr" -> 3
  | "May" -> 4
  | "Jun" -> 5
  | "Jul" -> 6
  | "Aug" -> 7
  | "Sep" -> 8
  | "Oct" -> 9
  | "Nov" -> 10
  | "Dec" -> 11
  | unknown -> failwith ("Unknown month: " ^ unknown)

let string_of_month: int -> string = function
  | 0  -> "Jan"
  | 1  -> "Feb"
  | 2  -> "Mar"
  | 3  -> "Apr"
  | 4  -> "May"
  | 5  -> "Jun"
  | 6  -> "Jul"
  | 7  -> "Aug"
  | 8  -> "Sep"
  | 9  -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | unknown -> failwith ("Unknown month: " ^ (string_of_int unknown))

let string_of_timestamp (time: float): string =
  let tm = Unix.gmtime time in
  let month_str = string_of_month tm.tm_mon in
  let year = 1900 + tm.tm_year in
  Printf.sprintf "%s %d, %d" month_str tm.tm_mday year
