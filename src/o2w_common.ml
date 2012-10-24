type menu_item =
  | Internal of int * Cow.Html.t
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
  pkg_stats: (OpamPackage.t * int64) list;
  (** Global download count (sum of all packages download count) *)
  global_stats: int64;
  (** Update count (number of 'index.tar.gz' downloads *)
  update_stats: int64;
}

(** Log entry intermediate types *)

type log_client_os =
  | Mac_osx of string
  | Unix of string
  | Windows of string
  | Unknown_os of string

type log_client_browser =
  | Chromium of string
  | Internet_explorer of string
  | Mozilla of string
  | Unknown_browser of string

type log_client = log_client_os * log_client_browser

(** Different requests made to the repository server *)
type log_request =
  (** Request of type "GET /\\(.+\\)\\.html HTTP/[.0-9]+" *)
  | Html_req of string
  (** Request of type "GET /archives/\\(.+\\)\\+opam\\.tar\\.gz HTTP/[.0-9]+" *)
  | Archive_req of OpamPackage.t
  (** Request of type "GET /urls\\.txt HTTP/[.0-9]+" *)
  | Update_req
  | Unknown_req of string

type log_referrer =
  | External_ref of string
  | Internal_ref of string
  | No_ref

(** A high-level, OPAM-repository specific type for a apache log entry *)
type log_entry = {
  log_timestamp: Unix.tm;
  log_host: string;
  log_request: log_request;
  log_referrer: log_referrer;
  log_client: log_client;
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
