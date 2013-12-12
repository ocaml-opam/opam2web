(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes


type page = {
  page_link    : Cow.Html.link;
  page_depth   : int;
  page_contents: Cow.Xml.signal list;
}

type menu = {
  menu_link: Cow.Html.link;
  menu_item: menu_item;
}

and menu_item =
  | Internal of int * Cow.Xml.signal list
  | No_menu of int * Cow.Xml.signal list
  | Submenu of menu list
  | Nav_header
  | Divider
  | External

type statistics = {
  (** Individual package download count *)
  pkg_stats: int64 package_map;
  (** Global download count (sum of all packages download count) *)
  global_stats: int64;
  (** Update count (number of 'urls.txt' downloads *)
  update_stats: int64;
  (** Number of unique IPs *)
  users_stats: int64;
}

type statistics_set = {
  alltime_stats: statistics;
  day_stats    : statistics;
  week_stats   : statistics;
  month_stats  : statistics;
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
  log_custom: log_entry -> bool
}
