(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2018 OCamlPro                                        *)
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

type 'a t = {
  file: OpamFilename.t;
  version: Digest.t;
  default: 'a
}

let cache ~version file default =
  let version =
    Digest.string (OpamVersion.(to_string (full ())) ^" "^
                   string_of_int version)
  in
  let file = OpamFilename.of_string file in
  { file; version; default}

let write_cache (drop: 'a) (cache: 'a t) =
  OpamFilename.mkdir (OpamFilename.dirname cache.file);
  let oc = open_out_bin (OpamFilename.to_string cache.file) in
  Digest.output oc cache.version;
  Marshal.to_channel oc drop [];
  close_out oc

let read_cache_opt cache =
  try
    let ic = open_in_bin (OpamFilename.to_string cache.file) in
    let stored =
      if Digest.input ic = cache.version then
        (Printf.printf "Reading logs cache from %s... %!"
           (OpamFilename.to_string cache.file);
         let timer = OpamConsole.timer () in
         let c = Marshal.from_channel ic in
         Printf.printf "done (%.3fs).\n%!" (timer ());
         Some c)
      else
        (Printf.printf "Skipping mismatching logs cache %s\n%!"
           (OpamFilename.to_string cache.file);
         None)
    in
    close_in ic;
    stored
  with _ -> None

let read_cache cache =
  match read_cache_opt cache with
  | Some c -> c
  | None -> cache.default

