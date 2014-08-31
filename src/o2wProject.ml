(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

let pkg_href = OpamfUniverse.Pkg.href ~href_base:Uri.(of_string "../")

module Field = struct
  type t = Author | License | Homepage | Maintainer

  let to_string = function
    | Author -> "authorship"
    | License -> "license"
    | Homepage -> "homepage"
    | Maintainer -> "maintainership"
end

module Change = struct
  type change =
  | Add of (Field.t * string)
  | Remove of (Field.t * string)
  | Change of (Field.t * string * string)
  type t = {
    change : change;
  }

  let to_html = Field.(function
    | { change = Add ((Author | Maintainer) as p,a) } ->
      <:html<<strong>$str:a$</strong> gained <em>$str:to_string p$</em>.>>
    | { change = Remove ((Author | Maintainer) as p,a) } ->
      <:html<<strong>$str:a$</strong> lost <em>$str:to_string p$</em>.>>
    | { change = Change ((Author | Maintainer) as p,a,a') } ->
      <:html<<strong>$str:a$</strong> assumed <em>$str:to_string p$</em>
             from <strong>$str:a'$</strong>.>>
    | { change = Add (License,a) } ->
      <:html<Now <em>licensed</em> under <strong>$str:a$</strong>.>>
    | { change = Remove (License,a) } ->
      <:html<No longer <em>licensed</em> under <strong>$str:a$</strong>.>>
    | { change = Change (License,a,a') } ->
      <:html<<em>License</em> changed to <strong>$str:a$</strong>
             from <strong>$str:a'$</strong>.>>
    | { change = Add (Homepage,a) } ->
      <:html<<strong><a href=$str:a$>$str:a$</a></strong> added as
             <em>homepage</em>.>>
    | { change = Remove (Homepage,a) } ->
      <:html<<strong><a href=$str:a$>$str:a$</a></strong> removed as
             <em>homepage</em>.>>
    | { change = Change (Homepage,a,a') } ->
      <:html<<em>Homepage</em> changed to
             <strong><a href=$str:a$>$str:a$</a></strong>
             from <strong><a href=$str:a'$>$str:a'$</a></strong>.>>
  )
end

module Event = struct
  type event =
  | Published of (OpamPackage.Name.t * OpamPackage.Version.t * Change.t list)
  type t = {
    timestamp : float;
    event     : event;
  }

  let to_html name = function
    | { timestamp; event = Published (name, version, changes) } ->
      let href = pkg_href name version in
      let v = OpamPackage.Version.to_string version in
      let changes_html = match changes with
        | [] -> <:html<&>>
        | _ ->
          let changes_list = List.map (fun c ->
            <:html<<li>$Change.to_html c$</li>&>>
          ) changes in
          <:html<<ul>$list:changes_list$</ul>&>>
      in
      <:html<
        <h4>
          $str:O2wMisc.string_of_timestamp timestamp$
        </h4>
        <p><strong>Published</strong>
           version <a href=$uri:href$>$str: v$</a>
           $changes_html$
        </p>
      >>
end

let to_html ~statistics universe name vset =
  let open OpamfUniverse in
  let pname = OpamPackage.Name.of_string name in

  let versions = OpamPackage.Version.(List.sort compare (Set.elements vset)) in
  let packages = List.rev_map (OpamPackage.create pname) versions in

  let opams = List.rev_map (fun pkg ->
    OpamPackage.version pkg, OpamPackage.Map.find pkg universe.pkgs_opams
  ) packages in

  let infos = List.rev_map (fun pkg ->
    OpamPackage.version pkg, OpamPackage.Map.find pkg universe.pkgs_infos
  ) packages in

  let versions_from_newest = List.rev versions in
  let previous_version v =
    let rec prev = function
      | version::pv::_ when version = v -> Some pv
      | _::vs -> prev vs
      | [] -> None
    in
    prev versions_from_newest
  in

  let latest_v = OpamPackage.Name.Map.find pname universe.max_versions in
  let latest_p = OpamPackage.create pname latest_v in
  let latest = OpamPackage.Map.find latest_p universe.pkgs_infos in

  let proj_versions =
    Some ("Versions", O2wPackage.version_links ~pkg_href universe pname None)
  in

  let mk_tr = function
    | None                   -> <:html<&>>
    | Some (title, contents) ->
      <:html<
            <tr>
              <th>$str: title$</th>
              <td>$contents$</td>
            </tr>
      >> in

  let rec pretty_html_list ?(last="and") = function
    | []    -> <:html<&>>
    | [a]   -> a
    | [a;b] -> <:html<$a$ $str: last$ $b$>>
    | h::t  -> <:html<$h$, $pretty_html_list ~last t$>>
  in

  let v_after_link v = match previous_version v with
    | None -> <:html<&>>
    | Some v ->
      let v_href = pkg_href pname v in
      let v_str = OpamPackage.Version.to_string v in
      <:html< (after <a href=$uri: v_href$>$str: v_str$</a>)>>
  in

  let span_list name l : (string * Cow.Xml.t) option = match l with
    | [] -> None
    | [e, v] -> Some (name, <:html<$e$$v_after_link v$>>)
    | l ->
      let l = List.map (fun (e,v) -> <:html<$e$$v_after_link v$>>) l in
      Some (name ^ "s", pretty_html_list l)
  in

  let span_strings name lo = match lo with
    | None -> None
    | Some l -> span_list name (List.map (fun (s,v) -> (<:html<$str:s$>>, v)) l)
  in

  let span_links name lo = match lo with
    | None -> None
    | Some l -> span_list name (List.map (fun (s,v) ->
      (<:html< <a href=$str: s$>$str: s$</a> >>,v))
                                  l)
  in

  let tenure valuevs =
    let rec prev (value, v) = function
      | [] -> Some (value, v)
      | (None, version)::vs -> prev (value, version) vs
      | (v', version)::vs when v' = value -> prev (value, version) vs
      | (Some _, _)::_ -> Some (value, v)
    in
    match valuevs with [] -> None | v::vs -> prev v vs
  in

  let tenures lists = List.fold_left (fun m (v, items) -> match m with
    | None -> Some (List.map (fun i -> (i, v)) items)
    | Some ts -> Some (List.map (fun (i,late_v) ->
      (i, if items = [] || List.mem i items then v else late_v)
    ) ts)
  ) None lists in

  let opam_field span_fn name field_fn =
    let items = List.rev_map (fun (v, o) -> (v, field_fn o)) opams in
    span_fn name (tenures items)
  in

  let proj_author = opam_field span_strings "Author" OpamFile.OPAM.author in

  let proj_license = opam_field span_strings "License" OpamFile.OPAM.license in

  let proj_homepage = opam_field span_links "Homepage" OpamFile.OPAM.homepage in

  let proj_issues =
    opam_field span_links "Issue Tracker" OpamFile.OPAM.bug_reports
  in

  let proj_maintainer =
    opam_field span_strings "Maintainer" OpamFile.OPAM.maintainer
  in

  let proj_tags = opam_field span_strings "Tag" OpamFile.OPAM.tags in

  let opam_compiler name field_fn =
    let valuevs = List.rev_map (fun (v, o) -> (field_fn o, v)) opams in
    match tenure valuevs with
    | None | Some (None, _) -> None
    | Some (Some v, version) ->
      let formula_str = OpamFormula.(
        string_of_formula (fun (relop,v) ->
          (string_of_relop relop)^" "^(OpamCompiler.Version.to_string v)
        ) v
      ) in
      Some (name, <:html<$str:formula_str$$v_after_link version$>>)
  in

  let opam_os name field_fn =
    let valuevs = List.rev_map (fun (v, o) -> (Some (field_fn o), v)) opams in
    OpamFormula.(match tenure valuevs with
    | None | Some (None, _) | Some (Some Empty, _) -> None
    | Some (Some f, version) ->
      let formula_str = string_of_formula (fun (b,s) ->
        if b then s else "!"^s
      ) f
      in
      Some (name, <:html<$str:formula_str$$v_after_link version$>>)
    )
  in

  let proj_compiler = opam_compiler "OCaml" OpamFile.OPAM.ocaml_version in

  let proj_os = opam_os "OS" OpamFile.OPAM.os in

  let module StringSet = Set.Make(String) in

  let set_of_list =
    List.fold_left (fun set v -> StringSet.add v set) StringSet.empty
  in

  let list_diff field_t field field' = match field, field' with
    | [], [] -> []
    | _, _ ->
      let a = set_of_list field in
      let b = set_of_list field' in
      let removes = StringSet.(elements (diff b a)) in
      let adds = StringSet.(elements (diff a b)) in
      match adds, removes with
      | [a], [r] -> [Change.({ change = Change (field_t, a, r)})]
      | _, _ ->
        (List.map
           (fun v -> Change.({ change = Add (field_t, v) }))
           adds)
        @(List.map
            (fun v -> Change.({ change = Remove (field_t, v) }))
            removes)
  in

  let opam_list_diff pkg pkg' field_t field_fn =
    let opam  = OpamPackage.Map.find pkg  universe.pkgs_opams in
    let opam' = OpamPackage.Map.find pkg' universe.pkgs_opams in
    list_diff field_t (field_fn opam) (field_fn opam')
  in

  let events = List.fold_left OpamfUniverse.(fun l -> function
    | _, { published = None }           -> l
    | v, { published = Some timestamp } ->
      let p = OpamPackage.create pname v in
      let changes = match previous_version v with
        | None -> []
        | Some prev ->
          let prev = OpamPackage.create pname prev in
          (opam_list_diff p prev Field.Author OpamFile.OPAM.author)
          @(opam_list_diff p prev Field.License OpamFile.OPAM.license)
          @(opam_list_diff p prev Field.Maintainer OpamFile.OPAM.maintainer)
          @(opam_list_diff p prev Field.Homepage OpamFile.OPAM.homepage)
      in
      Event.({ timestamp; event=Published (pname, v, changes) }::l)
  ) [] infos in

  let proj_events = List.map (fun ev ->
    <:html<<div class="event">$Event.to_html name ev$</div>&>>
  ) events in

  <:html<
    <h2>$str: name$</h2>

    <div class="row">
      <div class="span12">

        <div class="well">$latest.descr$</div>

        <table class="table">
          <tbody>
            $mk_tr proj_versions$
            $mk_tr proj_author$
            $mk_tr proj_license$
            $mk_tr proj_homepage$
            $mk_tr proj_issues$
            $mk_tr proj_maintainer$
            $mk_tr proj_tags$

            $mk_tr proj_compiler$
            $mk_tr proj_os$
          </tbody>
        </table>

        <h3>Events</h3>

        $list:proj_events$
      </div>
    </div>
  >>
