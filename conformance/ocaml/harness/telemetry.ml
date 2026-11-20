(* A service, or an SDK, held to the telemetry the spec lists for it
   (specs/src/service-telemetry/telemetry.json, and the rows of
   specs/src/sdk-telemetry/telemetry.json it names; or that table alone),
   from what its OTLP receiver got, in both directions. *)

let shown_value : Otlp.value -> string = function
  | Text s -> s
  | Flag b -> string_of_bool b
  | Number n -> Printf.sprintf "%g" n
  | Unreadable -> "undefined"

(* A value as JSON writes it, for a finding. *)
let json_value : Otlp.value -> string = function
  | Text s -> Yojson.Safe.to_string (`String s)
  | other -> shown_value other

let listed_json : Protocol.value -> string = function
  | String s -> Yojson.Safe.to_string (`String s)
  | Boolean b -> string_of_bool b

let carries (listed : Protocol.value) (value : Otlp.value) =
  match (listed, value) with
  | String s, Text t -> s = t
  | Boolean b, Flag f -> b = f
  | _ -> false

let shown_resource (resource : Otlp.resource) =
  "{"
  ^ String.concat ", "
      (List.map (fun (key, value) -> key ^ "=" ^ shown_value value) resource)
  ^ "}"

let bus_span = Re.compile (Re.Pcre.re {|^(bus\.(?:emit|recv)):.*$|})

(* Everything the receiver got, by name, for a failing case to show. A bus
   span's attributes, and an id, show only that they were carried. *)
let received (otlp : Otlp.t) =
  let spans = Dynarray.create () in
  Dynarray.iter
    (fun (span : Otlp.span) ->
      let name =
        Re.replace bus_span
          ~f:(fun g -> Re.Group.get g 1 ^ ":{channel}")
          span.name
      in
      let _, kind, attributes, resources =
        match Dynarray.find_opt (fun (n, _, _, _) -> n = name) spans with
        | Some seen -> seen
        | None ->
            let seen =
              (name, span.kind, Dynarray.create (), Dynarray.create ())
            in
            Dynarray.add_last spans seen;
            seen
      in
      ignore kind;
      List.iter
        (fun (key, value) ->
          Otlp.add_once attributes
            (key ^ "="
            ^
            if
              String.starts_with ~prefix:"bus." name
              || String.ends_with ~suffix:".id" key
            then "…"
            else shown_value value))
        span.attributes;
      Otlp.add_once resources (shown_resource span.resource))
    otlp.spans;
  let metrics =
    List.sort compare
      (List.map
         (fun (name, (metric : Otlp.metric)) ->
           Printf.sprintf "%s [%s] %s resource=%s" name
             (String.concat "," (Dynarray.to_list metric.instruments))
             (String.concat " "
                (List.map
                   (fun (key, values) ->
                     key ^ "="
                     ^
                     if key = "bus.channel" then "…"
                     else
                       String.concat "|"
                         (List.sort compare
                            (List.map shown_value (Dynarray.to_list values))))
                   (Dynarray.to_list metric.attributes)))
             (String.concat "|"
                (List.map shown_resource (Dynarray.to_list metric.resources))))
         (Dynarray.to_list otlp.metrics))
  in
  String.concat "\n"
    ([ "spans:" ]
    @ List.map
        (fun (name, kind, attributes, resources) ->
          Printf.sprintf "  %s kind=%d resource=%s %s" name kind
            (String.concat "|" (Dynarray.to_list resources))
            (String.concat " "
               (List.sort compare (Dynarray.to_list attributes))))
        (Dynarray.to_list spans)
    @ [ "metrics:" ]
    @ List.map (fun m -> "  " ^ m) metrics
    @ List.map (fun u -> "unreadable: " ^ u) (Dynarray.to_list otlp.unreadable)
    )

(* What [attributes], carried under a row's name, carries that the row does
   not list: a key it does not name, or a value outside the values it lists,
   each value of the type it was carried as, so true is not "true". *)
let outside_the_row what (listed : Protocol.attribute list) attributes =
  List.concat_map
    (fun (key, values) ->
      match
        List.find_opt (fun (a : Protocol.attribute) -> a.key = key) listed
      with
      | None ->
          [
            Printf.sprintf "%s carries %s, which the spec does not list" what
              key;
          ]
      | Some { values = None; _ } -> []
      | Some { values = Some allowed; _ } ->
          List.filter_map
            (fun v ->
              if List.exists (fun l -> carries l v) allowed then None
              else
                Some
                  (Printf.sprintf "%s carries %s=%s, outside %s" what key
                     (json_value v)
                     (String.concat ", " (List.map listed_json allowed))))
            values)
    attributes

let uniq findings =
  List.rev
    (List.fold_left
       (fun seen f -> if List.mem f seen then seen else f :: seen)
       [] findings)

(* [whole] says the exporter is the whole of what the rows are for, so that
   what no row names is a finding. *)
let held_to ~whole (otlp : Otlp.t)
    ((spans : Protocol.span_row list), (metrics : Protocol.metric_row list)) who
    =
  let required = List.filter (fun (a : Protocol.attribute) -> not a.only) in
  let find_span pattern extra =
    Dynarray.find_opt
      (fun (s : Otlp.span) -> Re.execp pattern s.name && extra s)
      otlp.spans
  in
  List.iter
    (fun (row : Protocol.span_row) ->
      if row.span_expected then (
        let pattern = Protocol.span_pattern row.name in
        ignore
          (Run.eventually
             ("a " ^ row.name ^ " span")
             15.0
             (fun () -> find_span pattern (fun _ -> true)));
        List.iter
          (fun (a : Protocol.attribute) ->
            ignore
              (Run.eventually
                 (Printf.sprintf "a %s span carrying %s" row.name a.key) 15.0
                 (fun () ->
                   find_span pattern (fun s ->
                       List.mem_assoc a.key s.attributes))))
          (required row.span_attributes)))
    spans;
  List.iter
    (fun (row : Protocol.metric_row) ->
      if row.metric_expected then
        let seen =
          Run.eventually
            ("the " ^ row.metric ^ " metric")
            15.0
            (fun () -> Otlp.metric otlp row.metric)
        in
        List.iter
          (fun (a : Protocol.attribute) ->
            Run.eventually (Printf.sprintf "%s carrying %s" row.metric a.key)
              15.0 (fun () ->
                if Dynarray.exists (fun (k, _) -> k = a.key) seen.attributes
                then Some ()
                else None))
          (required row.metric_attributes))
    metrics;
  let of_spans =
    List.concat_map
      (fun (span : Otlp.span) ->
        match
          List.find_opt
            (fun (r : Protocol.span_row) ->
              Re.execp (Protocol.span_pattern r.name) span.name)
            spans
        with
        | None ->
            if whole then
              [
                Printf.sprintf "a span %s, which the spec does not list for %s"
                  span.name who;
              ]
            else []
        | Some row ->
            (if span.kind <> row.kind then
               [
                 Printf.sprintf "%s is of kind %d, not %d" span.name span.kind
                   row.kind;
               ]
             else [])
            @ outside_the_row span.name row.span_attributes
                (List.map (fun (k, v) -> (k, [ v ])) span.attributes))
      (Dynarray.to_list otlp.spans)
  in
  let of_metrics =
    List.concat_map
      (fun (name, (metric : Otlp.metric)) ->
        match
          List.find_opt
            (fun (r : Protocol.metric_row) -> r.metric = name)
            metrics
        with
        | None ->
            if whole then
              [
                Printf.sprintf
                  "a metric %s, which the spec does not list for %s" name who;
              ]
            else []
        | Some row ->
            List.filter_map
              (fun instrument ->
                if instrument = row.instrument then None
                else
                  Some
                    (Printf.sprintf "%s arrived as a %s, not a %s" name
                       instrument row.instrument))
              (Dynarray.to_list metric.instruments)
            @ outside_the_row name row.metric_attributes
                (List.map
                   (fun (k, values) -> (k, Dynarray.to_list values))
                   (Dynarray.to_list metric.attributes)))
      (Dynarray.to_list otlp.metrics)
  in
  of_spans @ of_metrics

(* The resource attribute a service names the knowledge base it serves by. *)
let kb_key = "xynolog.kb"

(* Wait for every row [service] must export to arrive, with every attribute
   not marked only; then what arrived that the spec does not list for it: a
   span or metric no row of its names, of another kind, or carrying an
   attribute or value outside its row; or exported under a resource that
   carries other keys than the table's resource rows list for it, or names
   another knowledge base than [kb]'s DID. [kb] is None where the case's
   document names none; which it is must be what the table says. [plane] is
   the plane a gateway runs on, whose rows it must export. Judged on
   everything the receiver got, so run it after the traffic. *)
let outside_the_table ?plane (otlp : Otlp.t) service (kb : string option) =
  let service_word = Protocol.Service_rows.services_item_value service in
  let keys = Protocol.resource_keys service in
  (match (List.mem kb_key keys, kb) with
  | true, None ->
      failwith
        (Printf.sprintf
           "the table says the %s names its knowledge base in its resource \
            (%s), and the case handed it none"
           service_word kb_key)
  | false, Some did ->
      failwith
        (Printf.sprintf
           "the case handed the %s the knowledge base %s, and the table says \
            its resource names none"
           service_word did)
  | _ -> ());
  let unlisted =
    held_to ~whole:true otlp
      (Protocol.telemetry ?plane service)
      ("the " ^ service_word)
  in
  let resources = Dynarray.create () in
  let under resource =
    match Dynarray.find_opt (fun (r, _, _) -> r = resource) resources with
    | Some (_, spans, metrics) -> (spans, metrics)
    | None ->
        let counts = (ref 0, ref 0) in
        Dynarray.add_last resources (resource, fst counts, snd counts);
        counts
  in
  Dynarray.iter
    (fun (span : Otlp.span) -> incr (fst (under span.resource)))
    otlp.spans;
  Dynarray.iter
    (fun (_, (metric : Otlp.metric)) ->
      Dynarray.iter (fun r -> incr (snd (under r))) metric.resources)
    otlp.metrics;
  let of_resources =
    List.concat_map
      (fun (resource, spans, metrics) ->
        let what =
          Printf.sprintf "the resource %s, of %d spans and %d metrics,"
            (shown_resource resource) !spans !metrics
        in
        List.filter_map
          (fun (key, _) ->
            if List.mem key keys then None
            else
              Some
                (Printf.sprintf
                   "%s carries %s, which the spec does not list for the %s" what
                   key service_word))
          resource
        @ List.filter_map
            (fun key ->
              if List.mem_assoc key resource then None
              else
                Some
                  (Printf.sprintf
                     "%s does not carry %s, which the spec lists for the %s"
                     what key service_word))
            keys
        @
        match (kb, List.assoc_opt kb_key resource) with
        | Some did, Some named when named <> Otlp.Text did ->
            [
              Printf.sprintf "%s names the knowledge base %s, not %s" what
                (shown_value named) did;
            ]
        | _ -> [])
      (Dynarray.to_list resources)
  in
  uniq (unlisted @ of_resources)

(* The same for an SDK, from what its driver exported, against
   sdk-telemetry/telemetry.json, with one difference. A service is its whole
   process, so anything it exports that no row names is a finding. A
   driver's process is the SDK and whatever else the driver runs, so what no
   row names is not the SDK's to answer for; what arrives under a row's name
   is. A driver that uses one [transport] alone is held to its rows. *)
let outside_the_sdk_table ?transport otlp =
  uniq
    (held_to ~whole:false otlp (Protocol.sdk_telemetry ?transport ()) "an SDK")

(* ── Log lines (service-telemetry/log-lines.json) ───────────────────── *)

(* What [template] holds before its [{placeholder}]. *)
let before placeholder template =
  match Re.exec_opt (Re.compile (Re.str placeholder)) template with
  | Some found -> String.sub template 0 (Re.Group.start found 0)
  | None -> failwith (Printf.sprintf "%S names no %s" template placeholder)

(* The bus log's line for [op] on [channel]; and what every line of it
   starts with. *)
let bus_line op channel =
  let bus = (Protocol.tables ()).service_telemetry_log_lines.bus in
  if not (List.mem op bus.ops) then
    failwith ("service-telemetry/log-lines.json lists no bus op " ^ op);
  Protocol.fill_template bus.line [ ("op", op); ("channel", channel) ]

let bus_line_start () =
  before "{op}" (Protocol.tables ()).service_telemetry_log_lines.bus.line

(* A supervisor's events file, as SUPERVISE_EVENTS names one: [lives]
   starts of the service it calls [name], each its log line. [body] runs
   with its path, and it is removed after. *)
let with_supervisor_events ~name ~lives body =
  let events =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "%s-conformance-supervisor-%s.log" name (Fresh.uuid ()))
  in
  let life n =
    Protocol.fill_template
      (Protocol.tables ()).service_telemetry_log_lines.supervisor.line
      [
        ("time", Printf.sprintf "2026-09-27T00:00:0%dZ" n);
        ("name", name);
        ("count", string_of_int n);
      ]
  in
  Out_channel.with_open_text events (fun out ->
      List.iter
        (fun n -> output_string out (life n ^ "\n"))
        (List.init lives Fun.id));
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists events then Sys.remove events)
    (fun () -> body events)
