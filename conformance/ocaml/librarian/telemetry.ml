(* What a Librarian exports (LIBRARIAN.md § Telemetry),
   case for case. *)

open Xynolog_conformance
open Support

let member = Yojson.Safe.Util.member

let options ?(content = false) ?(summary = false) () =
  `Assoc
    [
      ("depth", `Int 1);
      ("maxResources", `Int 10);
      ("includeContent", `Bool content);
      ("includeSummary", `Bool summary);
    ]

(* A resource gather that asks for the graph and the similar passages and
   for nothing a model writes; one that asks for the texts too; and one
   that asks its model for a summary and suggestions. *)
let plain = options ()
let texts = options ~content:true ()
let summarized = options ~content:true ~summary:true ()

(* A settle bound short enough for the case to wait out. *)
let short_settle_ms = 2_000

(* Wait until [metric] has carried every value the table lists for the
   Librarian's [key] of it. *)
let every_value_of otlp metric key =
  let _, metrics = Protocol.telemetry Librarian in
  let listed =
    match
      List.find_opt
        (fun (row : Protocol.metric_row) ->
          row.metric = Protocol.metric_name metric)
        metrics
    with
    | Some row -> (
        match
          List.find_opt
            (fun (a : Protocol.attribute) -> a.key = key)
            row.metric_attributes
        with
        | Some { values = Some values; _ } -> values
        | _ ->
            failwith
              (Printf.sprintf
                 "the table lists no values of %s for a Librarian's %s" key
                 metric))
    | None -> failwith ("the table lists no " ^ metric ^ " for a Librarian")
  in
  Run.eventually
    (Printf.sprintf "%s carrying every %s: %s" metric key
       (String.concat ", " (List.map Telemetry.listed_json listed)))
    30.0
    (fun () ->
      match Otlp.metric otlp metric with
      | None -> None
      | Some seen -> (
          match Dynarray.find_opt (fun (k, _) -> k = key) seen.attributes with
          | Some (_, values)
            when List.for_all
                   (fun l -> Dynarray.exists (Telemetry.carries l) values)
                   listed ->
              Some ()
          | _ -> None))

let exports_exactly_the_table otlp (w : W.t) =
  (* The world's Librarian was started with no endpoint, and exports
     nothing. This one is told where to export, and how often, and waits
     two seconds for the Smelter. *)
  let short =
    W.changed w [ "gather"; "settleTimeoutMs" ] (Some (`Int short_settle_ms))
  in
  W.restart_librarian w
    ~launch:
      {
        short with
        env =
          short.env
          @ [
              ("OTEL_EXPORTER_OTLP_ENDPOINT", otlp.Otlp.endpoint);
              ("OTEL_METRIC_EXPORT_INTERVAL", "500");
              ("OTEL_BSP_SCHEDULE_DELAY", "100");
            ];
      };
  (* Two requests of a model that are answered, the first with the tokens its provider counted. *)
  let marsh = W.created w tide ~name:"A salt marsh" in
  W.ready w [ marsh ];
  Ollama.script w.gatherer
    [
      Answer
        {
          response = "A marsh that floods twice a day.";
          stop = Done;
          usage = Some (212, 9);
        };
      answer
        (Yojson.Safe.to_string
           (`List
              [
                `Assoc [ ("suggestion", `String "Read the tide tables") ];
                `Assoc [ ("suggestion", `String "Walk the sea wall") ];
              ]));
    ];
  let resource_gather id options =
    `Assoc [ ("resourceId", `String id); ("options", options) ]
  in
  ignore
    (W.answered w "gather:resource-requested"
       (resource_gather marsh summarized));
  (* A request of a model that fails: answered without its summary and suggestions. *)
  Ollama.script w.gatherer [ fails ];
  ignore
    (W.answered w "gather:resource-requested"
       (resource_gather marsh summarized));
  (* Two more that fail, in a gather around a reference bound to the
     marsh: the summary of what it links to, then the relationship
     sentence. Answered without either. *)
  let fable = W.created w fox ~name:"A fox and a dog" in
  let reference = W.referenced w fable "fox" in
  W.bound w fable reference marsh;
  W.ready w [ fable ];
  Ollama.script w.gatherer [ fails; fails ];
  ignore
    (W.answered w "gather:requested"
       (`Assoc
          [ ("annotationId", `String reference); ("resourceId", `String fable) ]));
  (* A text that is derived from a resource's bytes is asked of the gateway. *)
  let pdf =
    W.created w
      (Pdf.of_pages [ [ "A snipe drums over the fen" ] ])
      ~name:"Birds of the fen" ~format:"application/pdf"
  in
  W.ready w [ pdf ];
  ignore (W.answered w "gather:resource-requested" (resource_gather pdf texts));
  (* A gather the Smelter does not decide in time: answered without similar passages. *)
  ignore (W.stop_smelter w : int option);
  Fun.protect
    ~finally:(fun () -> W.start_smelter w)
    (fun () ->
      let unsettled =
        W.created w "A cooper hoops the staves of an oak cask.\n" ~name:"Cask"
      in
      W.woven w [ unsettled ];
      ignore
        (W.answered w "gather:resource-requested"
           (resource_gather unsettled plain)));
  (* A gather for a resource the graph does not come to hold: failed. *)
  ignore (W.stop_weaver w : int option);
  Fun.protect
    ~finally:(fun () -> W.start_weaver w)
    (fun () ->
      let unwoven =
        W.created w "A stoat hunts along the dry stone wall.\n" ~name:"Stoat"
      in
      ignore
        (Run.eventually ("the view of " ^ unwoven) 10.0 (fun () ->
             Archivist_world.view w.kb unwoven));
      ignore
        (W.failed w "gather:resource-requested" (resource_gather unwoven plain)));
  let complete =
    Protocol.span_name "bus.emit:{channel}"
      [ ("channel", "gather:resource-complete") ]
  in
  let span =
    Run.eventually "the span of a gather it answered" 15.0 (fun () ->
        Dynarray.find_opt (fun (s : Otlp.span) -> s.name = complete) otlp.spans)
  in
  Check.that "the span is exported under the Librarian's name"
    (List.assoc_opt "service.name" span.resource
    = Some (Otlp.Text "xynolog-librarian"));
  (* Each way a request of a model ends, the tokens read and written, and each thing a gather went without. *)
  every_value_of otlp "xynolog.inference.calls" "inference.outcome";
  every_value_of otlp "xynolog.inference.duration" "inference.outcome";
  every_value_of otlp "xynolog.inference.tokens" "inference.direction";
  every_value_of otlp "xynolog.gather.degraded" "projection";
  match Telemetry.outside_the_table otlp Librarian (Some w.kb.world.kb.did) with
  | [] -> ()
  | unlisted ->
      failwith (String.concat "\n" (unlisted @ [ Telemetry.received otlp ]))

let tests implementations =
  let world = world_of "telemetry" implementations in
  let otlp = lazy (Otlp.start ()) in
  [
    Testo.create ~category:[ "what a Librarian exports" ]
      ~max_duration:300.0
      "exports, under the Librarian's name, exactly the telemetry the spec \
       lists: every row it can, and nothing else" (fun () ->
        W.end_case world (exports_exactly_the_table (Lazy.force otlp)));
  ]
