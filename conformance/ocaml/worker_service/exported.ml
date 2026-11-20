(* telemetry.test.ts, named exported beside the harness's Telemetry: the
   telemetry a worker exports (WORKER-SERVICE.md § Telemetry): everything
   specs/src/service-telemetry/telemetry.json lists for it, and the rows of
   specs/src/sdk-telemetry/telemetry.json it names there, exported over
   OTLP to the endpoint the standard OpenTelemetry environment names; and
   nothing else. What it records of a job names the knowledge base that
   handed the job over. *)

open Xynolog_conformance
open Support
open Case

let job_span = "job:{jobType}"

let listed : Protocol.value -> Yojson.Safe.t = function
  | String s -> str s
  | Boolean b -> `Bool b

(* The values a row lists for its attribute [key]. *)
let values_listed what (attributes : Protocol.attribute list) key =
  match
    List.find_opt (fun (a : Protocol.attribute) -> a.key = key) attributes
  with
  | Some { values = Some values; _ } -> `List (List.map listed values)
  | Some { values = None; _ } ->
      failwith (Printf.sprintf "%s lists no values for %s" what key)
  | None -> failwith (Printf.sprintf "%s lists no attribute %s" what key)

let names_as_the_spec (_ : W.t) =
  let spans, metrics = Protocol.telemetry Worker in
  let schema = Protocol.component_json Gateway in
  let motivations =
    `List
      (List.map
         (fun (motivation, _) -> str motivation)
         (Yojson.Safe.Util.to_assoc
            (Check.at (schema "MarkJobParams") [ "discriminator"; "mapping" ])))
  in
  let job_types = Check.at (schema "JobType") [ "enum" ] in
  let providers =
    Check.at (schema "Engine") [ "properties"; "provider"; "enum" ]
  in
  let span_row name =
    match
      List.find_opt (fun (r : Protocol.span_row) -> r.name = name) spans
    with
    | Some row -> (name, row.span_attributes)
    | None -> failwith ("the worker's table lists no span " ^ name)
  in
  let metric_row name =
    match
      List.find_opt (fun (r : Protocol.metric_row) -> r.metric = name) metrics
    with
    | Some row -> (name, row.metric_attributes)
    | None -> failwith ("the worker's table lists no metric " ^ name)
  in
  List.iter
    (fun (name, attributes) ->
      Check.equal (name ^ "'s job types") job_types
        (values_listed name attributes "job.type");
      Check.equal (name ^ "'s motivations") motivations
        (values_listed name attributes "job.motivation"))
    [
      span_row job_span;
      metric_row "xynolog.job.outcome";
      metric_row "xynolog.job.duration";
    ];
  List.iter
    (fun (name, attributes) ->
      if
        List.exists
          (fun (a : Protocol.attribute) -> a.key = "inference.provider")
          attributes
      then
        Check.equal (name ^ "'s providers") providers
          (values_listed name attributes "inference.provider"))
    (List.map (fun (r : Protocol.span_row) -> (r.name, r.span_attributes)) spans
    @ List.map
        (fun (r : Protocol.metric_row) -> (r.metric, r.metric_attributes))
        metrics)

let source = "res-ws-telemetry-source"
let pdf = "res-ws-telemetry-pdf"

(* A mark job on a PDF the Smelter declined, queued: its text is asked of
   the gateway, and nothing is asked of the model. *)
let pdf_job (w : W.t) =
  W.describe w.own pdf "application/pdf";
  Hashtbl.replace w.own.world.archivist.anchored_texts pdf (fun () ->
      `Assoc [ ("kind", str "declined"); ("declined", str "no-text-layer") ]);
  W.queued w.own "mark"
    (`Assoc [ ("resourceId", str pdf); ("motivation", str "highlighting") ])

(* One job of each motivation, a generation, a job on a PDF, and each way a
   job ends that a case can make: failed by its provider, declined, cut off
   twice, kept though its count says it missed mentions, and cancelled. They
   are queued in this order and run one at a time. *)
let traffic otlp (w : W.t) =
  let job name params = mark_job w.own ("telemetry-" ^ name) params in
  let highlighted = job "highlighting" (motivated "highlighting") in
  let commented = job "commenting" (motivated "commenting") in
  let assessed = job "assessing" (motivated "assessing") in
  let linked = job "linking" (linking [ "Person" ]) in
  let tagged = job "tagging" (tagging (argument_of_claims ()) [ "Claim" ]) in
  let yielded = titled_yield w.own ~source "telemetry" in
  let on_pdf = pdf_job w in
  let failed = job "failed" (motivated "highlighting") in
  let declined = highlighting_job ~about:" \n" w.own "telemetry-declined" in
  let cut_off = job "cut-off" (motivated "highlighting") in
  let under_reported = job "under-reported" (linking [ "Place" ]) in
  let cancelled = job "cancelled" (linking [ "Person"; "Place" ]) in
  (* The last job is cancelled while its first entity type's batch is being
     committed. *)
  w.own.hooks <-
    {
      w.own.hooks with
      commit =
        Some
          (fun commit ->
            if commit.committed_job = str cancelled.id then
              W.emit w.own "job:cancel-requested"
                (`Assoc [ ("jobId", str cancelled.id) ]);
            None);
    };
  let cut = Support.cut_off (proposing [ exact "the first program" ]) in
  Ollama.script w.ollama
    [
      (* Highlighting: the provider reports its token usage, and the four
         spans are found the four ways a span can be. *)
      Ollama.Answer
        {
          response =
            proposing
              [
                exact "the first program";
                `Assoc
                  [
                    ("exact", str "London");
                    ("prefix", str "standards, and ");
                    ("suffix", str " ignored it");
                  ];
                exact "London";
                exact "CHARLES BABBAGE";
              ];
          stop = Done;
          usage = Some (412, 57);
        };
      proposed
        [
          `Assoc
            [
              ("exact", str "Ada Lovelace");
              ("comment", str "She wrote the notes.");
            ];
        ];
      proposed
        [
          `Assoc
            [
              ("exact", str "nobody tested that claim for a century");
              ("assessment", str "No source is given.");
            ];
        ];
      proposed [ mention "Ada Lovelace" "Person" ];
      counting 1;
      proposed [ exact "London was wrong." ];
      answer "The engine was designed in London and never built.";
      Fails (500, {|{"error":"overloaded"}|});
      cut;
      cut;
      (* One place found where the count says nine: the piece cannot be cut
         smaller, so what was found is kept. *)
      proposed [ mention "London" "Place" ~prefix:"engine in " ];
      counting 9;
      (* The cancelled job's first entity type: its second is never asked
         for. *)
      proposed [ mention "Charles Babbage" "Person" ];
      counting 1;
    ];
  let served = W.start ~env:(exporting otlp) w in
  List.iter
    (fun (channel, job) -> ignore (settled ~channel served job : Yojson.Safe.t))
    [
      ("job:complete", highlighted);
      ("job:complete", commented);
      ("job:complete", assessed);
      ("job:complete", linked);
      ("job:complete", tagged);
      ("job:complete", yielded);
      ("job:complete", on_pdf);
      ("job:fail", failed);
      ("job:complete", declined);
      ("job:fail", cut_off);
      ("job:complete", under_reported);
      ("job:cancel", cancelled);
    ];
  (* And one request it answers. *)
  ignore (limits_asked w.own : Bus_stream.message);
  [
    ("highlighting", highlighted);
    ("failed", failed);
    ("linking", linked);
    ("yield", yielded);
  ]

(* What a job metric carried under [key], each value once, in order. *)
let distinct metric key =
  `List (List.map otlp_json (List.sort_uniq compare (values_of metric key)))

let exports_the_table otlp (w : W.t) =
  let jobs = traffic otlp w in
  let did = w.own.world.kb.did in
  (* A span for each job it held, of the kind and with the attributes its row
     lists. *)
  let row =
    match
      List.find_opt
        (fun (r : Protocol.span_row) -> r.name = job_span)
        (fst (Protocol.telemetry Worker))
    with
    | Some row -> row
    | None -> failwith ("the worker's table lists no span " ^ job_span)
  in
  List.iter
    (fun (name, attributes) ->
      let job = List.assoc name jobs in
      let span =
        span_of otlp
          (Protocol.span_name job_span [ ("jobType", job.W.job_type) ])
          job
      in
      Check.equal "the kind of its span" (`Int row.kind) (`Int span.kind);
      Check.equal "the name it exports under" (str "xynolog-worker")
        (match List.assoc_opt "service.name" span.resource with
        | Some value -> otlp_json value
        | None -> `Null);
      Check.equal
        ("what the span of " ^ name ^ " carries")
        (`Assoc
           (List.map (fun (k, v) -> (k, str v)) attributes
           @ [
               ("job.id", str job.id);
               ("resource.id", str job.resource_id);
               ("xynolog.kb", str did);
             ]))
        (`Assoc (List.map (fun (k, v) -> (k, otlp_json v)) span.attributes)))
    [
      ( "highlighting",
        [ ("job.type", "mark"); ("job.motivation", "highlighting") ] );
      ("failed", [ ("job.type", "mark"); ("job.motivation", "highlighting") ]);
      ("linking", [ ("job.type", "mark"); ("job.motivation", "linking") ]);
      ("yield", [ ("job.type", "yield") ]);
    ];
  (* Its jobs counted and timed by how they ended: nine completed, the two
     declined among them, two failed, and one cancelled. *)
  List.iter
    (fun metric ->
      let seen =
        Run.eventually (metric ^ " with every outcome and every motivation")
          15.0 (fun () ->
            match Otlp.metric otlp (Protocol.metric_name metric) with
            | Some got
              when List.map
                     (fun key ->
                       match carried_under got key with
                       | Some values ->
                           List.length (List.sort_uniq compare values)
                       | None -> 0)
                     [ "job.outcome"; "job.motivation"; "job.type" ]
                   = [ 3; 5; 2 ] ->
                Some got
            | _ -> None)
      in
      Check.equal (metric ^ "'s outcomes")
        (strs [ "cancelled"; "completed"; "failed" ])
        (distinct seen "job.outcome");
      Check.equal
        (metric ^ "'s knowledge bases")
        (strs [ did ])
        (distinct seen "xynolog.kb"))
    [ "xynolog.job.outcome"; "xynolog.job.duration" ];
  (* What the provider said it read and wrote, counted as it said it. *)
  Run.eventually "the tokens of the generation that reported them" 15.0
    (fun () ->
      match
        Otlp.metric otlp (Protocol.metric_name "xynolog.inference.tokens")
      with
      | Some got -> (
          match carried_under got "inference.direction" with
          | Some directions
            when List.length (List.sort_uniq compare directions) = 2 ->
              Some ()
          | _ -> None)
      | None -> None);
  match Telemetry.outside_the_table otlp Worker None with
  | [] -> ()
  | unlisted ->
      failwith (String.concat "\n" (unlisted @ [ Telemetry.received otlp ]))

let names_its_knowledge_base otlp (w : W.t) =
  let second = W.second w in
  let here = List.nth W.ollama_engines 0
  and there = List.nth W.ollama_engines 1 in
  let jobs =
    [
      (w.own, highlighting_job w.own "telemetry-here");
      (second, highlighting_job second "telemetry-there");
    ]
  in
  (* The two knowledge bases' engines work at once, so the provider answers
     by which model asked. *)
  answer_by_model w;
  let served =
    W.start
      ~engines:[ W.entry w here highlighting_only ]
      ~serving:[ (second, [ W.entry w there highlighting_only ], None) ]
      ~env:(exporting otlp) w
  in
  List.iter
    (fun ((kb : W.kb), job) ->
      ignore (settled (W.at served kb) job : Yojson.Safe.t);
      let span =
        span_of otlp (Protocol.span_name job_span [ ("jobType", "mark") ]) job
      in
      Check.equal
        ("the knowledge base its span names, at " ^ W.domain kb)
        (str kb.world.kb.did)
        (match List.assoc_opt "xynolog.kb" span.attributes with
        | Some value -> otlp_json value
        | None -> `Null))
    jobs;
  (* Each job's count names its own knowledge base: between them, the two. *)
  List.iter
    (fun metric ->
      let seen =
        Run.eventually (metric ^ " of the second knowledge base") 15.0
          (fun () ->
            match Otlp.metric otlp (Protocol.metric_name metric) with
            | Some got
              when match carried_under got "xynolog.kb" with
                   | Some kbs -> List.mem (Otlp.Text second.world.kb.did) kbs
                   | None -> false ->
                Some got
            | _ -> None)
      in
      Check.equal
        (metric ^ "'s knowledge bases")
        (strs (List.sort compare [ w.own.world.kb.did; second.world.kb.did ]))
        (distinct seen "xynolog.kb"))
    [ "xynolog.job.outcome"; "xynolog.job.duration" ]

(* Each case that exports has a receiver of its own, so what one case's
   worker exported is not judged in another's. *)
let receiving body (w : W.t) =
  let otlp = Otlp.start () in
  Fun.protect ~finally:(fun () -> Otlp.close otlp) (fun () -> body otlp w)

let tests implementations =
  in_world implementations "telemetry"
    ~category:[ "what a worker exports" ]
    [
      case "names jobs and providers in its rows as the spec names them"
        names_as_the_spec;
      (* One case, and judged before the worker is stopped: everything it
         exported for the traffic above, held to the table in both
         directions. *)
      case
        "exports, under the worker's name, exactly the telemetry the spec \
         lists: every row it can, and nothing else"
        (receiving exports_the_table);
      case
        "names, on what it records of each job, the knowledge base of the two \
         it serves that handed the job over"
        (receiving names_its_knowledge_base);
    ]
