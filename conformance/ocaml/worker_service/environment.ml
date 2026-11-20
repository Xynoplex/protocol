(* What a Worker service reads from its environment
   beside its service account and its providers' keys (WORKER-SERVICE.md
   § Environment), each variable by name: what the service does with it, and
   without it. Each case starts a worker with the environment it is about,
   exporting to a receiver of its own. A worker claims as soon as it has
   started, so most cases need no job: the claim it sends, and the answer
   that nothing is pending, are the frames they look for.

   A span is told by what it carries rather than by name: its kind, and the
   channel it is about. *)

open Xynolog_conformance
open Support
open Case

let claim = "job:claim"
let nothing_pending = "job:claim-failed"
let queued = "job:queued"

(* The span kinds the cases name, as OTLP numbers them. *)
let producer = Protocol.otlp_kind Producer
let consumer = Protocol.otlp_kind Consumer

let about channel kind (s : Otlp.span) =
  s.kind = kind
  && List.assoc_opt "bus.channel" s.attributes = Some (Otlp.Text channel)

let sent channel = about channel producer
let got channel = about channel consumer

let span what (otlp : Otlp.t) matches =
  Run.eventually what 10.0 (fun () -> Dynarray.find_opt matches otlp.spans)

let spans_where (otlp : Otlp.t) matches =
  List.filter matches (Dynarray.to_list otlp.spans)

let metric_names (otlp : Otlp.t) =
  strs (List.map fst (Dynarray.to_list otlp.metrics))

let in_output (served : W.served) text =
  List.exists
    (fun line -> Re.execp (Re.compile (Re.str text)) line)
    (Worker_service.output served.worker)

let endpoint (otlp : Otlp.t) =
  ("OTEL_EXPORTER_OTLP_ENDPOINT", Some otlp.endpoint)

(* A worker with [env], exporting to a receiver of its own, once it has
   claimed and been told nothing is pending. *)
let with_env ?engines (w : W.t) env run =
  let otlp = Otlp.start () in
  Fun.protect ~finally:(fun () -> Otlp.close otlp) @@ fun () ->
  let served = W.start ?engines ~env:(env otlp) w in
  ignore (W.emitted served claim : W.emitted);
  run served otlp;
  (* The worker is stopped before its receiver is: an export that finds no
     receiver is retried, and holds the next case up. *)
  ignore (Worker_service.stop served.worker : int option)

(* The variables WORKER-SERVICE.md § Environment names in code. *)
let named_in_the_contract () =
  let document =
    In_channel.with_open_bin "contract/WORKER-SERVICE.md" In_channel.input_all
  in
  let section =
    match
      Re.exec_opt
        (Re.Perl.compile_pat ~opts:[ `Dotall ] {|\n## Environment\n(.*?)\n## |})
        document
    with
    | Some found -> Re.Group.get found 1
    | None -> failwith "WORKER-SERVICE.md has no section Environment"
  in
  List.sort_uniq compare
    (List.map
       (fun found -> Re.Group.get found 1)
       (Re.all
          (Re.Perl.compile_pat {|`([A-Z][A-Z0-9]*(?:_[A-Z0-9]+)+)`|})
          section))

let names_every_variable () =
  (* LOG_LEVEL and LOG_FORMAT are named there as the two it does not read. *)
  Check.equal "the variables WORKER-SERVICE.md § Environment names"
    (strs
       (List.sort compare
          (Protocol.service_environment Worker @ [ "LOG_LEVEL"; "LOG_FORMAT" ])))
    (strs (named_in_the_contract ()))

let service_name (w : W.t) =
  List.iter
    (fun (name, expected) ->
      with_env w
        (fun otlp ->
          [
            endpoint otlp;
            ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
            ("OTEL_SERVICE_NAME", name);
          ])
        (fun _ otlp ->
          let s = span ("the " ^ claim ^ " it sent") otlp (sent claim) in
          Check.equal "the service its span is exported under" (str expected)
            (match List.assoc_opt "service.name" s.resource with
            | Some (Otlp.Text name) -> str name
            | _ -> `Null)))
    [
      (None, "xynolog-worker");
      (Some "conformance-renamed", "conformance-renamed");
    ]

(* The channels a job sends one message of each on, and nothing else. *)
let a_job_sends =
  [
    "job:start";
    "browse:resource-requested";
    "mark:commit";
    "job:checkpoint";
    "job:complete";
  ]

let job_span (otlp : Otlp.t) (job : W.job) =
  span ("the span of " ^ job.id) otlp (fun s ->
      List.assoc_opt "job.id" s.attributes = Some (Otlp.Text job.id))

let in_the_job's_trace (w : W.t) =
  let job = highlighting_job w.own "traced" in
  Ollama.script w.ollama [ proposed [ exact "the first program" ] ];
  with_env w
    (fun otlp -> [ endpoint otlp; ("OTEL_BSP_SCHEDULE_DELAY", Some "100") ])
    (fun served otlp ->
      ignore (settled served job : Yojson.Safe.t);
      let held = job_span otlp job in
      List.iter
        (fun channel ->
          let s = span ("the " ^ channel ^ " it sent") otlp (sent channel) in
          Check.equal
            ("the trace of its " ^ channel)
            (str held.trace_id) (str s.trace_id))
        a_job_sends)

let a_trace_each (w : W.t) =
  let jobs =
    [
      highlighting_job w.own "traced-first";
      highlighting_job w.own "traced-second";
    ]
  in
  Ollama.script w.ollama
    (List.map
       (fun text -> proposed [ exact text ])
       [ "the first program"; "Charles Babbage" ]);
  with_env w
    (fun otlp -> [ endpoint otlp; ("OTEL_BSP_SCHEDULE_DELAY", Some "100") ])
    (fun served otlp ->
      (* One engine runs both, one after the other. *)
      List.iter (fun job -> ignore (settled served job : Yojson.Safe.t)) jobs;
      let traces =
        List.map (fun job -> (job_span otlp job).Otlp.trace_id) jobs
      in
      Check.that "the second job is not in the trace of the first"
        (List.nth traces 1 <> List.nth traces 0);
      (* The traces of the spans [which] finds, once there are [count]. *)
      let traces_of ?(count = 2) what which =
        Run.eventually (Printf.sprintf "the %d %s" count what) 10.0 (fun () ->
            let found = spans_where otlp which in
            if List.length found >= count then Some found else None)
      in
      let sorted_traces spans =
        strs
          (List.sort compare
             (List.map (fun (s : Otlp.span) -> s.trace_id) spans))
      in
      (* Each job sends one of each of these, and nothing else on their
         channels: each job's is in its own trace. *)
      List.iter
        (fun channel ->
          Check.equal
            ("the traces of its " ^ channel)
            (strs (List.sort compare traces))
            (sorted_traces (traces_of (channel ^ " it sent") (sent channel))))
        a_job_sends;
      (* Each job was handed over by one job:claimed, and is run in the
         trace that reply arrived in. *)
      Check.equal "the traces of the job:claimed it received"
        (strs (List.sort compare traces))
        (sorted_traces
           (traces_of "job:claimed it received" (got "job:claimed")));
      (* It claimed when it started and when it settled each job. The
         suite's gateway does not export, so no reply brings a claim's trace
         back to the worker: a claim in a job's trace here was made inside
         that job's span. *)
      let claims = traces_of ~count:3 (claim ^ " it sent") (sent claim) in
      Check.equal "its claims in a job's trace" (`List [])
        (strs
           (List.filter_map
              (fun (s : Otlp.span) ->
                if List.mem s.trace_id traces then Some s.trace_id else None)
              claims)))

let exports_nothing (otlp : Otlp.t) =
  Check.equal "the spans it exported" (`Int 0)
    (`Int (Dynarray.length otlp.spans));
  Check.equal "the metrics it exported" (`List []) (metric_names otlp)

let no_exporter (w : W.t) =
  with_env w
    (fun _ ->
      [
        ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
        ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
      ])
    (fun served otlp ->
      Run.sleep 2.0;
      exports_nothing otlp;
      Check.equal "the spans and metrics it wrote" (`List [])
        (strs
           (List.filter
              (fun line ->
                Re.execp (Re.compile (Re.str claim)) line
                || Re.execp
                     (Re.compile (Re.str "xynolog.process.start_time"))
                     line)
              (Worker_service.output served.worker))))

let sdk_disabled (w : W.t) =
  with_env w
    (fun otlp ->
      [
        endpoint otlp;
        ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
        ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
        ("OTEL_SDK_DISABLED", Some "true");
      ])
    (fun _ otlp ->
      Run.sleep 2.0;
      exports_nothing otlp)

let console_exporter (w : W.t) =
  with_env w
    (fun _ ->
      [
        ("OTEL_CONSOLE_EXPORTER", Some "true");
        ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
      ])
    (fun served _ ->
      Run.eventually
        ("a span about " ^ claim ^ " in the output")
        10.0
        (fun () ->
          if
            List.exists
              (fun line ->
                Re.execp (Re.compile (Re.str claim)) line
                && not (String.starts_with ~prefix:"{" line))
              (Worker_service.output served.worker)
          then Some ()
          else None))

let metrics_to_console (w : W.t) =
  with_env w
    (fun otlp ->
      [
        endpoint otlp;
        ("OTEL_BSP_SCHEDULE_DELAY", Some "100");
        ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
        ("OTEL_METRICS_EXPORTER", Some "console");
      ])
    (fun served otlp ->
      ignore (span ("the " ^ claim ^ " it sent") otlp (sent claim) : Otlp.span);
      let started = Protocol.metric_name "xynolog.process.start_time" in
      Run.eventually (started ^ " in the output") 10.0 (fun () ->
          if in_output served started then Some () else None);
      Check.equal "the metrics it exported" (`List []) (metric_names otlp))

let metric_interval (w : W.t) =
  with_env w
    (fun otlp -> [ endpoint otlp; ("OTEL_METRIC_EXPORT_INTERVAL", Some "500") ])
    (fun _ otlp ->
      Run.eventually "a metric export within 3 s" 3.0 (fun () ->
          if Dynarray.length otlp.metrics > 0 then Some () else None))

let span_delay (w : W.t) =
  with_env w
    ~engines:[ W.entry w (List.hd W.ollama_engines) highlighting_only ]
    (fun otlp -> [ endpoint otlp; ("OTEL_BSP_SCHEDULE_DELAY", Some "100") ])
    (fun _ otlp ->
      (* An announcement the worker hears and does not claim: its filters do
         not take the job. *)
      let unclaimed =
        W.queued w.own "mark"
          (`Assoc
             [
               ("resourceId", str "res-ws-environment-unclaimed");
               ("motivation", str "assessing");
             ])
      in
      W.announce w.own unclaimed;
      ignore
        (Run.eventually (Printf.sprintf "the %s it received, within 2 s" queued)
           2.0 (fun () -> Dynarray.find_opt (got queued) otlp.spans)
          : Otlp.span))

let bus_log (w : W.t) =
  List.iter
    (fun on ->
      with_env w
        (fun _ -> [ ("XYNOLOG_BUS_LOG", if on then Some "1" else None) ])
        (fun served _ ->
          if on then
            List.iter
              (fun line ->
                Run.eventually
                  ("a " ^ line ^ " line")
                  5.0
                  (fun () -> if in_output served line then Some () else None))
              [ "[bus EMIT] " ^ claim; "[bus RECV] " ^ nothing_pending ]
          else (
            Run.sleep 0.5;
            Check.equal "the bus lines it wrote" (`List [])
              (strs
                 (List.filter
                    (fun line -> Re.execp (Re.compile (Re.str "[bus ")) line)
                    (Worker_service.output served.worker))))))
    [ false; true ]

let restarts (w : W.t) =
  let events =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "worker-conformance-supervisor-%s.log" (Fresh.uuid ()))
  in
  let life n =
    Printf.sprintf
      "[supervise 2026-09-30T00:00:0%dZ] starting worker (rapid failures so \
       far: %d)"
      n n
  in
  Out_channel.with_open_bin events (fun out ->
      List.iter (fun n -> output_string out (life n ^ "\n")) [ 0; 1; 2 ]);
  Fun.protect ~finally:(fun () -> Sys.remove events) @@ fun () ->
  let restarts = Protocol.metric_name "xynolog.process.restarts" in
  with_env w
    (fun otlp ->
      [
        endpoint otlp;
        ("OTEL_METRIC_EXPORT_INTERVAL", Some "500");
        ("SUPERVISE_EVENTS", Some events);
        ("SUPERVISE_NAME", Some "worker");
      ])
    (fun _ otlp ->
      let metric =
        Run.eventually restarts 10.0 (fun () ->
            match Otlp.metric otlp restarts with
            | Some m when Dynarray.length m.values > 0 -> Some m
            | _ -> None)
      in
      Check.equal "the restarts it last reported" (`Float 2.0)
        (`Float (Dynarray.get_last metric.values));
      Check.equal "the instruments of its restarts" (strs [ "gauge" ])
        (strs (Dynarray.to_list metric.instruments)));
  (* With one of the two and not the other, the metric does not exist. *)
  List.iter
    (fun half ->
      with_env w
        (fun otlp ->
          [ endpoint otlp; ("OTEL_METRIC_EXPORT_INTERVAL", Some "500"); half ])
        (fun _ otlp ->
          (* Two exports of what it does report, and the restarts in
             neither. *)
          let started = Protocol.metric_name "xynolog.process.start_time" in
          Run.eventually "two metric exports" 10.0 (fun () ->
              match Otlp.metric otlp started with
              | Some m when Dynarray.length m.values >= 2 -> Some ()
              | _ -> None);
          Check.that "it reports no restarts"
            (Option.is_none (Otlp.metric otlp restarts))))
    [ ("SUPERVISE_EVENTS", Some events); ("SUPERVISE_NAME", Some "worker") ]

let reads_no_log_variables (w : W.t) =
  let lines ~level ~format unlisted some =
    let served =
      W.start
        ~document:(function
          | `Assoc members ->
              `Assoc
                (List.map
                   (fun (k, v) ->
                     match k with
                     | "logLevel" -> (k, str level)
                     | "logFormat" -> (k, str format)
                     | _ -> (k, v))
                   members)
          | other -> other)
        ~unlisted w
    in
    ignore (W.emitted served claim : W.emitted);
    if some then
      Run.eventually "a line of its log" 5.0 (fun () ->
          if Worker_service.stdout served.worker <> [] then Some () else None)
    else Run.sleep 0.5;
    ignore (Worker_service.stop served.worker : int option);
    Worker_service.stdout served.worker
  in
  (* Told to log from info as JSON, with an environment that says errors
     only, and plainly: it logs as its document says. *)
  let json =
    lines ~level:"info" ~format:"json"
      [ ("LOG_LEVEL", "error"); ("LOG_FORMAT", "simple") ]
      true
  in
  Check.that "it logged" (json <> []);
  List.iter
    (fun line ->
      Check.that
        (Printf.sprintf "its line is a JSON object: %s" line)
        (match Yojson.Safe.from_string line with
        | `Assoc _ -> true
        | _ -> false
        | exception Yojson.Json_error _ -> false))
    json;
  (* Told to log plainly, with an environment that says JSON: plainly. *)
  let simple =
    lines ~level:"info" ~format:"simple"
      [ ("LOG_LEVEL", "debug"); ("LOG_FORMAT", "json") ]
      true
  in
  Check.that "it logged" (simple <> []);
  List.iter
    (fun line ->
      Check.that
        (Printf.sprintf "its line is plain: %s" line)
        ((not (String.starts_with ~prefix:"{" line))
        && Check.matches_re {| \[[A-Z]+\] |} line))
    simple;
  (* Told to log errors only, with an environment that says everything:
     nothing has gone wrong, and it logs nothing. *)
  Check.equal "what it logged" (`List [])
    (strs
       (lines ~level:"error" ~format:"json"
          [ ("LOG_LEVEL", "debug"); ("LOG_FORMAT", "simple") ]
          false))

let tests implementations =
  Case.test
    ~category:[ "the worker's environment" ]
    (case
       "WORKER-SERVICE.md § Environment names every variable the suite starts \
        a worker with, and no other" (fun () -> ()))
    names_every_variable
  :: in_world implementations "environment"
       ~category:[ "the worker's environment" ]
       [
         case
           "OTEL_SERVICE_NAME names the service its spans are exported under; \
            xynolog-worker when unset"
           service_name;
         case
           "with OTEL_EXPORTER_OTLP_ENDPOINT, every message a job sends is in \
            the trace of the job's span"
           in_the_job's_trace;
         case
           "with OTEL_EXPORTER_OTLP_ENDPOINT, each job an engine runs has a \
            trace of its own: its span continues the trace of the job:claimed \
            that handed it over, the second job is not in the trace of the \
            first, and the claim made when a job is settled is not in that \
            job's trace"
           a_trace_each;
         case
           "without OTEL_EXPORTER_OTLP_ENDPOINT or OTEL_CONSOLE_EXPORTER, \
            exports nothing and writes no span or metric"
           no_exporter;
         case
           "OTEL_SDK_DISABLED=true exports nothing, though an endpoint is set"
           sdk_disabled;
         case
           "OTEL_CONSOLE_EXPORTER=true, with no endpoint, writes its spans to \
            the worker's output"
           console_exporter;
         case
           "OTEL_METRICS_EXPORTER=console writes metrics to the output, and \
            spans still reach the endpoint"
           metrics_to_console;
         case
           "OTEL_METRIC_EXPORT_INTERVAL sets how often metrics are exported \
            (30 s when unset)"
           metric_interval;
         case
           "OTEL_BSP_SCHEDULE_DELAY sets how long a finished span waits to be \
            exported (5 s when unset)"
           span_delay;
         case
           "XYNOLOG_BUS_LOG writes a line for each frame the worker sends and \
            receives; unset, it writes none"
           bus_log;
         case
           "SUPERVISE_EVENTS and SUPERVISE_NAME report \
            xynolog.process.restarts: the lives the supervisor recorded, less \
            one"
           restarts;
         case
           "reads neither LOG_LEVEL nor LOG_FORMAT: how much it logs, and how, \
            are its document's"
           reads_no_log_variables;
       ]
