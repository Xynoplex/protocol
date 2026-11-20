(* The harness's own cases: that what the suites stand on does what they
   need of it. Each needs nothing but this machine, except where its name
   says which server it reaches. *)

open Xynolog_conformance

let check what condition = if not condition then failwith what

let raw_get port path =
  (* A client of the harness's own, on a bare socket, so a case sees every
     byte a server sent and when the connection ended. *)
  Eio.Switch.run @@ fun sw ->
  let flow =
    Eio.Net.connect ~sw (Run.net ()) (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Eio.Flow.copy_string
    (Printf.sprintf "GET %s HTTP/1.1\r\nhost: x\r\n\r\n" path)
    flow;
  let buffer = Buffer.create 256 in
  let chunk = Cstruct.create 4096 in
  let rec read () =
    match Eio.Flow.single_read flow chunk with
    | n ->
        Buffer.add_string buffer (Cstruct.to_string ~len:n chunk);
        read ()
    | exception (End_of_file | Eio.Io _) -> ()
  in
  read ();
  Buffer.contents buffer

let sse_flushes () =
  let second, said = Eio.Promise.create () in
  let server =
    Serve.start (fun _ ->
        Serve.Stream
          {
            status = 200;
            headers = [ ("content-type", "text/event-stream") ];
            events =
              (fun write ->
                write "event: one\ndata: 1\n\n";
                (* The second is written only once the client has read the first. *)
                Eio.Promise.await second;
                write "event: two\ndata: 2\n\n");
          })
  in
  Fun.protect ~finally:server.stop @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let response, next = Call.exchange ~sw "GET" (server.origin ^ "/events") in
  check "the stream answers 200" (Http.Response.status response = `OK);
  let chunk () =
    match next () with
    | Cohttp.Transfer.Chunk text | Final_chunk text -> text
    | Done -> failwith "the stream ended"
  in
  Eio.Time.with_timeout_exn (Run.clock ()) 5.0 (fun () ->
      check "the first event arrives before the second is written"
        (chunk () = "event: one\ndata: 1\n\n"));
  Eio.Promise.resolve said ();
  check "the second event follows" (chunk () = "event: two\ndata: 2\n\n")

(* One chunk larger than any read buffer, and one body of a stated length
   as large: each arrives whole. cohttp-eio 6.3.0's client corrupts the
   first; the harness's client is not that. *)
let reads_large_bodies () =
  let large =
    String.init 100_000 (fun at -> Char.chr (Char.code 'a' + (at mod 26)))
  in
  let server =
    Serve.start (fun request ->
        match request.path with
        | "/chunked" ->
            Serve.Stream
              {
                status = 200;
                headers = [];
                events = (fun write -> write large);
              }
        | _ -> Serve.Reply { Serve.status = 200; headers = []; body = large })
  in
  Fun.protect ~finally:server.stop @@ fun () ->
  check "a chunk of 100 kB arrives whole"
    ((Call.call server.origin "GET" "/chunked").text = large);
  check "a body of 100 kB arrives whole"
    ((Call.call server.origin "GET" "/sized").text = large)

let dropped_is_empty () =
  let server = Serve.start (fun _ -> Serve.Dropped) in
  Fun.protect ~finally:server.stop @@ fun () ->
  check "a dropped request is answered with no byte"
    (raw_get server.port "/" = "");
  match Call.call server.origin "GET" "/" with
  | _ -> failwith "the client read an answer from a dropped connection"
  | exception (Eio.Io _ | End_of_file | Failure _) -> ()

let held_is_abandoned () =
  let seen = ref None in
  let never, _ = Eio.Promise.create () in
  let server =
    Serve.start (fun request ->
        seen := Some request;
        Serve.Held never)
  in
  Fun.protect ~finally:server.stop @@ fun () ->
  (match
     Eio.Time.with_timeout (Run.clock ()) 0.5 (fun () ->
         Ok (raw_get server.port "/held"))
   with
  | Error `Timeout -> ()
  | Ok text -> failwith ("a held request was answered: " ^ text));
  let request = Run.eventually "the held request" 5.0 (fun () -> !seen) in
  ignore
    (Run.eventually "the held request to be abandoned" 5.0 (fun () ->
         if request.abandoned then Some () else None))

let held_is_answered () =
  let answer, answered = Eio.Promise.create () in
  let server = Serve.start (fun _ -> Serve.Held answer) in
  Fun.protect ~finally:server.stop @@ fun () ->
  Eio.Fiber.both
    (fun () ->
      let reply = Call.call server.origin "GET" "/held" in
      check "a released request is answered"
        (reply.status = 201 && reply.text = "late"))
    (fun () ->
      Run.sleep 0.2;
      Eio.Promise.resolve answered
        (Some { Serve.status = 201; headers = []; body = "late" }))

let partly_answered () =
  let server =
    Serve.start (fun _ ->
        Serve.Partly
          ({ Serve.status = 200; headers = []; body = "0123456789" }, 5))
  in
  Fun.protect ~finally:server.stop @@ fun () ->
  match
    Eio.Time.with_timeout (Run.clock ()) 0.5 (fun () ->
        Ok (raw_get server.port "/"))
  with
  | Error `Timeout -> ()
  | Ok text -> failwith ("the connection ended: " ^ text)

let stop_ends_kept_alive () =
  let server =
    Serve.start (fun _ ->
        Serve.Reply { Serve.status = 200; headers = []; body = "ok" })
  in
  Eio.Switch.run @@ fun sw ->
  let flow =
    Eio.Net.connect ~sw (Run.net ())
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, server.port))
  in
  Eio.Flow.copy_string "GET / HTTP/1.1\r\nhost: x\r\n\r\n" flow;
  let input = Eio.Buf_read.of_flow ~max_size:max_int flow in
  check "answered" (Eio.Buf_read.line input = "HTTP/1.1 200 OK");
  server.stop ();
  Eio.Time.with_timeout_exn (Run.clock ()) 5.0 (fun () ->
      match Eio.Buf_read.take_all input with
      | _ -> ()
      | exception Eio.Io _ -> ())

let outlives_its_bound () = Run.sleep 30.0

(* ── OTLP ───────────────────────────────────────────────────────────── *)

module Proto = Opentelemetry_proto

let gzip plain =
  let input = De.bigstring_create De.io_buffer_size
  and output = De.bigstring_create De.io_buffer_size in
  let compressed = Buffer.create 256 in
  let read = ref 0 in
  let refill buffer =
    let length = min (String.length plain - !read) De.io_buffer_size in
    for at = 0 to length - 1 do
      Bigarray.Array1.set buffer at plain.[!read + at]
    done;
    read := !read + length;
    length
  in
  let flush buffer length =
    for at = 0 to length - 1 do
      Buffer.add_char compressed (Bigarray.Array1.get buffer at)
    done
  in
  Gz.Higher.compress
    ~w:(De.Lz77.make_window ~bits:15)
    ~q:(De.Queue.create 0x1000) ~level:4 ~refill ~flush ()
    (Gz.Higher.configuration Gz.Unix (fun () -> 0l))
    input output;
  Buffer.contents compressed

let attribute key value = Proto.Common.make_key_value ~key ~value ()

let service =
  Proto.Resource.make_resource
    ~attributes:[ attribute "service.name" (String_value "standin") ]
    ()

let traces () =
  let span =
    Proto.Trace.make_span
      ~trace_id:(Bytes.of_string "0123456789abcdef")
      ~span_id:(Bytes.of_string "01234567")
      ~name:"bus.emit:gather:complete" ~kind:Span_kind_producer
      ~attributes:
        [
          attribute "bus.channel" (String_value "gather:complete");
          attribute "retry" (Bool_value false);
        ]
      ()
  in
  let request =
    Proto.Trace_service.make_export_trace_service_request
      ~resource_spans:
        [
          Proto.Trace.make_resource_spans ~resource:service
            ~scope_spans:[ Proto.Trace.make_scope_spans ~spans:[ span ] () ]
            ();
        ]
      ()
  in
  let encoder = Pbrt.Encoder.create () in
  Proto.Trace_service.encode_pb_export_trace_service_request request encoder;
  Pbrt.Encoder.to_string encoder

let metrics () =
  let point n outcome =
    Proto.Metrics.make_number_data_point ~value:(As_int n)
      ~attributes:[ attribute "inference.outcome" (String_value outcome) ]
      ()
  in
  let request =
    Proto.Metrics_service.make_export_metrics_service_request
      ~resource_metrics:
        [
          Proto.Metrics.make_resource_metrics ~resource:service
            ~scope_metrics:
              [
                Proto.Metrics.make_scope_metrics
                  ~metrics:
                    [
                      Proto.Metrics.make_metric ~name:"xynolog.inference.calls"
                        ~data:
                          (Sum
                             (Proto.Metrics.make_sum ~is_monotonic:true
                                ~data_points:
                                  [ point 2L "answered"; point 1L "failed" ]
                                ()))
                        ();
                    ]
                  ();
              ]
            ();
        ]
      ()
  in
  let encoder = Pbrt.Encoder.create () in
  Proto.Metrics_service.encode_pb_export_metrics_service_request request encoder;
  Pbrt.Encoder.to_string encoder

let exported (otlp : Otlp.t) path headers body =
  let reply = Call.call ~headers ~body otlp.endpoint "POST" path in
  check (Printf.sprintf "%s is answered 200" path) (reply.status = 200)

let reads_otlp () =
  let otlp = Otlp.start () in
  Fun.protect ~finally:(fun () -> Otlp.close otlp) @@ fun () ->
  let protobuf = ("content-type", "application/x-protobuf") in
  exported otlp "/v1/traces"
    [ protobuf; ("content-encoding", "gzip") ]
    (gzip (traces ()));
  exported otlp "/v1/metrics" [ protobuf ] (metrics ());
  exported otlp "/v1/traces"
    [ ("content-type", "application/json") ]
    {|{"resourceSpans":[{"resource":{"attributes":[{"key":"service.name","value":{"stringValue":"standin"}}]},
      "scopeSpans":[{"spans":[{"name":"gather.resource","kind":1,"traceId":"00","attributes":[{"key":"n","value":{"intValue":"7"}}]}]}]}]}|};
  check
    ("nothing is unreadable: "
    ^ String.concat "; " (Dynarray.to_list otlp.unreadable))
    (Dynarray.length otlp.unreadable = 0);
  match Dynarray.to_list otlp.spans with
  | [ gzipped; json ] -> (
      check "the gzipped protobuf span's name"
        (gzipped.name = "bus.emit:gather:complete");
      check "its kind is a producer's" (gzipped.kind = 4);
      check "its resource"
        (gzipped.resource = [ ("service.name", Otlp.Text "standin") ]);
      check "its attributes, each as it was carried"
        (gzipped.attributes
        = [
            ("bus.channel", Otlp.Text "gather:complete");
            ("retry", Otlp.Flag false);
          ]);
      check "its trace id, as hex"
        (gzipped.trace_id = Ohex.encode "0123456789abcdef");
      check "the JSON span's int64, carried as a string"
        (json.attributes = [ ("n", Otlp.Number 7.0) ]);
      match Otlp.metric otlp "xynolog.inference.calls" with
      | None -> failwith "the metric did not arrive"
      | Some metric ->
          check "a monotonic sum is a counter"
            (Dynarray.to_list metric.instruments = [ "counter" ]);
          check "its points' values"
            (Dynarray.to_list metric.values = [ 2.0; 1.0 ]);
          check "every outcome it carried"
            (List.map
               (fun (k, vs) -> (k, Dynarray.to_list vs))
               (Dynarray.to_list metric.attributes)
            = [ ("inference.outcome", [ Otlp.Text "answered"; Text "failed" ]) ]
            ))
  | spans ->
      failwith (Printf.sprintf "%d spans arrived, not 2" (List.length spans))

(* ── Neo4j ──────────────────────────────────────────────────────────── *)

let graph_of_suite () =
  match (Sys.getenv_opt "NEO4J_HOST", Sys.getenv_opt "NEO4J_PASSWORD") with
  | Some host, Some password ->
      Neo4j.connect ~host ~password
        ~database:
          (Neo4j.database_of (Protocol.kb_identity Protocol.suite_kb).did)
  | _ ->
      failwith
        "this case reads the librarian suite's neo4j: NEO4J_HOST and \
         NEO4J_PASSWORD name it"

let reads_neo4j_alike () =
  let graph = graph_of_suite () in
  let label = "Probe_" ^ Fresh.hex 6 in
  Fun.protect ~finally:(fun () ->
      Neo4j.run graph (Printf.sprintf "MATCH (n:%s) DETACH DELETE n" label);
      Neo4j.close graph)
  @@ fun () ->
  Neo4j.run graph
    ~parameters:[ ("name", `String "a fox") ]
    (Printf.sprintf
       "CREATE (a:%s {name: $name, n: 3, f: 1.5, tags: ['x', 'y'], ok: \
        true})-[:LINKS {weight: 2}]->(b:%s {name: 'a dog'})"
       label label);
  match
    Neo4j.agreed graph
      (Printf.sprintf
         "MATCH (a:%s)-[r]->(b) RETURN a, r, b, a.n AS n, a.tags AS tags, {at: \
          a.f} AS m"
         label)
  with
  | [ [ `Assoc a; `Assoc r; `Assoc _; n; tags; _ ] ] ->
      check "the node carries its labels"
        (List.assoc "labels" a = `List [ `String label ]);
      check "the relationship its type" (List.assoc "type" r = `String "LINKS");
      check "an integer" (n = `Intlit "3");
      check "a list" (tags = `List [ `String "x"; `String "y" ])
  | records ->
      failwith
        (Printf.sprintf "%d records, not one of six values"
           (List.length records))

(* A git checkout of its own, made fresh, for [run]. *)
let in_checkout run =
  let root = Filename.temp_dir "conformance-git-" "" in
  Fun.protect
    ~finally:(fun () -> Gateway.remove root)
    (fun () ->
      Result.iter_error
        (fun err -> failwith ("git init failed: " ^ err))
        (Archivist_world.git_in root [ "init"; "--quiet" ]);
      run root)

let index_lock root = Filename.concat (Filename.concat root ".git") "index.lock"

(* The subject of each branch's last commit: none before the first commit. *)
let branch_tips root =
  match
    Archivist_world.git_in root
      [ "for-each-ref"; "--format=%(contents:subject)"; "refs/heads/" ]
  with
  | Ok out -> List.filter (fun l -> l <> "") (String.split_on_char '\n' out)
  | Error err -> failwith ("git for-each-ref failed: " ^ err)

let runs_once_unlocked () =
  in_checkout @@ fun root ->
  let lock = index_lock root in
  Out_channel.with_open_bin lock ignore;
  Eio.Switch.run @@ fun sw ->
  let run =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Archivist_world.git_by_hand root
          [ "commit"; "--quiet"; "--allow-empty"; "-m"; "once unlocked" ])
  in
  Run.sleep 0.3;
  check "it has not run while the index is locked" (Eio.Promise.peek run = None);
  check "nothing is committed while the index is locked" (branch_tips root = []);
  Sys.remove lock;
  ignore (Eio.Promise.await_exn run : string);
  check "it ran once the lock was gone" (branch_tips root = [ "once unlocked" ])

let fails_at_once () =
  in_checkout @@ fun root ->
  match
    Archivist_world.git_by_hand root [ "checkout"; "--quiet"; "no-such-branch" ]
  with
  | _ -> failwith "git checkout of no branch succeeded"
  | exception Failure said ->
      check
        ("it fails with what git said, not " ^ said)
        (Check.matches_re "pathspec 'no-such-branch' did not match" said)

let waits_for_a_writer () =
  in_checkout @@ fun root ->
  let lock = index_lock root in
  let in_flight = "an index still being written" in
  Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_excl ] 0o644 lock
    (fun out -> output_string out in_flight);
  Eio.Switch.run @@ fun sw ->
  let pending =
    Eio.Fiber.fork_promise ~sw (fun () -> Archivist_world.hold_git_index root)
  in
  Run.sleep 0.075;
  check "it holds nothing while another writer does"
    (Eio.Promise.peek pending = None);
  check "the writer's lock is neither truncated nor removed"
    (In_channel.with_open_bin lock In_channel.input_all = in_flight);
  Sys.remove lock;
  let release = Eio.Promise.await_exn pending in
  check "it holds the lock" (Sys.file_exists lock);
  release ();
  check "its release removes the lock" (not (Sys.file_exists lock))

let fails_without_an_index () =
  in_checkout @@ fun root ->
  match Archivist_world.hold_git_index (Filename.concat root "missing") with
  | (_ : unit -> unit) ->
      failwith "it held an index in a directory that is not there"
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

(* A case that reads the stores the librarian suite reads. The run make
   validate makes leaves it out: [-t "not stores"]. *)
let stores = Testo.Tag.declare "stores"

let tests _env =
  List.map
    (fun (table, gate) ->
      Testo.create ~category:[ "census" ] ("holds the harness to " ^ table) gate)
    Census.gates
  @ [
      Testo.create ~category:[ "serve" ]
        "flushes each event of a stream as it is written" sse_flushes;
      Testo.create ~category:[ "call" ]
        "reads a body whole, however large its chunks" reads_large_bodies;
      Testo.create ~category:[ "serve" ]
        "ends a dropped request's connection with no byte" dropped_is_empty;
      Testo.create ~category:[ "serve" ] "sees a held request's client leave"
        held_is_abandoned;
      Testo.create ~category:[ "serve" ]
        "answers a held request when it is released" held_is_answered;
      Testo.create ~category:[ "serve" ] "writes half a body and then nothing"
        partly_answered;
      Testo.create ~category:[ "serve" ]
        "ends a kept-alive connection when it stops" stop_ends_kept_alive;
      Testo.create ~category:[ "runner" ] ~max_duration:2.0
        ~expected_outcome:(Should_fail "it sleeps past its bound")
        "is stopped when it outlives its bound" outlives_its_bound;
      Testo.create ~category:[ "git run by hand" ]
        "runs once the lock on the index is gone, and not before"
        runs_once_unlocked;
      Testo.create ~category:[ "git run by hand" ]
        "fails at once, with what git said, for anything but a locked index"
        fails_at_once;
      Testo.create
        ~category:[ "holding the git index" ]
        "waits for an existing writer without truncating or removing its lock"
        waits_for_a_writer;
      Testo.create
        ~category:[ "holding the git index" ]
        "fails immediately when the lock cannot be created for another reason"
        fails_without_an_index;
      Testo.create ~category:[ "otlp" ]
        "reads an export as protobuf, gzipped or not, and as JSON" reads_otlp;
      Testo.create ~category:[ "neo4j" ] ~tags:[ stores ]
        "reads one graph alike over Bolt and over the Query API (neo4j)"
        reads_neo4j_alike;
    ]

let () =
  Suite.main ~project:"xynolog-conformance-harness" ~workers:4 ~argv:Sys.argv
    tests
