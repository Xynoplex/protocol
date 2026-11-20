(* The harness's own cases: that what the suites stand on does what they
   need of it. Each needs nothing but this machine, except where its name
   says which server it reaches. *)

open Xynolog_conformance
open Expect

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

(* A stream whose client leaves while it waits to write more: the request
   is abandoned, and what writes the stream is stopped. *)
let stream_is_abandoned () =
  let seen = ref None and stopped = ref false in
  let never, _ = Eio.Promise.create () in
  let server =
    Serve.start (fun request ->
        seen := Some request;
        Serve.Stream
          {
            status = 200;
            headers = [ ("content-type", "text/event-stream") ];
            events =
              (fun write ->
                Fun.protect
                  ~finally:(fun () -> stopped := true)
                  (fun () ->
                    write "event: one\ndata: 1\n\n";
                    Eio.Promise.await never));
          })
  in
  Fun.protect ~finally:server.stop @@ fun () ->
  (match
     Eio.Time.with_timeout (Run.clock ()) 0.5 (fun () ->
         Ok (raw_get server.port "/stream"))
   with
  | Error `Timeout -> ()
  | Ok text -> failwith ("a stream that never ends ended: " ^ text));
  let request = Run.eventually "the stream's request" 5.0 (fun () -> !seen) in
  ignore
    (Run.eventually "the stream to be abandoned" 5.0 (fun () ->
         if request.abandoned && !stopped then Some () else None))

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

(* ── The client proxy, in front of a stand-in ───────────────────────── *)

let has needle text =
  let n = String.length needle in
  let rec at i =
    i + n <= String.length text && (String.sub text i n = needle || at (i + 1))
  in
  at 0

let port_of (proxy : Client_proxy.t) =
  Option.get (Uri.port (Uri.of_string proxy.origin))

(* [body] run on a proxy in front of a stand-in that answers with
   [handler]. *)
let behind handler body =
  let server = Serve.start handler in
  Fun.protect ~finally:server.stop @@ fun () ->
  let proxy = Client_proxy.start server.origin in
  Fun.protect ~finally:(fun () -> Client_proxy.close proxy) @@ fun () ->
  body proxy

let ok _ =
  Serve.Reply
    {
      Serve.status = 200;
      headers = [ ("content-type", "application/json") ];
      body = {|{"ok":true}|};
    }

let closes proxy (r : Client_proxy.request) =
  Client_proxy.until proxy "its exchange to close" (fun () ->
      if r.closed then Some () else None)

let unanswered_is_empty () =
  behind ok @@ fun proxy ->
  Client_proxy.leave_unanswered proxy (fun _ -> true) 1;
  check "a request left unanswered is answered with no byte"
    (raw_get (port_of proxy) "/" = "");
  let left = Client_proxy.next proxy "the request" (fun _ -> true) in
  closes proxy left;
  check "it records no answer" (left.status = None && left.answered = None);
  check "the next is passed on" ((Call.call proxy.origin "GET" "/").status = 200)

let lost_is_empty () =
  behind ok @@ fun proxy ->
  Client_proxy.lose_answer proxy (fun _ -> true) 1;
  check "a request whose answer is lost is answered with no byte"
    (raw_get (port_of proxy) "/" = "");
  let lost = Client_proxy.next proxy "the request" (fun _ -> true) in
  closes proxy lost;
  check "it records the stand-in's whole answer"
    (lost.status = Some 200 && lost.answer_lost
    && lost.answer = Some (`Assoc [ ("ok", `Bool true) ]));
  check "the next is passed on"
    ((Call.call proxy.origin "GET" "/").status = 200
    && not (Dynarray.get_last proxy.requests).answer_lost)

let events_stream events =
  Serve.Stream
    {
      status = 200;
      headers = [ ("content-type", "text/event-stream") ];
      events;
    }

let rechunked () =
  behind (fun _ ->
      events_stream (fun write ->
          write "event: ping\ndata: {}\n\n";
          write "event: bus-event\nid: e-1\ndata: {\"channel\":\"c\"}\n\n"))
  @@ fun proxy ->
  Client_proxy.rechunk proxy 1;
  let text = raw_get (port_of proxy) "/events" in
  check "each byte is a chunk of its own" (has "\r\n1\r\ne\r\n1\r\nv\r\n" text);
  check "the stream ends with its last chunk" (has "\r\n0\r\n\r\n" text);
  let stream = Client_proxy.next proxy "the stream" (fun _ -> true) in
  closes proxy stream;
  match Dynarray.to_list stream.events with
  | [ ping; frame ] ->
      check "it records each event as the stand-in wrote it"
        (ping.event = "ping" && ping.data = "{}" && ping.id = None
       && frame.event = "bus-event" && frame.id = Some "e-1"
        && frame.data = {|{"channel":"c"}|});
      check "each after the stream was answered, in order"
        (Option.get stream.answered < ping.at && ping.at < frame.at)
  | events ->
      failwith
        (Printf.sprintf "it recorded %d events, not 2" (List.length events))

let cut_is_abrupt () =
  let never, _ = Eio.Promise.create () in
  behind (fun _ ->
      events_stream (fun write ->
          write "event: ping\ndata: {}\n\n";
          Eio.Promise.await never))
  @@ fun proxy ->
  Eio.Switch.run @@ fun sw ->
  let flow =
    Eio.Net.connect ~sw (Run.net ())
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port_of proxy))
  in
  Eio.Flow.copy_string "GET /events HTTP/1.1\r\nhost: x\r\n\r\n" flow;
  let input = Eio.Buf_read.of_flow ~max_size:max_int flow in
  let rec until_event () =
    if Eio.Buf_read.line input <> "event: ping" then until_event ()
  in
  Eio.Time.with_timeout_exn (Run.clock ()) 5.0 until_event;
  let stream = Client_proxy.next proxy "the stream" (fun _ -> true) in
  Client_proxy.cut proxy;
  let rest =
    Eio.Time.with_timeout_exn (Run.clock ()) 5.0 (fun () ->
        match Eio.Buf_read.take_all input with
        | text -> text
        | exception Eio.Io _ -> "")
  in
  check "the cut stream ends with no last chunk"
    (not (has "\r\n0\r\n\r\n" rest));
  closes proxy stream

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
      Testo.create ~category:[ "serve" ] "sees a stream's client leave"
        stream_is_abandoned;
      Testo.create ~category:[ "serve" ]
        "answers a held request when it is released" held_is_answered;
      Testo.create ~category:[ "serve" ] "writes half a body and then nothing"
        partly_answered;
      Testo.create ~category:[ "serve" ]
        "ends a kept-alive connection when it stops" stop_ends_kept_alive;
      Testo.create ~category:[ "client proxy" ]
        "ends a request it leaves unanswered with no byte" unanswered_is_empty;
      Testo.create ~category:[ "client proxy" ]
        "records an answer it keeps from the client, and writes none of it"
        lost_is_empty;
      Testo.create ~category:[ "client proxy" ]
        "passes a stream on a byte at a time, and records each event" rechunked;
      Testo.create ~category:[ "client proxy" ]
        "ends a stream it cuts with no last chunk" cut_is_abrupt;
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
      Testo.create ~category:[ "neo4j" ] ~tags:[ stores ]
        "reads one graph alike over Bolt and over the Query API (neo4j)"
        reads_neo4j_alike;
    ]
  @ Otlp_cases.tests @ Implementations_cases.tests @ Issuer_cases.tests
  @ Net_cases.tests @ Protocol_cases.tests

let () =
  Suite.main ~project:"xynolog-conformance-harness" ~workers:4 ~argv:Sys.argv
    tests
