(* boot.test.ts: how a Librarian starts, answers for itself and stops
   (LIBRARIAN.md § Settings, § Boot, § Health, § Stopping): what its health
   reports, that it stops cleanly, every setting it refuses to start without
   or refuses the value of, that it refuses to start where its settings name
   another knowledge base than the one at its gateway, and that it gives up
   when the gateway does not answer.

   A case that must see a Librarian refuse stops the world's, since both are
   given one health port; a case that needs the world's starts it again. *)

open Xynolog_conformance
open Support

(* A knowledge base the gateway does not serve, which a Librarian's settings
   are made to name. *)
let elsewhere () = Protocol.kb_identity "a comment is not part of the value"

(* What a Librarian says of a member its configuration document lacks: the
   words every service that reads a document refuses one with. *)
let missing member =
  Re.seq [ Re.str ("Missing required property: " ^ member); Re.eow ]

(* Each way a Librarian's settings can be ones it refuses to start on: how,
   the member of its document changed (taken away where no value is given),
   and what its refusal names. *)
let refused_settings () =
  let without how path member = (how, path, None, missing member) in
  [
    without "without the knowledge base it serves" [ "kb" ] "kb";
    without "without the gateway's address" [ "gatewayUrl" ] "gatewayUrl";
    without "without the issuer" [ "identity"; "issuer" ] "issuer";
    without "without the Archivist's address" [ "archivist" ] "archivist";
    without "without a graph database" [ "graph" ] "graph";
    without "without a vector database" [ "vectors" ] "vectors";
    without "without an embedding provider" [ "embedding" ] "embedding";
    without "without the Matcher's model" [ "matcher" ] "matcher";
    without "without the Gatherer's model" [ "gatherer" ] "gatherer";
    (* A store in one process's memory could not be shared with the service
       that writes it. *)
    ( "on a graph that lives in one process's memory",
      [ "graph" ],
      Some (`Assoc [ ("type", `String "memory") ]),
      Re.str "graph.type is memory" );
    ( "on a vector index that lives in one process's memory",
      [ "vectors" ],
      Some (`Assoc [ ("type", `String "memory") ]),
      Re.str "vectors.type is memory" );
    (* A gather's two waits, for the graph and for the vectors, must both end
       before a worker that holds a job is called stalled. *)
    ( "on a settle bound as long as the stall of a held job",
      [ "gather"; "settleTimeoutMs" ],
      Some (`Int (Protocol.client_timing "heldJobStallMs")),
      Re.str "gather.settleTimeoutMs must be less than" );
    ( "on a settle bound of no time at all",
      [ "gather"; "settleTimeoutMs" ],
      Some (`Int 0),
      Re.str "gather.settleTimeoutMs" );
  ]

(* A Librarian launched as [launch] refused to start, and its output holds
   what [named] matches. *)
let refuses ?timeout (w : W.t) launch named =
  let code, output = W.refused ?timeout w launch in
  let output = String.concat "\n" output in
  Check.that ("it exited saying nothing is wrong:\n" ^ output) (code <> Some 0);
  if not (Re.execp (Re.compile named) output) then
    failwith ("its output does not name the setting:\n" ^ output)

let health (w : W.t) =
  W.running_librarian w;
  Check.equal "its health"
    (`Assoc
       [
         ("status", `String "ok");
         ("kb", `String w.kb.world.kb.did);
         ("actors", `List [ `String "matcher"; `String "gatherer" ]);
       ])
    (W.health w)

let stops (w : W.t) =
  W.running_librarian w;
  Check.that "it exits with 0" (W.stop_librarian w = Some 0);
  W.start_librarian w

let refuses_settings (_, path, value, named) (w : W.t) =
  refuses w (W.changed w path value) named

let refuses_without variable (w : W.t) =
  refuses w (W.with_variable w variable None) (Re.str variable)

let refuses_another_kb (w : W.t) =
  let other = elsewhere () in
  let code, output =
    W.refused w
      (W.changed w [ "kb" ]
         (Some
            (`Assoc
               [
                 ("name", `String other.name); ("domain", `String other.domain);
               ])))
  in
  let output = String.concat "\n" output in
  Check.that "it exited saying something is wrong" (code <> Some 0);
  Check.contains_text "its output" w.kb.world.kb.did output;
  Check.contains_text "its output" other.did output

let gives_up (w : W.t) =
  (* A port nothing listens on. A Librarian tries to sign in there a bounded
     number of times, which may be once, then ends. *)
  refuses ~timeout:120.0 w
    (W.changed w [ "gatewayUrl" ] (Some (`String "http://127.0.0.1:9")))
    (Re.str "127.0.0.1:9")

let tests implementations =
  let world = world_of "boot" implementations in
  let category = [ "a Librarian, as a process" ] in
  let refusing =
    category @ [ "started on settings or an environment it cannot run on" ]
  in
  let case ?(category = category) ?(max_duration = 120.0) name body =
    Testo.create ~category ~max_duration name (fun () -> W.end_case world body)
  in
  [
    case
      "answers at its health port for itself, for the knowledge base it serves \
       and for the two actors it runs"
      health;
    case "stops when it is told to, and says nothing is wrong by its exit" stops;
  ]
  @ List.map
      (fun ((how, _, _, _) as refused) ->
        case ~category:refusing
          (Printf.sprintf "refuses to start %s, and names the setting" how)
          (refuses_settings refused))
      (refused_settings ())
  @ List.map
      (fun variable ->
        case ~category:refusing
          (Printf.sprintf
             "refuses to start without %s, half of its service account, and \
              names it"
             variable)
          (refuses_without variable))
      [ "XYNOLOG_OIDC_CLIENT_ID"; "XYNOLOG_OIDC_CLIENT_SECRET" ]
  @ [
      case ~category:refusing
        "refuses to start without the graph database's password, and names the \
         variable its settings name"
        (refuses_without W.graph_password);
      case ~category:refusing
        "refuses to start without its token for the vector index, and names \
         the variable its settings name"
        (refuses_without Qdrant.token_variable);
      case ~category:refusing
        "refuses to start on settings that name another knowledge base than \
         the one at its gateway, and names both"
        refuses_another_kb;
      case ~category:refusing ~max_duration:150.0
        "gives up, and exits saying where it was told the gateway is, when the \
         gateway does not answer"
        gives_up;
    ]
