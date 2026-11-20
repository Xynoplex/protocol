(* boot.test.ts: how a Weaver starts, answers for itself and stops
   (WEAVER.md § Boot, § Health, § Stopping): what its health reports, what
   it refuses to start without, that it refuses to start where its settings
   name another knowledge base than the one at its gateway, and that a
   gateway it cannot reach ends it. *)

open Xynolog_conformance
open Support

(* A knowledge base the gateway does not serve, which a Weaver's settings
   are made to name. *)
let elsewhere () = Protocol.kb_identity "a comment is not part of the value"

(* The members of a reconcile's summary, as WeaverHealth states them. *)
let summary_members () =
  match Protocol.json_of "weaver/WeaverHealth.json" with
  | Some schema ->
      List.sort compare
        (Yojson.Safe.Util.keys
           (Check.at schema
              [
                "properties"; "reconcile"; "properties"; "summary"; "properties";
              ]))
  | None -> failwith "specs/src/weaver has no WeaverHealth"

let reports_its_health (w : W.t) =
  let id = uploaded w "Counted" in
  W.caught_up w [ id ];
  W.restart w;
  let health = W.health w in
  Check.equal "its status" (str "ok") (Check.at health [ "status" ]);
  Check.equal "the knowledge base it serves" (str w.kb.world.kb.did)
    (Check.at health [ "kb" ]);
  Check.equal "how it follows the record"
    (`Assoc
       [
         ("sweep", str "done");
         ("stopped", `Int 0);
         ("refused", `Int 0);
         ("streamDrops", `Int 0);
       ])
    (Check.at health [ "following" ]);
  Check.equal "how its reconcile ended" (str "done")
    (Check.at health [ "reconcile"; "phase" ]);
  Check.equal "what its reconcile's summary counts"
    (`List (List.map str (summary_members ())))
    (`List
       (List.map str
          (List.sort compare (Yojson.Safe.Util.keys (summary health)))));
  Check.that "its reconcile checked at least one resource"
    (count health "resourcesChecked" >= 1)

let stops_cleanly (w : W.t) =
  Check.that "it exits with 0" (W.stop w = Some 0);
  W.start w

(* A Weaver launched as [launch] refuses to start: not with 0, and its
   output says each of [said]. The world's Weaver is started again
   after. *)
let refuses ?timeout (w : W.t) launch said =
  ignore (W.stop w : int option);
  Fun.protect
    ~finally:(fun () -> W.start w)
    (fun () ->
      let code, output = W.refused ?timeout w launch in
      Check.that "it does not exit with 0" (code <> Some 0);
      List.iter (fun s -> s output) said)

let contains needle output = Check.contains_text "its output" needle output

let matches pattern output =
  Check.that
    (Printf.sprintf "its output matches /%s/:\n%s" pattern output)
    (Check.matches_re pattern output)

let tests implementations =
  in_world implementations "boot"
    ~category:[ "a Weaver, as a process" ]
    [
      Case.case
        "reports at its health port the knowledge base it serves, how it \
         follows the record and how its reconcile ended"
        reports_its_health;
      Case.case
        "stops when it is told to, and says nothing is wrong by its exit"
        stops_cleanly;
      Case.case
        "refuses to start without its service account, and names what is \
         missing" (fun w ->
          refuses w
            (W.with_variable w "XYNOLOG_OIDC_CLIENT_SECRET" None)
            [ contains "XYNOLOG_OIDC_CLIENT_SECRET" ]);
      Case.case
        "refuses to start without the graph database's password, and names the \
         variable its settings name" (fun w ->
          refuses w
            (W.with_variable w W.graph_password None)
            [ contains W.graph_password ]);
      Case.case
        "refuses a graph that lives in one process's memory: it could not be \
         shared with a reader" (fun w ->
          refuses w
            (W.changed w [ "graph" ] (`Assoc [ ("type", str "memory") ]))
            [ matches "memory" ]);
      Case.case
        "refuses to start on settings that name another knowledge base than \
         the one at its gateway, and names both" (fun w ->
          let other = elsewhere () in
          refuses w
            (W.changed w [ "kb" ]
               (`Assoc
                  [ ("name", str other.name); ("domain", str other.domain) ]))
            [ contains w.kb.world.kb.did; contains other.did ]);
      Case.case ~timeout:120.0
        "gives up, and exits saying why, when the gateway it is told of does \
         not answer" (fun w ->
          (* A port nothing listens on. A Weaver tries again a bounded number
             of times, then ends. *)
          refuses ~timeout:110.0 w
            (W.changed w [ "gatewayUrl" ] (str "http://127.0.0.1:9"))
            [ matches {|127\.0\.0\.1:9|} ]);
    ]
