(* What the weaver files share: a file's world, content to annotate, and the
   ways a knowledge base comes to hold a resource and an annotation, each
   through the Archivist, as a person, a service or a worker records; and
   what the graph is to hold of the record's view. *)

open Xynolog_conformance
module W = Weaver_world
module G = Graph_reader

let str s = `String s

(* [cases] against one Weaver world, under [category]: the file's, made by
   its first case. *)
let in_world implementations file =
  let made =
    Suite.world ("weaver/" ^ file)
      ~make:(fun () -> W.create implementations)
      ~close:W.close
  in
  fun ~category cases ->
    List.map
      (fun (c : W.t Case.t) ->
        Case.test ~category c (fun () -> W.end_case made c.run))
      cases

let text = "The quick brown fox jumps over the lazy dog.\n"

(* Where [exact] begins in [text], in the UTF-16 units a selector counts:
   the text is ASCII, so each is a byte. *)
let start_of exact =
  match Re.exec_opt (Re.compile (Re.str exact)) text with
  | Some group -> Re.Group.start group 0
  | None -> failwith (exact ^ " is not in the text")

(* Upload a resource as a person: its id. *)
let uploaded ?by (w : W.t) name = W.uploaded w ?by name text

(* A target on [exact] in [resource_id]'s text, by where it is and what it
   quotes. *)
let target resource_id exact =
  let start = start_of exact in
  `Assoc
    [
      ("source", str resource_id);
      ( "selector",
        `List
          [
            `Assoc
              [
                ("type", str "TextPositionSelector");
                ("start", `Int start);
                ("end", `Int (start + String.length exact));
              ];
            `Assoc [ ("type", str "TextQuoteSelector"); ("exact", str exact) ];
          ] );
    ]

let marked (who : Archivist_world.client) resource_id motivation exact =
  Yojson.Safe.Util.to_string
    (Check.at
       (Archivist_world.ask who "mark:create-request"
          (`Assoc
             [
               ("resourceId", str resource_id);
               ( "request",
                 `Assoc
                   [
                     ("motivation", str motivation);
                     ("target", target resource_id exact);
                   ] );
             ]))
       [ "annotationId" ])

(* A highlight of [exact], asked for by [who]: its id. *)
let highlighted who resource_id exact =
  marked who resource_id "highlighting" exact

(* A reference on [exact] that links nowhere yet, asked for by [who]: its
   id. *)
let referenced who resource_id exact = marked who resource_id "linking" exact

let tag value =
  `Assoc
    [
      ("type", str "TextualBody");
      ("value", str value);
      ("purpose", str "tagging");
    ]

let link source =
  `Assoc
    [
      ("type", str "SpecificResource");
      ("source", str source);
      ("purpose", str "linking");
    ]

(* Add [items] to an annotation's body, as [who] asks. *)
let bound (who : Archivist_world.client) resource_id annotation_id items =
  ignore
    (Archivist_world.ask who "bind:update-body"
       (`Assoc
          [
            ("resourceId", str resource_id);
            ("annotationId", str annotation_id);
            ( "operations",
              `List
                (List.map
                   (fun item -> `Assoc [ ("op", str "add"); ("item", item) ])
                   items) );
          ])
      : Yojson.Safe.t)

(* An annotation as a worker sends one whole. *)
let whole resource_id exact =
  `Assoc
    [
      ("@context", str "http://www.w3.org/ns/anno.jsonld");
      ("type", str "Annotation");
      ("id", str (Fresh.hex 16));
      ("motivation", str "highlighting");
      ("target", target resource_id exact);
      ("created", str "2026-01-01T00:00:00.000Z");
      ("modified", str "2026-01-01T00:00:00.000Z");
    ]

let id_of annotation = Yojson.Safe.Util.to_string (Check.at annotation [ "id" ])

(* Work [requester] asked [worker] for: the dispatcher records the job as
   the worker's, and the worker commits [annotation] citing it. The worker
   sends the write; the record says who asked. *)
let delegated (w : W.t) (requester : Archivist_world.client)
    (worker : Archivist_world.client) resource_id annotation =
  let job_id = Fresh.job_id () in
  let dispatcher = Archivist_world.sidecar w.kb "dispatcher" in
  Archivist_world.emit dispatcher "job:assign"
    (`Assoc
       [
         ("jobId", str job_id);
         ("jobType", str "mark");
         ("resourceId", str resource_id);
         ("holder", str worker.did);
         ("requester", str requester.did);
       ]);
  ignore
    (Run.eventually "the assignment to be recorded" 10.0 (fun () ->
         List.find_opt
           (fun event ->
             Protocol.at event [ "payload"; "jobId" ] = Some (str job_id))
           (W.recorded w resource_id "job:assigned"))
      : Yojson.Safe.t);
  ignore
    (Archivist_world.ask worker "mark:commit"
       (`Assoc
          [
            ("resourceId", str resource_id);
            ("jobId", str job_id);
            ("annotations", `List [ annotation ]);
          ])
      : Yojson.Safe.t)

(* ── The graph against the record ───────────────────────────────────── *)

let by_id annotations =
  List.sort (fun a b -> compare (id_of a) (id_of b)) annotations

let ids annotations = List.sort compare (List.map id_of annotations)

(* The annotations the record's view holds of a resource. *)
let viewed (w : W.t) resource_id =
  Check.list (W.view w resource_id) [ "annotations"; "annotations" ]

(* Whether the graph holds, for [id], what it is to hold of the record's
   view: its annotations, and the resource's name and whether it is
   archived. *)
let as_the_record_says (w : W.t) id =
  let view = W.view w id in
  Check.equal
    (id ^ "'s annotations in the graph")
    (`List (by_id (List.map Graph_codec.intended (viewed w id))))
    (`List (by_id (G.annotations_on w.graph id)));
  let intended = Graph_codec.intended_resource (Check.at view [ "resource" ]) in
  match G.resource w.graph id with
  | None -> failwith ("the graph holds no resource " ^ id)
  | Some held ->
      Check.matching (id ^ " in the graph")
        [
          ("name", Check.at intended [ "name" ]);
          ("archived", Check.at intended [ "archived" ]);
        ]
        held

(* A reconcile's summary, as the health reports it. *)
let summary health = Check.at health [ "reconcile"; "summary" ]

let count health name =
  Yojson.Safe.Util.to_int (Check.at (summary health) [ name ])
