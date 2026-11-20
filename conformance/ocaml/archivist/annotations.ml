(* Annotations (ARCHIVIST.md § Annotations, § Attribution, § Enrichment): an
   annotation assembled and recorded, a batch committed, bodies changed,
   annotations removed, and what each reads back as. *)

open Xynolog_conformance
open Support
open Case

let text = "The quick brown fox jumps over the lazy dog.\n"

let resource w name =
  A.created w
    (World.person_did w.A.world "ada")
    [
      ("name", name);
      ("storageUri", Printf.sprintf "file://annotated/%s.md" (Fresh.uuid ()));
    ]
    text

let whole ?extra = whole ?extra text

let annotation_id answer =
  match Protocol.at answer [ "annotationId" ] with
  | Some (`String id) -> id
  | _ -> failwith (Check.shown answer ^ " holds no annotationId")

let created c id request =
  annotation_id
    (A.ask c "mark:create-request"
       (`Assoc [ ("resourceId", str id); ("request", request) ]))

let linking request =
  match request with
  | `Assoc members ->
      `Assoc
        (List.map
           (function
             | "motivation", _ -> ("motivation", str "linking") | m -> m)
           members)
  | other -> other

let nth what list n =
  match List.nth_opt list n with
  | Some item -> item
  | None -> failwith (Printf.sprintf "%s holds no item %d" what n)

let last what list =
  match List.rev list with
  | item :: _ -> item
  | [] -> failwith (what ^ " holds nothing")

let view w id =
  match A.view w id with
  | Some view -> view
  | None -> failwith ("no view of " ^ id)

let first_annotation w id =
  nth "the view's annotations"
    (Check.list (view w id) [ "annotations"; "annotations" ])
    0

let listed w id =
  match A.annotations w id with
  | Some listed -> listed
  | None -> failwith ("the annotations of " ^ id ^ " were answered 404")

let ids annotations = `List (List.map (fun a -> str (id_of a)) annotations)

(* The first fact on [c]'s stream of [channel], scoped to [id]. *)
let scoped_fact (c : A.client) what channel id =
  match
    (Bus_stream.next c.stream what (fun m ->
         match m.frame with
         | Some f -> f.channel = channel && f.scope = Some id
         | None -> false))
      .frame
  with
  | Some frame -> frame
  | None -> failwith (what ^ " carried no frame")

let assembled w =
  let id = resource w "Assembled" in
  let grace = A.person ~scopes:[ id ] w "grace" in
  let annotation_id = created grace id (highlight id "quick brown" 4) in
  Check.that
    (annotation_id ^ " is no 32 lowercase hex digits")
    (Check.matches_re "^[0-9a-f]{32}$" annotation_id);
  let added = nth "the stream" (A.stored w id) 1 in
  matching "the added event"
    (`Assoc
       [
         ("type", str "mark:added");
         ("resourceId", str id);
         ("userId", str grace.did);
         ("metadata", `Assoc [ ("sequenceNumber", `Int 2) ]);
       ])
    added;
  let annotation = Check.at added [ "payload"; "annotation" ] in
  let person = `Assoc [ ("@type", str "Person"); ("@id", str grace.did) ] in
  matching "the annotation"
    (`Assoc
       [
         ("@context", annotation_constant "@context");
         ("type", annotation_constant "type");
         ("id", str annotation_id);
         ("motivation", str "highlighting");
         ("target", `Assoc [ ("source", str id) ]);
         ("creator", person);
         ("wasAttributedTo", `List [ person ]);
       ])
    annotation;
  lacks "the annotation" annotation [ "generator" ];
  Check.equal "its created and modified"
    (Check.at annotation [ "created" ])
    (Check.at annotation [ "modified" ]);
  matching "the view"
    (`Assoc
       [
         ("lastSequence", `Int 2);
         ( "annotations",
           `Assoc [ ("version", `Int 2); ("annotations", `List [ annotation ]) ]
         );
       ])
    (view w id);
  (* The published fact carries the annotation as the view holds it, beside
     its payload. *)
  let fact = scoped_fact grace "the scoped fact" "mark:added" id in
  let named = `Assoc [ ("id", str annotation_id) ] in
  matching "the fact"
    (`Assoc
       [
         ("type", str "mark:added");
         ("payload", `Assoc [ ("annotation", named) ]);
         ("annotation", named);
       ])
    fact.payload;
  lacks "the stored line"
    (Yojson.Safe.from_string (nth "the stream's lines" (A.stream_lines w id) 1))
    [ "annotation" ]

let unannotatable w =
  let format = "application/zip" in
  let reply =
    A.upload w
      (World.person_did w.A.world "ada")
      [
        ("name", "Archive");
        ("storageUri", "file://annotated/bundle.zip");
        ("format", format);
      ]
      "PK\003\004"
  in
  match reply.status with
  | 200 ->
      let id = A.string_member "resourceId" (Option.get reply.json) in
      let grace = A.person w "grace" in
      Check.equal "the refusal"
        (str (refusal "not-annotatable" [ ("mediaType", format) ]))
        (str
           (A.refused grace "mark:create-request"
              (`Assoc
                 [ ("resourceId", str id); ("request", highlight id "PK" 0) ])));
      Check.equal "the stream's length" (`Int 1)
        (`Int (List.length (A.stored w id)))
  | status -> (
      (* The media type is not one this knowledge base stores. *)
      let fill = [ ("type", format) ] in
      Check.equal "the upload's status"
        (`Int (http_status "upload-unsupported-media"))
        (`Int status);
      match reply.json with
      | Some body ->
          Check.equal "the upload's body"
            (error_body "upload-unsupported-media" fill)
            body
      | None -> failwith ("the upload's body is not JSON: " ^ reply.text))

let read_back w =
  let id = resource w "Read back" in
  let grace = A.person w "grace" in
  let first = created grace id (highlight id "quick" 4) in
  let second = created grace id (highlight id "lazy dog" 35) in
  let listed = listed w id in
  Check.equal "the listed ids"
    (`List [ str first; str second ])
    (ids (Check.list listed [ "annotations" ]));
  Check.equal "the total" (`Int 2) (Check.at listed [ "total" ]);
  let one =
    A.ask grace "browse:annotation-requested"
      (`Assoc [ ("resourceId", str id); ("annotationId", str second) ])
  in
  matching "the annotation read back"
    (`Assoc
       [
         ("annotation", `Assoc [ ("id", str second) ]);
         ("resource", `Assoc [ ("@id", str id); ("name", str "Read back") ]);
         ("resolvedResource", `Null);
       ])
    one;
  Check.equal "the refusal"
    (str (refusal "annotation-not-found" []))
    (str
       (A.refused grace "browse:annotation-requested"
          (`Assoc
             [
               ("resourceId", str id); ("annotationId", str (String.make 32 'f'));
             ])))

let over_http w =
  let id = resource w "Listed over HTTP" in
  let grace = A.person w "grace" in
  let first = created grace id (highlight id "quick" 4) in
  let second = created grace id (highlight id "lazy dog" 35) in
  let route = "/resources/{id}/annotations" in
  let reply = A.http w ~route "GET" (A.resource_path id "/annotations") in
  Check.equal "its status" (`Int 200) (`Int reply.status);
  Check.equal "its content type" (str "application/json")
    (match Http.Header.get reply.headers "content-type" with
    | Some t -> str t
    | None -> `Null);
  let listed = Option.get reply.json in
  Check.equal "the listed ids"
    (`List [ str first; str second ])
    (ids (Check.list listed [ "annotations" ]));
  Check.equal "the total" (`Int 2) (Check.at listed [ "total" ]);
  let none =
    A.http w ~route "GET" (A.resource_path (String.make 32 'e') "/annotations")
  in
  Check.equal "its status for a resource it does not hold"
    (`Int (http_status "annotations-not-found"))
    (`Int none.status);
  Check.equal "its body for a resource it does not hold"
    (error_body "annotations-not-found" [])
    (Option.get none.json)

let commit c id annotations =
  A.ask c "mark:commit"
    (`Assoc [ ("resourceId", str id); ("annotations", `List annotations) ])

let types events = `List (List.map (fun e -> Check.at e [ "type" ]) events)

let committed w =
  let id = resource w "Committed" in
  let importer = A.person w "importer" in
  let batch = [ whole id "quick"; whole id "brown"; whole id "lazy" ] in
  Check.equal "the first commit"
    (`Assoc [ ("persisted", `Int 3); ("annotationIds", ids batch) ])
    (commit importer id batch);
  Check.equal "the stream"
    (`List
       [
         str "yield:created";
         str "mark:added";
         str "mark:added";
         str "mark:added";
       ])
    (types (A.stored w id));
  let again = commit importer id (batch @ [ whole id "dog" ]) in
  Check.equal "the second commit's persisted" (`Int 4)
    (Check.at again [ "persisted" ]);
  Check.equal "the stream's length" (`Int 5)
    (`Int (List.length (A.stored w id)));
  Check.equal "the view's annotations"
    (`List
       (List.map (fun a -> str (id_of a)) batch
       @ [ nth "the annotation ids" (Check.list again [ "annotationIds" ]) 3 ]))
    (ids (Check.list (view w id) [ "annotations"; "annotations" ]));
  let recorded =
    Check.at (nth "the stream" (A.stored w id) 1) [ "payload"; "annotation" ]
  in
  let importer_named = `Assoc [ ("@id", str importer.did) ] in
  matching "the recorded annotation"
    (match List.hd batch with
    | `Assoc members ->
        `Assoc
          (members
          @ [
              ("creator", importer_named);
              ("wasAttributedTo", `List [ importer_named ]);
            ])
    | other -> other)
    recorded

let forged w =
  let id = resource w "Refused" in
  let importer = A.person w "importer" in
  let forged =
    whole id "quick"
      ~extra:
        [
          ( "creator",
            `Assoc
              [
                ("@type", str "Person");
                ("@id", str (World.person_did w.A.world "someone-else"));
              ] );
        ]
  in
  Check.equal "the refusal"
    (str
       (refusal ~channel:"mark:commit" "annotation-with-creator"
          [ ("annotationId", id_of forged) ]))
    (str
       (A.refused importer "mark:commit"
          (`Assoc [ ("resourceId", str id); ("annotations", `List [ forged ]) ])));
  Check.equal "the stream's length" (`Int 1)
    (`Int (List.length (A.stored w id)))

let removed w =
  let id = resource w "Removed" in
  let grace = A.person w "grace" in
  let annotation_id = created grace id (highlight id "quick" 4) in
  let named = [ ("resourceId", str id); ("annotationId", str annotation_id) ] in
  let history =
    A.ask grace "browse:annotation-history-requested" (`Assoc named)
  in
  Check.equal "the history's events"
    (`List [ str "mark:added" ])
    (types (Check.list history [ "events" ]));
  matching "the history"
    (`Assoc
       [
         ("total", `Int 1);
         ("annotationId", str annotation_id);
         ("resourceId", str id);
       ])
    history;
  Check.equal "the deletion"
    (`Assoc [ ("annotationId", str annotation_id) ])
    (A.ask grace "mark:delete" (`Assoc named));
  matching "the removal"
    (`Assoc
       [
         ("type", str "mark:removed");
         ("userId", str grace.did);
         ("payload", `Assoc [ ("annotationId", str annotation_id) ]);
         ("metadata", `Assoc [ ("sequenceNumber", `Int 3) ]);
       ])
    (nth "the stream" (A.stored w id) 2);
  matching "the view"
    (`Assoc
       [
         ("lastSequence", `Int 3);
         ( "annotations",
           `Assoc [ ("version", `Int 3); ("annotations", `List []) ] );
       ])
    (view w id);
  Check.equal "the annotations over HTTP"
    (`Assoc [ ("annotations", `List []); ("total", `Int 0) ])
    (listed w id)

let update_body c id annotation_id operations =
  A.ask c "bind:update-body"
    (`Assoc
       [
         ("resourceId", str id);
         ("annotationId", str annotation_id);
         ("operations", `List operations);
       ])

let op name members = `Assoc (("op", str name) :: members)

let link_to target =
  `Assoc
    [
      ("type", str "SpecificResource");
      ("source", str target);
      ("purpose", str "linking");
    ]

let tagging value =
  `Assoc
    [
      ("type", str "TextualBody");
      ("value", str value);
      ("purpose", str "tagging");
    ]

let body_changed w =
  let id = resource w "Bound" in
  let target = resource w "Linked to" in
  let grace = A.person ~scopes:[ id ] w "grace" in
  let annotation_id = created grace id (linking (highlight id "fox" 16)) in
  let tag = tagging "Animal" in
  let link = link_to target in
  Check.equal "the update's answer" (`Assoc [])
    (update_body grace id annotation_id
       [
         op "add" [ ("item", tag) ];
         op "add" [ ("item", link) ];
         op "add" [ ("item", tag) ];
       ]);
  Check.equal "the body"
    (`List [ tag; link ])
    (Check.at (first_annotation w id) [ "body" ]);
  let updated = nth "the stream" (A.stored w id) 2 in
  matching "the update"
    (`Assoc
       [
         ("type", str "mark:body-updated");
         ("userId", str grace.did);
         ("payload", `Assoc [ ("annotationId", str annotation_id) ]);
         ("metadata", `Assoc [ ("sequenceNumber", `Int 3) ]);
       ])
    updated;
  Check.equal "the annotation's modified"
    (Check.at updated [ "timestamp" ])
    (Check.at (first_annotation w id) [ "modified" ]);
  (* The fact carries the annotation with its body as it now stands. *)
  let fact = scoped_fact grace "the scoped fact" "mark:body-updated" id in
  matching "the fact"
    (`Assoc
       [
         ( "annotation",
           `Assoc [ ("id", str annotation_id); ("body", `List [ tag; link ]) ]
         );
       ])
    fact.payload;
  let retag = tagging "Mammal" in
  ignore
    (update_body grace id annotation_id
       [
         op "replace" [ ("oldItem", tag); ("newItem", retag) ];
         op "remove" [ ("item", link) ];
       ]
      : Yojson.Safe.t);
  Check.equal "the body" (`List [ retag ])
    (Check.at (first_annotation w id) [ "body" ]);
  (* What the annotation links to reads back with it. *)
  ignore
    (update_body grace id annotation_id [ op "add" [ ("item", link) ] ]
      : Yojson.Safe.t);
  matching "the annotation read back"
    (`Assoc
       [
         ( "resolvedResource",
           `Assoc [ ("@id", str target); ("name", str "Linked to") ] );
       ])
    (A.ask grace "browse:annotation-requested"
       (`Assoc [ ("resourceId", str id); ("annotationId", str annotation_id) ]))

(* The last event of [id]'s stream is the link from [annotation_id] to
   [generated], recorded as [grace], and the annotation's body is that
   link. *)
let linked w (grace : A.client) id annotation_id generated =
  let link = link_to generated in
  matching "the link"
    (`Assoc
       [
         ("type", str "mark:body-updated");
         ("userId", str grace.did);
         ( "payload",
           `Assoc
             [
               ("annotationId", str annotation_id);
               ("operations", `List [ op "add" [ ("item", link) ] ]);
             ] );
       ])
    (last "the stream" (A.stored w id));
  Check.equal "the body" (`List [ link ])
    (Check.at (first_annotation w id) [ "body" ])

let generated_on_upload w =
  let id = resource w "Source" in
  let grace = A.person ~scopes:[ id ] w "grace" in
  let annotation_id = created grace id (linking (highlight id "fox" 16)) in
  let generated =
    A.created w grace.did
      [
        ("name", "About the fox");
        ("storageUri", "file://generated/fox.md");
        ("sourceResourceId", id);
        ("sourceAnnotationId", annotation_id);
        ("generationPrompt", "Tell me about the fox");
      ]
      "Foxes.\n"
  in
  (* The record is read as the answer arrives, with nothing waited for: the
     link is recorded before the answer is sent. *)
  linked w grace id annotation_id generated;
  matching "the generated resource's record"
    (`Assoc
       [
         ( "generatedFrom",
           `Assoc
             [ ("resourceId", str id); ("annotationId", str annotation_id) ] );
         ("generationPrompt", str "Tell me about the fox");
       ])
    (Check.at (nth "the stream" (A.stored w generated) 0) [ "payload" ]);
  matching "the generated resource"
    (`Assoc [ ("wasDerivedFrom", str id) ])
    (Check.at (view w generated) [ "resource" ]);
  (* The link is published as any event is, to its resource's scope. *)
  ignore
    (scoped_fact grace "the link" "mark:body-updated" id : Bus_stream.frame)

let generated_on_create w =
  let id = resource w "Another source" in
  let grace = A.person ~scopes:[ id ] w "grace" in
  let annotation_id = created grace id (linking (highlight id "dog" 40)) in
  let uri = "file://generated/dog.md" in
  let content = "Dogs.\n" in
  A.write w uri content;
  let generated =
    A.string_member "resourceId"
      (A.ask grace "yield:create"
         (`Assoc
            [
              ("name", str "About the dog");
              ("storageUri", str uri);
              ("contentChecksum", str (sha256 content));
              ("byteSize", `Int (String.length content));
              ("format", str "text/markdown");
              ( "generatedFrom",
                `Assoc
                  [
                    ("resourceId", str id); ("annotationId", str annotation_id);
                  ] );
            ]))
  in
  (* As above: read as the reply arrives. *)
  linked w grace id annotation_id generated

let tests implementations =
  with_archivist implementations "an annotation"
    [
      case "is assembled, recorded and attributed to whoever asked" assembled;
      case "is refused on content that cannot be annotated" unannotatable;
      case
        "reads back alone, with its resource, and among its resource's \
         annotations"
        read_back;
      case
        "answers a resource's annotations over HTTP, and 404 for a resource it \
         does not hold"
        over_http;
      case "records a batch once, however often it is sent" committed;
      case "refuses an annotation that says who made it" forged;
      case "is removed, and its history stays in the log" removed;
      case "has its body changed by add, replace and remove, in order"
        body_changed;
      case
        "has linked a generated resource from the annotation it was generated \
         for when it answers the upload"
        generated_on_upload;
      case "has linked a generated resource when it replies to yield:create"
        generated_on_create;
    ]
