(* Admission (JOBS.md § job:create): what the gateway refuses at its door
   because it is not a job description, the checks the dispatcher makes and
   their refusals, the resource a job is recorded under, the two vocabulary
   reads, and the record a claim then hands out. *)

open Xynolog_conformance
open Support
open Case

let strings items = `List (List.map str items)
let both_reads w = D.reads w D.tag_schemas_read + D.reads w D.entity_types_read

let records_requester w =
  let creator = D.person w "creator" in
  let worker = D.worker w "creation" in
  let mark_id = mark creator [ motivation "commenting" ] in
  let marked = D.claimed worker [ D.marks "commenting" ] in
  Check.matching "the mark job's metadata"
    [
      ("id", str mark_id);
      ("type", str "mark");
      ("userId", str creator.did);
      ("retryCount", `Int 0);
      ("maxRetries", max_retries "mark");
    ]
    (Check.at marked [ "metadata" ]);
  is_time "the mark job's creation" marked [ "metadata"; "created" ];
  let yield_id = D.created creator "yield" (D.generation (D.resource_id ())) in
  let yielded = D.claimed worker [ D.yields ] in
  Check.matching "the yield job's metadata"
    [
      ("id", str yield_id);
      ("type", str "yield");
      ("userId", str creator.did);
      ("retryCount", `Int 0);
      ("maxRetries", max_retries "yield");
    ]
    (Check.at yielded [ "metadata" ]);
  Check.that "the two jobs' ids differ" (yield_id <> mark_id);
  Check.equal "the mark job's status's userId" (str creator.did)
    (Check.at (D.status_of creator mark_id) [ "userId" ]);
  Check.equal "the yield job's assignment's requester" (str creator.did)
    (Check.at (D.assigned w yield_id).payload [ "requester" ])

(* A mark job, and a parameter beside its own that it does not take. *)
let not_taken =
  [
    ( "a linking job given instructions",
      [ motivation "linking"; ("entityTypes", strings [ "Person" ]) ],
      ("instructions", str "find who is related to whom") );
    ( "a tagging job given a density",
      [
        motivation "tagging";
        ("schemaId", str "irac");
        ("categories", strings [ "Issue" ]);
      ],
      ("density", `Int 2) );
    ( "a highlighting job given a tone",
      [ motivation "highlighting" ],
      ("tone", str "scholarly") );
    ( "a commenting job given an assessment's tone",
      [ motivation "commenting" ],
      ("tone", str "critical") );
    ( "an assessing job given entity types",
      [ motivation "assessing" ],
      ("entityTypes", strings [ "Person" ]) );
    ( "a job given the resource among its parameters",
      [ motivation "highlighting" ],
      ("resourceId", str "res-caller") );
  ]

let refuses_not_taken (params, extra) w =
  let creator = D.person w "creator" in
  let worker = D.worker w "taker" in
  let resource_id = D.resource_id () in
  refused_at_door creator "job:create"
    (`Assoc
       [
         ("jobType", str "mark");
         ("resourceId", str resource_id);
         ("params", `Assoc (params @ [ extra ]));
       ]);
  nothing_queued w;
  (* The same job without it is one. *)
  let job_id = mark ~resource_id creator params in
  let m = D.text (`Assoc params) [ "motivation" ] in
  Check.equal "the job claimed" (str job_id)
    (str (D.id_of (D.claimed worker [ D.marks m ])))

let not_stated =
  [
    ( "a tagging job with an empty schemaId",
      [
        motivation "tagging";
        ("schemaId", str "");
        ("categories", strings [ "Issue" ]);
      ] );
    ( "a tagging job with no category",
      [
        motivation "tagging"; ("schemaId", str "irac"); ("categories", `List []);
      ] );
    ( "a tagging job with no schemaId",
      [ motivation "tagging"; ("categories", strings [ "Issue" ]) ] );
    ( "a linking job with no entity type",
      [ motivation "linking"; ("entityTypes", `List []) ] );
    ("a linking job that names no entity types", [ motivation "linking" ]);
    ("a job of no motivation", []);
    ("a job of a motivation no job has", [ motivation "bookmarking" ]);
  ]

let refuses_not_stated params w =
  let creator = D.person w "creator" in
  let before = both_reads w in
  refused_at_door creator "job:create"
    (`Assoc
       [
         ("jobType", str "mark");
         ("resourceId", str (D.resource_id ()));
         ("params", `Assoc params);
       ]);
  Check.equal "the vocabulary reads" (`Int before) (`Int (both_reads w));
  nothing_queued w

let refuses_no_resource w =
  let creator = D.person w "creator" in
  refused_at_door creator "job:create"
    (`Assoc
       [
         ("jobType", str "mark");
         ("params", `Assoc [ motivation "highlighting" ]);
       ]);
  nothing_queued w

let yield_under_focus w =
  let creator = D.person w "creator" in
  let worker = D.worker w "generator" in
  let resource_id = D.resource_id () in
  let job_id = D.created creator "yield" (D.generation resource_id) in
  Check.equal "the announcement's resource" (str resource_id)
    (Check.at (D.announced w job_id).payload [ "resourceId" ]);
  let job = D.claimed worker [ D.yields ] in
  Check.equal "the job's resource" (str resource_id)
    (Check.at job [ "params"; "resourceId" ]);
  Check.equal "the assignment's resource" (str resource_id)
    (Check.at (D.assigned w job_id).payload [ "resourceId" ])

let yield_under_annotation w =
  let creator = D.person w "creator" in
  let worker = D.worker w "generator" in
  let source_resource = D.resource_id () in
  let annotation =
    `Assoc
      [
        ("@context", str "http://www.w3.org/ns/anno.jsonld");
        ("type", str "Annotation");
        ("id", str ("ann-" ^ Fresh.uuid ()));
        motivation "linking";
        ("target", str source_resource);
        ("created", str (Ptime.to_rfc3339 ~frac_s:3 (Ptime_clock.now ())));
      ]
  in
  let params =
    D.generation "unused"
      ~extra:
        [
          ( "context",
            D.context_of
              (`Assoc
                 [
                   ("kind", str "annotation");
                   ("annotation", annotation);
                   ("sourceResource", D.descriptor_of source_resource);
                 ]) );
        ]
  in
  ignore (D.created creator "yield" params : string);
  Check.equal "the job's resource" (str source_resource)
    (Check.at (D.claimed worker [ D.yields ]) [ "params"; "resourceId" ])

(* A yield job broken as a case means it: a member beside its params, and
   a change to its params. *)
let yield_broken =
  [
    ( "a resourceId beside the context",
      [ ("resourceId", str "res-caller") ],
      Fun.id );
    ("an empty storageUri", [], Edit.set_at [ "storageUri" ] (str ""));
    ("an empty title", [], Edit.set_at [ "title" ] (str ""));
    ("no title", [], Edit.remove_at [ "title" ]);
    ("no context", [], Edit.remove_at [ "context" ]);
    ( "a context with no usable focus",
      [],
      Edit.set_at [ "context" ]
        (D.context_of (`Assoc [ ("kind", str "nothing") ])) );
  ]

let refuses_yield (envelope, change) w =
  let creator = D.person w "creator" in
  let params = change (D.generation (D.resource_id ())) in
  refused_at_door creator "job:create"
    (`Assoc ([ ("jobType", str "yield"); ("params", params) ] @ envelope));
  nothing_queued w

let yield_extras =
  [
    ("a referenceId", [ ("referenceId", str "ref-caller") ]);
    ("a resourceId", [ ("resourceId", str "res-caller") ]);
    ( "two it does not take",
      [ ("schemaId", str "irac"); ("instructions", str "be brief") ] );
  ]

let refuses_yield_parameter extra w =
  let creator = D.person w "creator" in
  let answer =
    D.create creator "yield" (D.generation (D.resource_id ()) ~extra)
  in
  Check.equal "the reply's channel" (str "job:create-failed")
    (str answer.channel);
  member_is "the reply's message"
    (Some
       (str
          (D.refusal_text "yield-parameter"
             ~fill:[ ("params", String.concat ", " (List.map fst extra)) ])))
    (D.message_of answer);
  nothing_queued w

let admits_registered w =
  let creator = D.person w "creator" in
  let before = D.reads w D.entity_types_read in
  ignore
    (mark creator
       [
         motivation "linking"; ("entityTypes", strings D.vocabulary.entity_types);
       ]
      : string);
  ignore
    (D.created creator "yield"
       (D.generation (D.resource_id ())
          ~extra:[ ("entityTypes", strings [ "Person" ]) ])
      : string);
  Check.equal "the entity-type reads"
    (`Int (before + 2))
    (`Int (D.reads w D.entity_types_read))

let refuses_unregistered w =
  let creator = D.person w "creator" in
  let linking =
    D.create ~resource_id:(D.resource_id ()) creator "mark"
      (`Assoc
         [
           motivation "linking";
           ("entityTypes", strings [ "Person"; "Unicorn"; "Dragon" ]);
         ])
  in
  Check.equal "the linking job's reply" (str "job:create-failed")
    (str linking.channel);
  member_is "the linking job's message"
    (Some
       (str
          (D.refusal_text "entity-type-not-registered"
             ~fill:[ ("types", "Unicorn, Dragon") ])))
    (D.message_of linking);
  lacks "the linking job's refusal" linking.payload [ "code" ];
  let yielded =
    D.create creator "yield"
      (D.generation (D.resource_id ())
         ~extra:[ ("entityTypes", strings [ "UnknownThing" ]) ])
  in
  member_is "the yield job's message"
    (Some
       (str
          (D.refusal_text "entity-type-not-registered"
             ~fill:[ ("types", "UnknownThing") ])))
    (D.message_of yielded);
  nothing_queued w

let reads_none_unnamed w =
  let creator = D.person w "creator" in
  let before = D.reads w D.entity_types_read in
  ignore (D.created creator "yield" (D.generation (D.resource_id ())) : string);
  ignore
    (D.created creator "yield"
       (D.generation (D.resource_id ()) ~extra:[ ("entityTypes", `List []) ])
      : string);
  ignore (mark creator [ motivation "highlighting" ] : string);
  Check.equal "the entity-type reads" (`Int before)
    (`Int (D.reads w D.entity_types_read))

let resolves_schema w =
  let creator = D.person w "creator" in
  let worker = D.worker w "tagger" in
  let resource_id = D.resource_id () in
  let params =
    [
      motivation "tagging";
      ("schemaId", str "irac");
      ("categories", strings [ "Issue" ]);
      ("language", str "en");
    ]
  in
  ignore (mark ~resource_id creator params : string);
  let job = D.claimed worker [ D.marks "tagging" ] in
  Check.equal "the job's params"
    (`Assoc
       ((("resourceId", str resource_id) :: params)
       @ [ ("schema", List.hd D.vocabulary.tag_schemas) ]))
    (Check.at job [ "params" ])

let refuses_unregistered_schema w =
  let creator = D.person w "creator" in
  let before = D.reads w D.tag_schemas_read in
  let unknown =
    D.create ~resource_id:(D.resource_id ()) creator "mark"
      (`Assoc
         [
           motivation "tagging";
           ("schemaId", str "nope");
           ("categories", strings [ "Issue" ]);
         ])
  in
  Check.equal "the reply's channel" (str "job:create-failed")
    (str unknown.channel);
  member_is "the reply's message"
    (Some
       (str
          (D.refusal_text "tag-schema-not-registered"
             ~fill:[ ("schemaId", "nope") ])))
    (D.message_of unknown);
  Check.equal "the tag-schema reads"
    (`Int (before + 1))
    (`Int (D.reads w D.tag_schemas_read));
  declined "the code of a claim for tagging jobs"
    (D.claim (D.worker w "idle") [ D.marks "tagging" ])

let holds_as_given w =
  let creator = D.person w "creator" in
  let worker = D.worker w "assessor" in
  let before = D.reads w D.tag_schemas_read in
  let resource_id = D.resource_id () in
  let params =
    [
      motivation "assessing";
      ("instructions", str "be kind");
      ("tone", str "constructive");
      ("density", `Int 2);
    ]
  in
  ignore (mark ~resource_id creator params : string);
  Check.equal "the job's params"
    (`Assoc (("resourceId", str resource_id) :: params))
    (Check.at (D.claimed worker [ D.marks "assessing" ]) [ "params" ]);
  Check.equal "the tag-schema reads" (`Int before)
    (`Int (D.reads w D.tag_schemas_read))

let absent_refuses w =
  let creator = D.person w "creator" in
  let peer = Protocol.failure "peer-unavailable" in
  List.iter
    (fun (params, read) ->
      let started = Run.now () in
      let answer =
        D.create ~resource_id:(D.resource_id ()) creator "mark" (`Assoc params)
      in
      let took = Run.now () -. started in
      Check.that
        (Printf.sprintf "the refusal took %g s, not under 5" took)
        (took < 5.0);
      Check.equal "the reply's channel" (str "job:create-failed")
        (str answer.channel);
      Check.matching "the refusal"
        [
          ( "message",
            str (Protocol.fill_template peer.message [ ("channel", read) ]) );
          ("code", str peer.code);
        ]
        answer.payload)
    [
      ( [ motivation "linking"; ("entityTypes", strings [ "Person" ]) ],
        D.entity_types_read );
      ( [
          motivation "tagging";
          ("schemaId", str "irac");
          ("categories", strings [ "Issue" ]);
        ],
        D.tag_schemas_read );
    ]

let absent_admits w =
  let creator = D.person w "creator" in
  ignore (mark creator [ motivation "highlighting" ] : string);
  ignore (D.created creator "yield" (D.generation (D.resource_id ())) : string)

let tests implementations =
  with_dispatcher implementations "job:create"
    ([
       case
         "records the requester, a fresh id, no retries yet and a budget by \
          type"
         records_requester;
     ]
    @ List.map
        (fun (what, params, extra) ->
          case
            (Printf.sprintf
               "refuses %s at the door, a parameter that job does not take, \
                and queues nothing"
               what)
            (refuses_not_taken (params, extra)))
        not_taken
    @ List.map
        (fun (what, params) ->
          case
            (Printf.sprintf
               "refuses %s at the door: a value a job needs is stated, and not \
                empty"
               what)
            (refuses_not_stated params))
        not_stated
    @ [
        case
          "refuses at the door a mark job that names no resource, and queues \
           nothing"
          refuses_no_resource;
        case "records a yield job under the resource its context focuses"
          yield_under_focus;
        case
          "records a yield job focused on an annotation under the resource the \
           annotation is on"
          yield_under_annotation;
      ]
    @ List.map
        (fun (what, envelope, change) ->
          case
            (Printf.sprintf
               "refuses at the door a yield job with %s, and queues nothing"
               what)
            (refuses_yield (envelope, change)))
        yield_broken
    @ List.map
        (fun (what, extra) ->
          case
            (Printf.sprintf
               "refuses a yield job given %s among its parameters: the \
                context's focus is what it is about, and it takes what a yield \
                job takes"
               what)
            (refuses_yield_parameter extra))
        yield_extras
    @ [
        case
          "admits entity types the knowledge base registers, reading them \
           afresh for every job"
          admits_registered;
        case
          "refuses entity types the knowledge base does not register, naming \
           every one, and queues nothing"
          refuses_unregistered;
        case "does not read the entity types for a job that names none"
          reads_none_unnamed;
        case
          "resolves a tagging job's schemaId into the schema itself, beside \
           it, so its worker needs no registry"
          resolves_schema;
        case
          "refuses a tagging job whose schema the knowledge base does not \
           register, having read the schemas"
          refuses_unregistered_schema;
        case
          "holds every other job's params as given, with its resource, and \
           reads no tag schemas"
          holds_as_given;
      ])
  @ with_dispatcher implementations ~vocabulary:None
      "job:create with the Archivist absent"
      [
        case
          "refuses at once a job that needs a vocabulary read, naming the read \
           and carrying peer-unavailable"
          absent_refuses;
        case "admits a job that needs no read" absent_admits;
      ]
