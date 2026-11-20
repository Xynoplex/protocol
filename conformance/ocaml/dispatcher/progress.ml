(* Progress and checkpoints (JOBS.md § job:report-progress, § job:checkpoint,
   § Checkpoints). The job's status shows its progress; its checkpoint is
   not exposed, so a checkpoint is read the way a worker reads it: off the
   claim of the attempt that follows a failure. *)

open Xynolog_conformance
open Support
open Case

(* A cursor [next] units in, of a unit of [size]. *)
let cursor_at ?(size = 10) next =
  `Assoc
    [
      ("next", `Int next);
      ("size", `Int size);
      ("found", `Int (next * 2));
      ("emitted", `Int next);
      ("errors", `Int (next mod 2));
    ]

(* End the attempt and claim the next: the claim carries the checkpoint as
   the record holds it. Its metadata. *)
let retried w (r : D.running) =
  D.settle ();
  D.fail r.worker r.ref "interrupted";
  let creator = D.person w "creator" in
  ignore
    (D.until_status creator r.ref.job_id "the job to be re-queued" "pending"
      : Yojson.Safe.t);
  Check.at (D.claimed r.worker [ D.filter_of r.job ]) [ "metadata" ]

(* ── job:report-progress ──────────────────────────────────────────── *)

let percentage_is n s =
  match Protocol.at s [ "progress"; "percentage" ] with
  | Some (`Int p) -> p = n
  | Some (`Float p) -> p = float_of_int n
  | _ -> false

let whole w =
  let r =
    D.running
      ~job:
        (D.Mark
           [
             motivation "tagging";
             ("schemaId", str "irac");
             ("categories", `List [ str "Issue" ]);
           ])
      w
  in
  let progress =
    `Assoc
      [
        ("percentage", `Int 40);
        ("message", `Assoc [ ("code", str "analyzing-tags") ]);
        ("current", `Assoc [ ("kind", str "category"); ("value", str "Issue") ]);
        ("processed", `Int 1);
        ("total", `Int 2);
      ]
  in
  D.report_progress ~progress r.worker r.ref (`Int 40);
  Check.equal "the job's progress" progress
    (Check.at
       (D.until r.creator r.ref.job_id "the progress to show" (percentage_is 40))
       [ "progress" ])

let bare w =
  let r = D.running w in
  D.report_progress r.worker r.ref (`Int 70);
  Check.equal "the job's progress"
    (`Assoc [ ("percentage", `Int 70) ])
    (Check.at
       (D.until r.creator r.ref.job_id "the progress to show" (fun s ->
            Protocol.at s [ "progress" ] <> Some (`Assoc [])
            && Protocol.at s [ "progress" ] <> None))
       [ "progress" ])

let not_running_progress w =
  let creator = D.person w "creator" in
  let worker = D.worker w "premature" in
  let resource_id = D.resource_id () in
  let job_id =
    D.created ~resource_id creator "mark" (`Assoc [ motivation "commenting" ])
  in
  D.report_progress worker { job_id; job_type = "mark"; resource_id } (`Int 90);
  D.settle ();
  lacks "the status" (D.status_of creator job_id) [ "progress" ];
  Check.equal "the claim's progress" (`Assoc [])
    (Check.at (D.claimed worker [ D.marks "commenting" ]) [ "progress" ])

(* ── job:checkpoint ───────────────────────────────────────────────── *)

let finished_units w =
  let r = D.running ~job:linking w in
  D.checkpoint r.worker r.ref.job_id [ "Person" ];
  let metadata = retried w r in
  Check.equal "the retry's finished units" (units [ "Person" ])
    (Check.at metadata [ "completedUnits" ]);
  lacks "the retry's metadata" metadata [ "unitCursors" ]

let unions w =
  let r = D.running ~job:linking w in
  D.checkpoint r.worker r.ref.job_id [ "Person" ];
  D.settle ();
  D.checkpoint r.worker r.ref.job_id [ "Place"; "Person" ];
  Check.equal "the retry's finished units"
    (units [ "Person"; "Place" ])
    (sorted (Check.at (retried w r) [ "completedUnits" ]))

let cursor_unfinished w =
  let r = D.running ~job:linking w in
  D.checkpoint
    ~unit_cursors:[ ("Person", cursor_at 2) ]
    r.worker r.ref.job_id [];
  let metadata = retried w r in
  Check.equal "the retry's cursors"
    (`Assoc [ ("Person", cursor_at 2) ])
    (Check.at metadata [ "unitCursors" ]);
  Check.equal "the retry's finished units" (`List [])
    (Option.value ~default:(`List [])
       (Protocol.at metadata [ "completedUnits" ]))

let advances w =
  let r = D.running ~job:linking w in
  D.checkpoint
    ~unit_cursors:[ ("Person", cursor_at 2) ]
    r.worker r.ref.job_id [];
  D.settle ();
  D.checkpoint
    ~unit_cursors:[ ("Person", cursor_at ~size:12 5) ]
    r.worker r.ref.job_id [];
  D.settle ();
  D.checkpoint
    ~unit_cursors:[ ("Person", cursor_at ~size:99 3) ]
    r.worker r.ref.job_id [];
  Check.equal "the retry's cursors"
    (`Assoc [ ("Person", cursor_at ~size:12 5) ])
    (Check.at (retried w r) [ "unitCursors" ])

let per_unit w =
  let r = D.running ~job:linking w in
  D.checkpoint
    ~unit_cursors:[ ("Person", cursor_at 2) ]
    r.worker r.ref.job_id [];
  D.settle ();
  D.checkpoint ~unit_cursors:[ ("Place", cursor_at 7) ] r.worker r.ref.job_id [];
  Check.equal "the retry's cursors"
    (`Assoc [ ("Person", cursor_at 2); ("Place", cursor_at 7) ])
    (Check.at (retried w r) [ "unitCursors" ])

let finished_cursor_merged w =
  let r = D.running ~job:linking w in
  D.checkpoint
    ~unit_cursors:[ ("Person", cursor_at 2); ("Place", cursor_at 4) ]
    r.worker r.ref.job_id [];
  D.settle ();
  D.checkpoint r.worker r.ref.job_id [ "Person" ];
  D.settle ();
  D.checkpoint
    ~unit_cursors:[ ("Person", cursor_at 9) ]
    r.worker r.ref.job_id [];
  D.settle ();
  D.checkpoint
    ~unit_cursors:[ ("Person", cursor_at ~size:99 5) ]
    r.worker r.ref.job_id [];
  let metadata = retried w r in
  Check.equal "the retry's finished units" (units [ "Person" ])
    (Check.at metadata [ "completedUnits" ]);
  Check.equal "the retry's cursors"
    (`Assoc [ ("Person", cursor_at 9); ("Place", cursor_at 4) ])
    (Check.at metadata [ "unitCursors" ])

let only_cursor_kept w =
  let r = D.running ~job:linking w in
  D.checkpoint
    ~unit_cursors:[ ("Person", cursor_at 2) ]
    r.worker r.ref.job_id [];
  D.settle ();
  D.checkpoint r.worker r.ref.job_id [ "Person" ];
  let metadata = retried w r in
  Check.equal "the retry's finished units" (units [ "Person" ])
    (Check.at metadata [ "completedUnits" ]);
  Check.equal "the retry's cursors"
    (`Assoc [ ("Person", cursor_at 2) ])
    (Check.at metadata [ "unitCursors" ])

let next_claimant w =
  let r = D.running ~job:linking w in
  D.checkpoint
    ~unit_cursors:[ ("Person", cursor_at 4) ]
    r.worker r.ref.job_id [];
  D.settle ();
  D.checkpoint
    ~unit_cursors:[ ("Person", cursor_at 9) ]
    r.worker r.ref.job_id [ "Person" ];
  D.settle ();
  D.checkpoint
    ~unit_cursors:[ ("Place", cursor_at 3) ]
    r.worker r.ref.job_id [ "Person" ];
  D.settle ();
  D.fail r.worker r.ref "interrupted";
  ignore
    (D.until_status r.creator r.ref.job_id "the job to be re-queued" "pending"
      : Yojson.Safe.t);
  let next = D.worker w "next-claimant" in
  let metadata =
    Check.at (D.claimed next [ D.filter_of r.job ]) [ "metadata" ]
  in
  Check.equal "the retry's finished units" (units [ "Person" ])
    (Check.at metadata [ "completedUnits" ]);
  Check.equal "the retry's cursors"
    (`Assoc [ ("Person", cursor_at 9); ("Place", cursor_at 3) ])
    (Check.at metadata [ "unitCursors" ])

let not_running_checkpoint w =
  let creator = D.person w "creator" in
  let worker = D.worker w "premature" in
  let job_id =
    D.created ~resource_id:(D.resource_id ()) creator "mark"
      (`Assoc
         [
           motivation "linking";
           ("entityTypes", `List [ str "Person"; str "Place" ]);
         ])
  in
  D.checkpoint
    ~unit_cursors:[ ("Place", cursor_at 1) ]
    worker job_id [ "Person" ];
  D.settle ();
  let metadata =
    Check.at (D.claimed worker [ D.marks "linking" ]) [ "metadata" ]
  in
  lacks "the claim's metadata" metadata [ "completedUnits" ];
  lacks "the claim's metadata" metadata [ "unitCursors" ]

let tests implementations =
  with_dispatcher implementations "job:report-progress"
    [
      case "records the progress a worker reports, whole" whole;
      case "records a bare percentage when the report carries no progress" bare;
      case
        "ignores progress for a job that is not running: its claim starts with \
         none"
        not_running_progress;
    ]
  @ with_dispatcher implementations "job:checkpoint"
      [
        case
          "records finished units, and a retry resumes with them; with no \
           cursor stated, the record has none"
          finished_units;
        case "unions successive checkpoints" unions;
        case
          "records a cursor for a unit without finishing it: a cursor never \
           says a unit is finished"
          cursor_unfinished;
        case
          "advances a cursor, and never moves it back: a late, older cursor \
           leaves the newer one whole"
          advances;
        case "keeps a cursor per unit" per_unit;
        case
          "keeps the cursor of a unit that finishes, and merges a later cursor \
           for it as any other: one further replaces it, one behind does not"
          finished_cursor_merged;
        case
          "keeps the cursor of a finished unit when it is the only cursor the \
           record has"
          only_cursor_kept;
        case
          "records a unit named finished with its cursor in one checkpoint, \
           keeps that cursor across a later checkpoint, and hands it to the \
           next claimant"
          next_claimant;
        case "has no effect on a job that is not running" not_running_checkpoint;
      ]
