(* The job protocol agrees with itself. It is stated in three places that a
   second implementation reads instead of the code: the Job schemas, the
   storage layout (jobs/storage.json) and the dispatcher's and a worker's
   configuration documents. Each restates a fact another owns:

   - JobCancelRequest is exactly one required [jobId], a JobId: a
     cancellation names one job, and selects no other way;
   - a job description, an announcement, a filter and a completion are each
     told apart by exactly JobType, and a mark job's parameters by exactly
     Motivation, each member stating the one value that selects it; and a
     motivation's parameters are closed;
   - an announcement carries the description less the job's input;
   - a filter is closed and names fields the job description has, and a mark
     filter always states its motivation;
   - JobResult has no discriminant, and its members are closed, state no
     kind and share no required member, so none is mistaken for another; the
     results the verbs report are, together, exactly its members;
   - the layout's record is a component;
   - Job's discriminator is its members, each one's status the value that
     selects it;
   - DispatcherConfig and WorkerConfig default nothing, and leave optional
     only a field that is a secret's name ([...Env]): the dispatcher and a
     worker each read what their document says and supply nothing of their
     own. *)

let ( / ) pointer token = pointer ^ "/" ^ Decode.escape token
let component name = Places.components ^ name ^ ".json"

(* The file a [$ref] written in [file] names. *)
let resolve file target = fst (Decode.resolve { target; file; pointer = "" })
let text = function Some (`String s) -> Some s | _ -> None

let texts = function
  | Some (`List items) ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

let keys = function Some (`Assoc members) -> List.map fst members | _ -> []
let same a b = List.sort compare a = List.sort compare b
let shown words = "[" ^ String.concat "," words ^ "]"

let refusals ~(read_json : string -> Yojson.Safe.t option) =
  let found = ref [] in
  let refuse file pointer fmt =
    Printf.ksprintf
      (fun message -> found := { Decode.file; pointer; message } :: !found)
      fmt
  in
  let schema file =
    match read_json file with
    | Some json -> json
    | None ->
        refuse file "" "is not there: the job protocol states it";
        `Null
  in
  let at pointer json = Decode.at_pointer pointer json in
  let job_types = texts (at "/enum" (schema (component "JobType"))) in
  let motivations = texts (at "/enum" (schema (component "Motivation"))) in
  let cancellation_file = component "JobCancelRequest" in
  let cancellation = schema cancellation_file in
  if
    not
      (same (keys (at "/properties" cancellation)) [ "jobId" ]
      && same (texts (at "/required" cancellation)) [ "jobId" ]
      && text (at "/properties/jobId/$ref" cancellation) = Some "./JobId.json")
  then
    refuse cancellation_file ""
      "is not exactly one required jobId, a JobId: a cancellation names one \
       job, and selects no other way";
  (* A union told apart by [property]: its mapping is its members, the
     property of each member is exactly the value that selects it, and the
     values are exactly [domain]. Each member by its value, with its file. *)
  let partitions name property domain domain_name =
    let file = component name in
    let union = schema file in
    let mapping =
      match at "/discriminator/mapping" union with
      | Some (`Assoc mapping) ->
          List.filter_map
            (function value, `String ref -> Some (value, ref) | _ -> None)
            mapping
      | _ -> []
    in
    if text (at "/discriminator/propertyName" union) <> Some property then
      refuse file "/discriminator" "%s is not told apart by %s" name property;
    let members =
      match at "/oneOf" union with
      | Some (`List members) ->
          List.filter_map (fun m -> text (at "/$ref" m)) members
      | _ -> []
    in
    if not (same members (List.map snd mapping)) then
      refuse file "/discriminator/mapping"
        "%s's discriminator maps %s, its members are %s" name
        (shown (List.map snd mapping))
        (shown members);
    if not (same (List.map fst mapping) domain) then
      refuse file "/discriminator/mapping" "%s is told apart by %s, %s is %s"
        name
        (shown (List.map fst mapping))
        domain_name (shown domain);
    List.map
      (fun (value, ref) ->
        let member_file = resolve file ref in
        let member = schema member_file in
        let selects =
          texts (at ("" / "properties" / property / "enum") member)
        in
        if selects <> [ value ] then
          refuse member_file
            ("" / "properties" / property)
            "%s selects %s by %s %s, whose %s is %s" name ref property value
            property (shown selects);
        (value, (member_file, member)))
      mapping
  in
  let created = partitions "JobCreateCommand" "jobType" job_types "JobType" in
  let queued = partitions "JobQueuedEvent" "jobType" job_types "JobType" in
  let filters = partitions "JobFilter" "jobType" job_types "JobType" in
  let completed =
    partitions "JobCompleteCommand" "jobType" job_types "JobType"
  in
  let marks =
    partitions "MarkJobParams" "motivation" motivations "Motivation"
  in
  List.iter
    (fun (motivation, (file, params)) ->
      if at "/additionalProperties" params <> Some (`Bool false) then
        refuse file ""
          "a %s job's parameters are open: a parameter it does not take would \
           be accepted"
          motivation)
    marks;
  let params_ref of_ job_type =
    Option.bind (List.assoc_opt job_type of_) (fun (_, json) ->
        text (at "/properties/params/$ref" json))
  in
  let show = Option.value ~default:"nothing" in
  (* An announcement carries the description less the job's input: a mark
     job's parameters whole, and a yield job's without the one member that is
     its input. *)
  if params_ref queued "mark" <> params_ref created "mark" then
    refuse
      (component "MarkJobQueuedEvent")
      "/properties/params"
      "a mark job is announced with %s, and created with %s"
      (show (params_ref queued "mark"))
      (show (params_ref created "mark"));
  (match (List.assoc_opt "yield" created, params_ref created "yield") with
  | Some (created_file, _), Some yield_ref -> (
      let yield_file = resolve created_file yield_ref in
      let yield_params = schema yield_file in
      match at "/allOf" yield_params with
      | Some (`List [ asked; input ])
        when Option.is_some (text (at "/$ref" asked)) ->
          let asked_ref = Option.get (text (at "/$ref" asked)) in
          if params_ref queued "yield" <> Some asked_ref then
            refuse
              (component "YieldJobQueuedEvent")
              "/properties/params"
              "a yield job is announced with %s, and what it is asked for is %s"
              (show (params_ref queued "yield"))
              asked_ref;
          if
            not
              (same (keys (at "/properties" input)) [ "context" ]
              && same (texts (at "/required" input)) [ "context" ])
          then
            refuse yield_file "/allOf/1"
              "a yield job's input is %s, and an announcement leaves out \
               exactly its context"
              (shown (keys (at "/properties" input)));
          let asked_file = resolve yield_file asked_ref in
          if List.mem "context" (keys (at "/properties" (schema asked_file)))
          then
            refuse asked_file "/properties/context"
              "%s carries the context it is defined without" asked_ref
      | _ ->
          refuse yield_file "/allOf"
            "a yield job's parameters are not stated as what is asked for and \
             its input")
  | _ ->
      refuse
        (component "YieldJobCreateCommand")
        "/properties/params" "a yield job's description names no parameters");
  (* A filter names fields of the job description, at the description's
     paths. *)
  List.iter
    (fun (job_type, (file, filter)) ->
      if at "/additionalProperties" filter <> Some (`Bool false) then
        refuse file "" "a %s filter is open" job_type;
      let described =
        Option.fold ~none:[]
          ~some:(fun (_, json) -> keys (at "/properties" json))
          (List.assoc_opt job_type created)
      in
      List.iter
        (fun name ->
          if not (List.mem name described) then
            refuse file
              ("" / "properties" / name)
              "a %s filter names %s, which a %s job description does not have"
              job_type name job_type)
        (keys (at "/properties" filter)))
    filters;
  (match List.assoc_opt "mark" filters with
  | Some (file, filter) ->
      List.iter
        (fun name ->
          List.iter
            (fun (motivation, (_, params)) ->
              if not (List.mem name (keys (at "/properties" params))) then
                refuse file
                  ("/properties/params/properties" / name)
                  "a mark filter names params.%s, which a %s job does not have"
                  name motivation)
            marks)
        (keys (at "/properties/params/properties" filter));
      if
        not
          (same
             (texts (at "/properties/params/required" filter))
             [ "motivation" ])
      then
        refuse file "/properties/params"
          "a mark filter does not always state its motivation"
  | None -> ());
  (* A result has no discriminant, so its members are told apart by what each
     alone carries: closed, with no required member in common. *)
  let results_file = component "JobResult" in
  let results = schema results_file in
  if Option.is_some (at "/discriminator" results) then
    refuse results_file "/discriminator"
      "JobResult has a discriminant of its own";
  let result_refs =
    match at "/oneOf" results with
    | Some (`List members) ->
        List.filter_map (fun m -> text (at "/$ref" m)) members
    | _ -> []
  in
  let required = Hashtbl.create 8 in
  List.iter
    (fun ref ->
      let file = resolve results_file ref in
      let result = schema file in
      if at "/additionalProperties" result <> Some (`Bool false) then
        refuse file "" "%s is open: another result would decode as it" ref;
      if List.mem "kind" (keys (at "/properties" result)) then
        refuse file "/properties/kind" "%s states a kind" ref;
      List.iter
        (fun name ->
          match Hashtbl.find_opt required name with
          | Some other ->
              refuse file "/required" "%s and %s both require %s" ref other name
          | None -> Hashtbl.replace required name ref)
        (texts (at "/required" result)))
    result_refs;
  (* A completion carries its verb's result. What is stored is any verb's
     (JobResult), so the verbs' results are, together, exactly its members. *)
  let reported =
    List.concat_map
      (fun (job_type, (file, completion)) ->
        match text (at "/properties/result/$ref" completion) with
        | None ->
            refuse file "/properties/result"
              "a %s completion's result is not a union of results" job_type;
            []
        | Some ref -> (
            let union_file = resolve file ref in
            let union = schema union_file in
            if Option.is_some (at "/discriminator" union) then
              refuse union_file "/discriminator"
                "%s has a discriminant of its own" ref;
            match at "/oneOf" union with
            | Some (`List members) ->
                List.filter_map (fun m -> text (at "/$ref" m)) members
            | _ ->
                refuse file "/properties/result"
                  "a %s completion's result is not a union of results" job_type;
                []))
      completed
  in
  if not (same (List.sort_uniq compare reported) result_refs) then
    refuse results_file "/oneOf" "the verbs report %s, JobResult is %s"
      (shown (List.sort_uniq compare reported))
      (shown result_refs);
  (match Option.bind (read_json "jobs/storage.json") (at "/bucket/record") with
  | Some (`String record) when Option.is_none (read_json (component record)) ->
      refuse "jobs/storage.json" "/bucket/record"
        "the storage layout's record %s is not a schema" record
  | _ -> ());
  let job_file = component "Job" in
  let job = schema job_file in
  let job_members =
    match at "/oneOf" job with
    | Some (`List members) ->
        List.filter_map (fun m -> text (at "/$ref" m)) members
    | _ -> []
  in
  let job_mapping =
    match at "/discriminator/mapping" job with
    | Some (`Assoc mapping) ->
        List.filter_map
          (function status, `String ref -> Some (status, ref) | _ -> None)
          mapping
    | _ -> []
  in
  if not (same job_members (List.map snd job_mapping)) then
    refuse job_file "/discriminator/mapping"
      "Job's discriminator maps %s, its members are %s"
      (shown (List.map snd job_mapping))
      (shown job_members);
  List.iter
    (fun (status, ref) ->
      let file = resolve job_file ref in
      let selects = texts (at "/properties/status/enum" (schema file)) in
      if selects <> [ status ] then
        refuse file "/properties/status"
          "Job's discriminator selects %s by status %s, whose status is %s" ref
          status (shown selects))
    job_mapping;
  (* A document's schema, followed through what it refers to and into what it
     lists: a part stated in a file of its own is held as the rest is. *)
  let rec everything_stated document file pointer path node =
    match text (at "/$ref" node) with
    | Some ref ->
        let target = resolve file ref in
        everything_stated document target "" path (schema target)
    | None -> (
        if Option.is_some (at "/default" node) then
          refuse file (pointer / "default") "%s defaults %s" document path;
        match (text (at "/type" node), node) with
        | Some "array", _ ->
            Option.iter
              (everything_stated document file (pointer / "items") (path ^ "[]"))
              (at "/items" node)
        | Some "object", _ ->
            let required = texts (at "/required" node) in
            List.iter
              (fun name ->
                let property_at =
                  if path = "" then name else path ^ "." ^ name
                in
                if
                  (not (List.mem name required))
                  && not (String.ends_with ~suffix:"Env" name)
                then
                  refuse file
                    (pointer / "properties" / name)
                    "%s leaves %s optional" document property_at;
                Option.iter
                  (everything_stated document file
                     (pointer / "properties" / name)
                     property_at)
                  (at ("" / "properties" / name) node))
              (keys (at "/properties" node))
        | _ -> ())
  in
  List.iter
    (fun (document, area) ->
      let file = area ^ "/" ^ document ^ ".json" in
      match read_json file with
      | None ->
          refuse file "" "%s is not a schema of specs/src/%s" document area
      | Some json -> everything_stated document file "" "" json)
    [ ("DispatcherConfig", "dispatcher"); ("WorkerConfig", "worker") ];
  List.rev !found
