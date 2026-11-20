(* A Worker service's world: the service as a process, and everything it
   meets. That is each knowledge base it serves, as the suite stands in for
   one: the trusted issuer, which holds the public half of the worker's key
   on a client of its own; a real gateway, with the harness's stand-in
   Archivist behind it for bytes, for uploads and for the text the Smelter
   read out of a PDF; and the suite itself on the gateway's bus, playing the
   two parties a worker asks things of there. As the dispatcher it answers
   job:claim from the jobs a case queued. As the record it answers
   browse:kb-requested, browse:resource-requested, mark:commit and
   browse:annotation-requested. Beside them are a stand-in Ollama and a
   stand-in Anthropic, the providers every knowledge base's engines share.

   A case's worker serves the world's own knowledge base, and, in a case
   about a worker of several, the world's second beside it. The worker is
   pointed at a recording proxy in front of each gateway, so everything it
   sends a knowledge base is read as a transcript: its sign-in, the stream it
   opens, each emit, each read of bytes. Nothing reaches inside the
   process. *)

let member = Yojson.Safe.Util.member
let str s = `String s

(* The client the world's own knowledge base's issuer holds the worker's key
   on: the worker's account there. *)
let worker_client = "xynolog-worker"

(* The client the second knowledge base's issuer holds the worker's key on:
   another than the first's, so that what the worker says it is called at
   each is told apart. *)
let second_worker_client = "xynolog-worker-second"

(* The case of kb-identity/cases.json the second knowledge base is. *)
let second_kb = "plain scalars, and a version, which names nothing"

(* The roles the worker's account holds at each issuer, in the order its
   token carries them. *)
let worker_roles () = [ Protocol.service_role (); Protocol.worker_role () ]

(* The variable an Anthropic engine's entry names for its key (apiKeyEnv).
   It is not one the provider's own library reads, so a worker that reaches
   the stand-in read the variable its document names. *)
let anthropic_key_variable = "WORKER_SERVICE_ANTHROPIC_KEY"

(* What the stand-in Anthropic says of a model unless a case says
   otherwise: it reads far more than it writes, and answers in a schema. *)
let anthropic_model : Anthropic.facts =
  {
    max_input_tokens = 200_000;
    max_output_tokens = 64_000;
    structured_outputs = true;
  }

(* An engine a case's worker runs: a provider and a model. An engine is no
   principal: the gateway names a worker by its account, and what a worker
   states of an engine is its provider and model, and no name. *)
type engine = { provider : string; model : string }

(* The engines the suite's workers run, two of each provider, so that a case
   can start a worker of two. *)
let ollama_engines =
  [
    { provider = "ollama"; model = "gemma2:27b" };
    { provider = "ollama"; model = "library/llama3" };
  ]

let anthropic_engines =
  [
    { provider = "anthropic"; model = "claude-sonnet-5" };
    { provider = "anthropic"; model = "claude-3.5-sonnet" };
  ]

(* ── Jobs ───────────────────────────────────────────────────────────── *)

(* A job as the dispatcher hands it to whoever claims it: the JobRunning,
   and what a case reads of it. *)
type job = {
  id : string;
  job_type : string;
  resource_id : string;
  retry_count : int;
  running : Yojson.Safe.t;
}

(* Whether a claim's filter takes [job]: its type, and for a mark job its
   motivation. *)
let takes filter job =
  member "jobType" filter = str job.job_type
  && (job.job_type <> "mark"
     || member "motivation" (member "params" filter)
        = member "motivation" (member "params" job.running))

(* ── What a knowledge base was asked ─────────────────────────────────── *)

(* A batch a worker committed, as the record received it. *)
type commit = {
  committed_to : string;  (** The resource. *)
  committed_job : Yojson.Safe.t;
  annotations : Yojson.Safe.t list;
  committed_by : Yojson.Safe.t;  (** The DID the gateway stamped it with. *)
}

(* One job:claim the dispatcher received: who claimed, with what filters,
   and the job it was answered with. *)
type claim = {
  claimed_by : Yojson.Safe.t;
  roles : Yojson.Safe.t;
  accepts : Yojson.Safe.t list;
  mutable answered : string option;
}

(* How a knowledge base answers a worker that asks which it is, when a case
   says otherwise than its plain answer: with another domain, with
   browse:kb-failed, or not at all. *)
type confirmation = Domain of string | Kb_failed of string | Unanswered

(* What a case has the suite's parties do instead of their plain answer.
   [claim] answers a claim with a refusal: its code, when it has one, and
   its message. [hand_any_job] answers a claim with the next job whatever the
   claim's filters: what no dispatcher does. [commit] runs when a batch
   arrives and before it is acknowledged, and may refuse it with a reason.
   [described] runs when a resource's description is asked for and before it
   is answered. [confirm] runs when the worker asks which knowledge base this
   is and before it is answered, and may answer otherwise than this
   knowledge base's domain. Each runs in a fiber of its own, and may wait. *)
type hooks = {
  claim : (unit -> (string option * string) option) option;
  hand_any_job : bool;
  commit : (commit -> string option) option;
  described : (string -> unit) option;
  confirm : (unit -> confirmation option) option;
}

let no_hooks =
  {
    claim = None;
    hand_any_job = false;
    commit = None;
    described = None;
    confirm = None;
  }

(* A knowledge base a worker serves, as the suite stands in for it: its
   world, behind the issuer that holds the worker's key on [client], and the
   suite on its bus as the dispatcher and the record. *)
type kb = {
  world : World.t;
  client : string;  (** The worker's account here. *)
  queue : job Dynarray.t;
      (** Jobs waiting to be claimed, in order. A claim is answered with the
          first its filters take. *)
  claims : claim Dynarray.t;
  descriptors : (string, Yojson.Safe.t) Hashtbl.t;
      (** What the record answers browse:resource-requested with. *)
  descriptor_reads : string Dynarray.t;
  commits : commit Dynarray.t;  (** Acknowledged or not. *)
  recorded : (string, Yojson.Safe.t list) Hashtbl.t;
      (** What the record holds, per resource: an annotation is recorded once,
          by its id. *)
  violations : string Dynarray.t;
      (** What the suite did wrong, as a party a worker meets. *)
  mutable hooks : hooks;
  mutable peer : World.responder option;
}

(* The channels the suite answers on a knowledge base's bus. *)
let answered_channels =
  [
    "job:claim";
    "browse:kb-requested";
    "browse:resource-requested";
    "mark:commit";
    "browse:annotation-requested";
  ]

let domain kb = kb.world.kb.domain

(* The DID the gateway names the worker by here: its account's. *)
let did kb = Protocol.agent_did (domain kb) (Issuer.subject_of kb.client)

(* The generator the worker states on what [engine] makes here. *)
let generator ?(engine = List.hd ollama_engines) kb =
  `Assoc
    [
      ("@type", str "Software");
      ("@id", str (did kb));
      ("provider", str engine.provider);
      ("model", str engine.model);
    ]

(* What a worker's document says of this knowledge base: its domain, its
   gateway at [gateway_url], its issuer and the client the worker signs in
   as there, and the engines the worker offers it. *)
let configuration ?client kb gateway_url engines =
  `Assoc
    [
      ("domain", str (domain kb));
      ("gatewayUrl", str gateway_url);
      ( "identity",
        `Assoc
          [
            ("issuer", str kb.world.issuer.origin);
            ("clientId", str (Option.value client ~default:kb.client));
          ] );
      ("engines", `List engines);
    ]

let storage_uri id = "file://worker-service/" ^ id

(* A resource's description on the bus, with no bytes behind the
   gateway. *)
let describe ?(name = "A resource") kb id media_type =
  Hashtbl.replace kb.descriptors id
    (`Assoc
       [
         ("@context", str "https://schema.org");
         ("@id", str id);
         ("name", str name);
         ( "representations",
           `List
             [
               `Assoc
                 [
                   ("mediaType", str media_type);
                   ("storageUri", str (storage_uri id));
                   ("rel", str "original");
                 ];
             ] );
       ])

(* A text resource: its bytes behind the gateway, and its description on
   the bus. *)
let resource ?(media_type = "text/markdown") ?name kb id text =
  let archivist = kb.world.archivist in
  Hashtbl.replace archivist.resources id
    { Archivist_standin.storage_uri = storage_uri id; media_type };
  Hashtbl.replace archivist.content (storage_uri id) text;
  describe ?name kb id media_type

(* The resources whose anchored text the gateway asked the Archivist for,
   one for each read, in order. *)
let anchored_text_reads kb =
  let read = Re.Perl.compile_pat "^/resources/([^/?]+)/anchored-text" in
  List.filter_map
    (fun (call : Archivist_standin.call) ->
      if call.meth <> "GET" then None
      else
        Option.map
          (fun group -> Uri.pct_decode (Re.Group.get group 1))
          (Re.exec_opt read call.path))
    (Dynarray.to_list kb.world.archivist.calls)

(* A job as the dispatcher hands it to whoever claims it, queued for the
   next claim that takes it. [metadata] are members a case sets over the
   plain ones. *)
let queued ?(metadata = []) kb job_type params =
  let id = Fresh.job_id () in
  let plain =
    [
      ("id", str id);
      ("type", str job_type);
      ("userId", str (World.person_did kb.world "requester"));
      ("created", str "2026-01-01T00:00:00.000Z");
      ("retryCount", `Int 0);
      ("maxRetries", `Int 3);
    ]
  in
  let running =
    `Assoc
      [
        ("status", str "running");
        ( "metadata",
          `Assoc
            (List.filter (fun (k, _) -> not (List.mem_assoc k metadata)) plain
            @ metadata) );
        ("params", params);
        ("startedAt", str "2026-01-01T00:00:01.000Z");
        ("progress", `Assoc []);
      ]
  in
  (match
     Protocol.against "JobClaimedResult" (`Assoc [ ("response", running) ])
   with
  | [] -> ()
  | refusals ->
      failwith
        (Printf.sprintf
           "the case queued a job a dispatcher could not hand out (%s)"
           (String.concat "; " refusals)));
  let job =
    {
      id;
      job_type;
      resource_id = Yojson.Safe.Util.to_string (member "resourceId" params);
      retry_count =
        Yojson.Safe.Util.to_int
          (member "retryCount" (member "metadata" running));
      running;
    }
  in
  Dynarray.add_last kb.queue job;
  job

let peer kb =
  match kb.peer with
  | Some peer -> peer
  | None -> failwith "the suite is not on this knowledge base's bus"

(* Emit as a party on the bus: the dispatcher's announcements, a
   cancellation, a request of the worker. *)
let emit ?correlation_id kb channel payload =
  let peer = peer kb in
  match
    World.emit kb.world peer.token
      (`Assoc
         ([ ("channel", str channel); ("payload", payload) ]
         @
         match correlation_id with
         | Some c ->
             [ ("correlationId", str c); ("clientId", str peer.client_id) ]
         | None -> []))
  with
  | { status = 202; _ } -> ()
  | reply ->
      failwith
        (Printf.sprintf "the gateway refused the suite's %s: %d %s" channel
           reply.status reply.text)

(* Announce [job] as the dispatcher announces one it has queued: job:queued,
   with the description and none of what the dispatcher adds. *)
let announce kb job =
  let description =
    match member "params" job.running with
    | `Assoc members ->
        `Assoc
          (List.filter
             (fun (k, _) -> k <> "resourceId" && k <> "schema")
             members)
    | other -> other
  in
  emit kb "job:queued"
    (`Assoc
       [
         ("jobId", str job.id);
         ("jobType", str job.job_type);
         ("resourceId", str job.resource_id);
         ("userId", member "userId" (member "metadata" job.running));
         ("params", description);
       ])

type listener = { stream : Bus_stream.t; client_id : string; token : string }

(* A stream of the suite's own, naming [channels]: for the replies and
   broadcasts a case reads. *)
let listen kb channels =
  let peer = peer kb in
  let client_id = "listener-" ^ Fresh.hex 8 in
  let stream =
    World.subscribe kb.world peer.token
      (Bus_stream.subscribe_body ~client_id ~global:channels ())
  in
  { stream; client_id; token = peer.token }

(* ── The parties the suite plays ─────────────────────────────────────── *)

let answer kb (frame : Bus_stream.frame) =
  let payload = frame.payload in
  let resource_id =
    match member "resourceId" payload with
    | `String id -> id
    | other -> Check.shown other
  in
  let response value = `Assoc [ ("response", value) ] in
  let failed ?code message =
    `Assoc
      ((match code with Some c -> [ ("code", str c) ] | None -> [])
      @ [ ("message", str message) ])
  in
  match frame.channel with
  | "job:claim" -> (
      let accepts =
        match member "accepts" payload with `List filters -> filters | _ -> []
      in
      let claim =
        {
          claimed_by = member "_userId" payload;
          roles = member "_roles" payload;
          accepts;
          answered = None;
        }
      in
      Dynarray.add_last kb.claims claim;
      match Option.bind kb.hooks.claim (fun refuse -> refuse ()) with
      | Some (code, message) -> Some ("job:claim-failed", failed ?code message)
      | None -> (
          let jobs = Dynarray.to_list kb.queue in
          let chosen =
            if kb.hooks.hand_any_job then List.nth_opt jobs 0
            else
              List.find_opt
                (fun job ->
                  List.exists (fun filter -> takes filter job) accepts)
                jobs
          in
          match chosen with
          | None ->
              Some
                ( "job:claim-failed",
                  failed ~code:"none-pending" "No pending job matches" )
          | Some job ->
              let left = List.filter (fun j -> j != job) jobs in
              Dynarray.clear kb.queue;
              List.iter (Dynarray.add_last kb.queue) left;
              claim.answered <- Some job.id;
              Some ("job:claimed", response job.running)))
  | "browse:kb-requested" -> (
      match Option.bind kb.hooks.confirm (fun confirm -> confirm ()) with
      | Some Unanswered -> None
      | Some (Kb_failed message) -> Some ("browse:kb-failed", failed message)
      | confirmation ->
          let domain =
            match confirmation with
            | Some (Domain other) -> other
            | _ -> domain kb
          in
          Some
            ( "browse:kb-result",
              response
                (`Assoc
                   [ ("name", str kb.world.kb.name); ("domain", str domain) ])
            ))
  | "browse:resource-requested" -> (
      Dynarray.add_last kb.descriptor_reads resource_id;
      Option.iter (fun described -> described resource_id) kb.hooks.described;
      match Hashtbl.find_opt kb.descriptors resource_id with
      | None ->
          Some
            ( "browse:resource-failed",
              failed ~code:"not-found" "Resource not found" )
      | Some resource ->
          Some
            ( "browse:resource-result",
              response
                (`Assoc [ ("resource", resource); ("lastSequence", `Int 1) ]) ))
  | "mark:commit" -> (
      let annotations =
        match member "annotations" payload with `List items -> items | _ -> []
      in
      let commit =
        {
          committed_to = resource_id;
          committed_job = member "jobId" payload;
          annotations;
          committed_by = member "_userId" payload;
        }
      in
      Dynarray.add_last kb.commits commit;
      match Option.bind kb.hooks.commit (fun verdict -> verdict commit) with
      | Some reason -> Some ("mark:commit-failed", failed reason)
      | None ->
          (* The record appends the annotations it does not hold. *)
          let held =
            Option.value ~default:[] (Hashtbl.find_opt kb.recorded resource_id)
          in
          let id a = member "id" a in
          let fresh =
            List.filter
              (fun a -> not (List.exists (fun h -> id h = id a) held))
              annotations
          in
          Hashtbl.replace kb.recorded resource_id (held @ fresh);
          (* persisted is how many of the batch are in the record, appended
             now or held already: never an append tally. *)
          Some
            ( "mark:commit-ok",
              response
                (`Assoc
                   [
                     ("persisted", `Int (List.length annotations));
                     ("annotationIds", `List (List.map id annotations));
                   ]) ))
  | "browse:annotation-requested" -> (
      let wanted = member "annotationId" payload in
      match
        List.find_opt
          (fun a -> member "id" a = wanted)
          (Option.value ~default:[] (Hashtbl.find_opt kb.recorded resource_id))
      with
      | None ->
          Some
            ( "browse:annotation-failed",
              failed ~code:"not-found"
                ("Annotation not found: "
                ^
                match wanted with
                | `String id -> id
                | other -> Check.shown other) )
      | Some annotation ->
          Some
            ( "browse:annotation-result",
              response
                (`Assoc
                   [
                     ("annotation", annotation);
                     ("resource", `Null);
                     ("resolvedResource", `Null);
                   ]) ))
  | channel ->
      Dynarray.add_last kb.violations
        (Printf.sprintf "the suite was sent %s, which it does not answer"
           channel);
      None

(* Stand in for the knowledge base [identity] is, whose issuer holds the
   public half of the worker's key on [client]. *)
let stand_in ~gateway identity client public_key =
  let world =
    World.create ~command:gateway ~kb:identity
      ~accounts:
        [ (client, { Issuer.proof = Key public_key; roles = worker_roles () }) ]
      In_process
  in
  let kb =
    {
      world;
      client;
      queue = Dynarray.create ();
      claims = Dynarray.create ();
      descriptors = Hashtbl.create 8;
      descriptor_reads = Dynarray.create ();
      commits = Dynarray.create ();
      recorded = Hashtbl.create 8;
      violations = Dynarray.create ();
      hooks = no_hooks;
      peer = None;
    }
  in
  kb.peer <- Some (World.responder world answered_channels (answer kb));
  kb

(* What a failing case shows of this knowledge base: what its record was
   committed and its dispatcher claimed from, and what its gateway
   wrote. *)
let kb_account kb =
  let commit c =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("resourceId", str c.committed_to);
           ("jobId", c.committed_job);
           ("annotations", `List c.annotations);
           ("by", c.committed_by);
         ])
  in
  let claim c =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("by", c.claimed_by);
           ("roles", c.roles);
           ("accepts", `List c.accepts);
           ( "answered",
             match c.answered with Some id -> str id | None -> `Null );
         ])
  in
  [ Printf.sprintf "  %s's record was committed:" (domain kb) ]
  @ List.map (fun c -> "    " ^ commit c) (Dynarray.to_list kb.commits)
  @ [ Printf.sprintf "  %s's dispatcher was claimed from:" (domain kb) ]
  @ List.map (fun c -> "    " ^ claim c) (Dynarray.to_list kb.claims)
  @ [ Printf.sprintf "  %s's gateway wrote:" (domain kb) ]
  @ List.map
      (fun line -> "    " ^ line)
      (Process.output kb.world.gateway.process)

(* What went wrong here that no case may let pass, and forget what the case
   scripted: a frame outside the spec, an Archivist call or answer outside
   its own, anything the suite itself handed a worker outside the spec, and
   a grant the issuer was told to fail that no grant met. *)
let forget kb =
  let violations = Dynarray.to_list kb.violations @ World.drain kb.world in
  Dynarray.clear kb.violations;
  let issuer = kb.world.issuer in
  let unmet = List.of_seq (Queue.to_seq issuer.failing) in
  Queue.clear issuer.failing;
  Dynarray.clear issuer.met;
  let shown = function
    | Issuer.Unavailable -> "unavailable"
    | Unanswered -> "unanswered"
  in
  let violations =
    violations
    @
    match unmet with
    | [] -> []
    | unmet ->
        [
          Printf.sprintf "%s's issuer was told to fail grants no grant met: %s"
            (domain kb)
            (String.concat ", " (List.map shown unmet));
        ]
  in
  Dynarray.clear kb.queue;
  Dynarray.clear kb.claims;
  Hashtbl.reset kb.descriptors;
  Dynarray.clear kb.descriptor_reads;
  Dynarray.clear kb.commits;
  Hashtbl.reset kb.recorded;
  kb.hooks <- no_hooks;
  let archivist = kb.world.archivist in
  Dynarray.clear archivist.uploads;
  Dynarray.clear archivist.calls;
  Hashtbl.reset archivist.anchored_texts;
  archivist.mode.refuse_uploads <- None;
  violations

(* ── A running worker, as one knowledge base it serves meets it ─────── *)

type served = {
  worker : Worker_service.t;
  proxy : Client_proxy.t;  (** In front of this knowledge base's gateway. *)
  proxies : (kb * Client_proxy.t) list;
      (** In front of the gateway of each knowledge base the worker serves. *)
}

(* The same worker, as [kb] meets it. *)
let at served kb =
  match List.find_opt (fun (k, _) -> k == kb) served.proxies with
  | Some (_, proxy) -> { served with proxy }
  | None -> failwith ("the worker does not serve " ^ domain kb)

let requests served = Dynarray.to_list served.proxy.requests

(* The token a request carried as its bearer. *)
let bearer_of (r : Client_proxy.request) =
  Option.map
    (fun value ->
      if String.starts_with ~prefix:"Bearer " value then
        String.sub value 7 (String.length value - 7)
      else value)
    (Http.Header.get r.headers "authorization")

type sign_in = { signed_in_as : string; token : string }

(* Each sign-in the gateway answered, in order: the DID it answered
   GET /api/users/me with, and the token the worker carried. *)
let sign_ins served =
  List.filter_map
    (fun (r : Client_proxy.request) ->
      match (r.meth, r.path, r.status, r.answer, bearer_of r) with
      | "GET", "/api/users/me", Some 200, Some answer, Some token -> (
          match member "did" answer with
          | `String did -> Some { signed_in_as = did; token }
          | _ -> None)
      | _ -> None)
    (requests served)

(* Every token the worker carried here as its bearer. *)
let tokens served =
  List.sort_uniq compare (List.filter_map bearer_of (requests served))

(* The DID the gateway named the bearer of [r]'s token by, when it signed in
   with it. *)
let by served r =
  match bearer_of r with
  | None -> None
  | Some bearer ->
      Option.map
        (fun s -> s.signed_in_as)
        (List.find_opt (fun s -> s.token = bearer) (sign_ins served))

let is_object = function Some (`Assoc _) -> true | _ -> false

type opened = { opened_by : string option; body : Yojson.Safe.t; closed : bool }

(* Every stream opened: who opened it, what it names, and whether it has
   ended. *)
let streams served =
  List.filter_map
    (fun (r : Client_proxy.request) ->
      match (r.meth, r.path, r.json) with
      | "POST", "/bus/subscribe", Some (`Assoc _ as body) ->
          Some { opened_by = by served r; body; closed = r.closed }
      | _ -> None)
    (requests served)

type carried = { frame : Yojson.Safe.t; carried_at : int }

(* The frames on [channel] the worker's streams carried to it, in the order
   the proxy read them. *)
let carried served channel =
  List.concat_map
    (fun (r : Client_proxy.request) ->
      if r.meth = "POST" && r.path = "/bus/subscribe" then
        List.filter_map
          (fun (event : Client_proxy.event) ->
            match
              if event.event = "bus-event" then Call.parsed event.data else None
            with
            | Some (`Assoc _ as frame) when member "channel" frame = str channel
              ->
                Some { frame; carried_at = event.at }
            | _ -> None)
          (Dynarray.to_list r.events)
      else [])
    (requests served)

(* One POST /bus/emit a worker made. *)
type emitted = {
  channel : string;
  payload : Yojson.Safe.t;
  correlation_id : string option;
  scope : string option;
  emitted_by : string option;
      (** The DID the gateway named the bearer of its token by. *)
  status : int option;  (** What the gateway answered it with. *)
  arrived : int;  (** On the clock the proxy's requests and events share. *)
}

let emit_of served (r : Client_proxy.request) =
  match (r.meth, r.path, r.json) with
  | "POST", "/bus/emit", Some (`Assoc _ as body) -> (
      let text name =
        match member name body with `String s -> Some s | _ -> None
      in
      match (member "channel" body, member "payload" body) with
      | `String channel, (`Assoc _ as payload) ->
          Some
            {
              channel;
              payload;
              correlation_id = text "correlationId";
              scope = text "scope";
              emitted_by = by served r;
              status = r.status;
              arrived = r.arrived;
            }
      | _ -> None)
  | _ -> None

(* Every emit, in the order the gateway received them; of one channel, when
   named. *)
let emits ?channel served =
  List.filter_map
    (fun r ->
      match emit_of served r with
      | Some e when channel = None || channel = Some e.channel -> Some e
      | _ -> None)
    (requests served)

(* The payloads emitted on [channel], in order. *)
let payloads served channel =
  List.map (fun e -> e.payload) (emits ~channel served)

(* The [count]th emit on [channel] that [matches] accepts, once the gateway
   has answered it. *)
let emitted ?(matches = fun _ -> true) ?(count = 1) ?(timeout = 20.0) served
    channel =
  Client_proxy.until ~timeout served.proxy
    (Printf.sprintf "emit %d on %s" count channel) (fun () ->
      List.nth_opt
        (List.filter
           (fun e -> e.status <> None && matches e)
           (emits ~channel served))
        (count - 1))

(* What a worker says on a knowledge base's bus as it starts there: which
   knowledge base this is, and what the agent is called. *)
let starting = [ "browse:kb-requested"; "principal:profile" ]

(* What the worker asked of the gateway, in order, by name: emit <channel>,
   GET <path>, POST <path>. Progress reports are left out: a worker does not
   wait for the gateway to take one before it goes on, so where they fall
   among the rest is not fixed. Sign-ins, streams, the question of which
   knowledge base this is and the agent's saying what it is called are left
   out too: what a worker says as it starts. *)
let sequence served =
  List.filter_map
    (fun (r : Client_proxy.request) ->
      if r.path = "/api/users/me" || r.path = "/bus/subscribe" then None
      else
        match emit_of served r with
        | Some e ->
            if List.mem e.channel starting || e.channel = "job:report-progress"
            then None
            else Some ("emit " ^ e.channel)
        | None -> Some (r.meth ^ " " ^ r.path))
    (requests served)

(* Progress reports in a fixed order: by percentage, then by what they
   say. *)
let sorted_progress reports =
  let key p =
    let percentage =
      match member "percentage" p with
      | `Int n -> string_of_int n
      | other -> Yojson.Safe.to_string other
    in
    Printf.sprintf "%6s %s"
      (String.make (max 0 (6 - String.length percentage)) '0' ^ percentage)
      (Yojson.Safe.to_string (Check.canonical p))
  in
  List.sort (fun a b -> compare (key a) (key b)) reports

(* The progress reports of [job_id], in the order of [sorted_progress]. *)
let progress served job_id =
  sorted_progress
    (List.filter
       (fun p -> member "jobId" p = str job_id)
       (payloads served "job:report-progress"))

let clipped json =
  let text = Yojson.Safe.to_string json in
  if String.length text > 300 then String.sub text 0 300 else text

(* Every request the gateway refused. *)
let refused_requests served =
  List.filter_map
    (fun (r : Client_proxy.request) ->
      match r.status with
      | Some status when status >= 400 ->
          Some
            (Printf.sprintf "%s %s answered %d: %s → %s" r.meth r.path status
               (clipped (Option.value r.json ~default:`Null))
               (clipped (Option.value r.answer ~default:`Null)))
      | _ -> None)
    (requests served)

(* What it asked of this knowledge base's gateway, for a failing case. *)
let served_account served =
  List.map
    (fun (r : Client_proxy.request) ->
      let what =
        match emit_of served r with
        | Some e -> "emit " ^ e.channel ^ " " ^ Yojson.Safe.to_string e.payload
        | None when r.path = "/bus/subscribe" ->
            "subscribe "
            ^ Yojson.Safe.to_string (Option.value r.json ~default:`Null)
        | None -> r.meth ^ " " ^ r.path
      in
      Printf.sprintf "    [%s] %s"
        (match r.status with
        | Some status -> string_of_int status
        | None -> "unanswered")
        what)
    (requests served)

(* ── The world ──────────────────────────────────────────────────────── *)

(* A case's world: its own knowledge base, which every case's worker
   serves, the second a case of a worker of two has it serve beside the
   first, the providers both share, and the workers the case started. *)
type t = {
  own : kb;
  gateway : string list;
  command : string list;
  catalogue : string;
  ollama : Ollama.t;
  anthropic : Anthropic.t;
  env : (string * string) list;
      (** The key the worker signs in with at every knowledge base. *)
  public_key : Mirage_crypto_pk.Rsa.pub;
      (** Which each knowledge base's issuer holds on the worker's client there.
      *)
  mutable second : kb option;
  served : served Dynarray.t;
}

let python (implementations : Implementations.t) =
  match List.assoc_opt "python" implementations.workers with
  | Some command -> command
  | None ->
      failwith
        "the worker-service suite starts the Python Worker service, and the \
         implementations file does not name it"

let create (implementations : Implementations.t) =
  let command = python implementations in
  let catalogue =
    match implementations.model_catalogue with
    | Some file -> file
    | None ->
        failwith
          "a worker's document names a model catalogue, and the \
           implementations file names none (modelCatalogue)"
  in
  (* The worker proves itself with a key: each issuer holds the public half,
     the worker's environment the private. *)
  Lazy.force Fresh.seeded;
  let private_key = Mirage_crypto_pk.Rsa.generate ~bits:2048 () in
  let public_key = Mirage_crypto_pk.Rsa.pub_of_priv private_key in
  let own =
    stand_in ~gateway:implementations.gateway
      (Protocol.kb_identity Protocol.suite_kb)
      worker_client public_key
  in
  let ollama = Ollama.start () in
  let anthropic =
    Anthropic.start
      ~api_key:("sk-ant-conformance-" ^ Fresh.hex 24)
      ~models:(List.map (fun e -> e.model) anthropic_engines)
      ~facts:anthropic_model ()
  in
  {
    own;
    gateway = implementations.gateway;
    command;
    catalogue;
    ollama;
    anthropic;
    env =
      [
        ( "XYNOLOG_OIDC_CLIENT_KEY",
          X509.Private_key.encode_pem (`RSA private_key) );
      ];
    public_key;
    second = None;
    served = Dynarray.create ();
  }

(* The second knowledge base: another domain, a gateway and an issuer of
   its own, the worker's key held there on [second_worker_client]. Made when
   a case first asks for it, and kept for the file's other cases. *)
let second t =
  match t.second with
  | Some kb -> kb
  | None ->
      let kb =
        stand_in ~gateway:t.gateway
          (Protocol.kb_identity second_kb)
          second_worker_client t.public_key
      in
      t.second <- Some kb;
      kb

(* An entry of a knowledge base's engines: [engine], used for [jobs], on the
   stand-in of its provider. An Anthropic engine's entry names the variable
   its key is in. *)
let entry t engine jobs =
  `Assoc
    ([
       ( "engine",
         `Assoc
           [ ("provider", str engine.provider); ("model", str engine.model) ] );
       ("jobs", `List jobs);
     ]
    @
    if engine.provider = "anthropic" then
      [
        ("baseUrl", str t.anthropic.origin);
        ("apiKeyEnv", str anthropic_key_variable);
      ]
    else [ ("baseUrl", str t.ollama.origin) ])

(* What a knowledge base's worker runs unless a case says otherwise: one
   engine, the first, used for every job. *)
let everything t =
  [ entry t (List.hd ollama_engines) (Dispatcher_world.every_job ()) ]

(* A worker's configuration document, serving [knowledge_bases]. *)
let settings t knowledge_bases =
  `Assoc
    [
      ("knowledgeBases", `List knowledge_bases);
      ("modelCatalogue", str t.catalogue);
      ("port", `Int (Net.free_port ()));
      ("logLevel", str "info");
      ("logFormat", str "json");
    ]

(* A launch of a worker of this world's knowledge base, for a case that
   must change it: the gateway is dialled directly. *)
let launch t =
  Worker_service.launched ~command:t.command
    ~settings:
      (settings t
         [ configuration t.own (World.origin t.own.world) (everything t) ])
    t.env

(* Start a worker that must refuse to start. *)
let refused ?timeout launch = Worker_service.refused ?timeout launch

(* [base] with [changes] over it: a variable with a value is set, one with
   none is withheld. *)
let changed_env base changes =
  List.filter (fun (name, _) -> not (List.mem_assoc name changes)) base
  @ List.filter_map
      (fun (name, value) -> Option.map (fun v -> (name, v)) value)
      changes

(* Start a worker serving this world's knowledge base, with [engines] there,
   and each of [serving] after it, behind a recording proxy in front of each
   gateway, and go on without waiting for its health. A knowledge base of
   [serving] is signed in at as its client unless an entry names another.
   [document] changes the settings before it starts. It is stopped when the
   case ends. Each variable its document names for a key holds the stand-in
   Anthropic's. *)
let spawn ?engines ?(serving = []) ?(env = []) ?(unlisted = [])
    ?(document = Fun.id) t =
  let own = Client_proxy.start (World.origin t.own.world) in
  let proxies = ref [ (t.own, own) ] in
  try
    let entries =
      configuration t.own own.origin
        (match engines with Some e -> e | None -> everything t)
      :: List.map
           (fun (kb, engines, client) ->
             let proxy = Client_proxy.start (World.origin kb.world) in
             proxies := !proxies @ [ (kb, proxy) ];
             configuration ?client kb proxy.origin engines)
           serving
    in
    let settings = document (settings t entries) in
    let keys =
      List.map
        (fun variable -> (variable, Some t.anthropic.api_key))
        (Worker_service.named settings)
    in
    let worker =
      Worker_service.spawn
        (Worker_service.launched ~unlisted ~command:t.command ~settings
           (changed_env t.env (keys @ env)))
    in
    let served = { worker; proxy = own; proxies = !proxies } in
    Dynarray.add_last t.served served;
    served
  with failure ->
    List.iter (fun (_, proxy) -> Client_proxy.close proxy) !proxies;
    raise failure

(* Start a worker as [spawn] does, and wait for its health. *)
let start ?engines ?serving ?env ?unlisted ?document t =
  let served = spawn ?engines ?serving ?env ?unlisted ?document t in
  Worker_service.healthy served.worker;
  served

(* ── The end of a case ──────────────────────────────────────────────── *)

(* What a failing case shows: every worker's transcript at each knowledge
   base, what it wrote, what its providers were asked, and each knowledge
   base's record, dispatcher and gateway. *)
let account t =
  let abandoned (request : Serve.request) =
    if request.abandoned then " (abandoned)" else ""
  in
  String.concat "\n"
    (List.concat_map
       (fun served ->
         List.concat_map
           (fun (kb, _) ->
             Printf.sprintf "  the worker asked %s's gateway:" (domain kb)
             :: served_account (at served kb))
           served.proxies
         @ "  the worker wrote:"
           :: List.map
                (fun line -> "    " ^ line)
                (Process.tail 80 served.worker.process))
       (Dynarray.to_list t.served)
    @ "  the stand-in Ollama was asked:"
      :: List.map
           (fun (body, request) ->
             "    " ^ Yojson.Safe.to_string body ^ abandoned request)
           (Dynarray.to_list t.ollama.generations)
    @ "  the stand-in Anthropic was asked:"
      :: List.map
           (fun (r : Anthropic.request) ->
             Printf.sprintf "    %s %s %s%s" r.meth r.path
               (Yojson.Safe.to_string (Option.value r.body ~default:`Null))
               (abandoned r.served))
           (Dynarray.to_list t.anthropic.requests)
    @ kb_account t.own
    @ match t.second with Some kb -> kb_account kb | None -> [])

(* Stop every worker the case started and forget what the case scripted:
   what went wrong that no case may let pass. That is a request of a
   gateway that it refused, unless [allow_refused], a request of a provider
   no case scripted, and what each knowledge base's [forget] returns. *)
let settle ?(allow_refused = false) t =
  let workers = Dynarray.to_list t.served in
  Dynarray.clear t.served;
  let refusals =
    List.concat_map
      (fun served ->
        let refusals =
          if allow_refused then []
          else
            List.concat_map
              (fun (kb, _) -> refused_requests (at served kb))
              served.proxies
        in
        ignore (Worker_service.stop served.worker : int option);
        List.iter (fun (_, proxy) -> Client_proxy.close proxy) served.proxies;
        refusals)
      workers
  in
  let violations =
    refusals
    @ Dynarray.to_list t.ollama.violations
    @ Dynarray.to_list t.anthropic.violations
    @ forget t.own
    @ match t.second with Some kb -> forget kb | None -> []
  in
  Ollama.reset t.ollama;
  Anthropic.reset t.anthropic;
  violations

let close t =
  ignore (settle ~allow_refused:true t : string list);
  Ollama.close t.ollama;
  Anthropic.close t.anthropic;
  Option.iter (fun kb -> World.close kb.world) t.second;
  World.close t.own.world

(* The end of a case: its account shown when it failed or settling found
   anything, and a failure when settling found anything. *)
let end_case ?allow_refused t body =
  World.end_case
    ~account:(fun () -> account t)
    ~settle:(fun () -> settle ?allow_refused t)
    body
