(* A stand-in Archivist: the knowledge base's record and bytes, over the
   HTTP surface the Archivist serves (specs/src/archivist/openapi.json) —
   the recording upload, which answers a repeat by its Idempotency-Key, byte
   reads by resource, the linked-data description, a resource's annotations,
   its anchored text, and the sequence-ranged event read that backs replay.
   It checks the caller's token as the real one does (an issuer token
   carrying the service role) and records every call, so a case can see what
   the gateway sent it.

   It is held to the Archivist's spec, as the real Archivist's own tests
   are: every request the gateway sends must be a declared operation
   carrying what the operation requires, and every reply the stand-in gives
   must match its declaration. Anything else lands in [violations], which
   fail the case. *)

type call = {
  meth : string;
  path : string;  (** With its query. *)
  claims : Jose.Jwt.t option;
  headers : Http.Header.t;
}

(* An upload the stand-in stored and recorded: the multipart fields, the
   file, who the gateway named, and the key it forwarded. *)
type upload = {
  fields : (string * string) list;
  file : string;
  principal : string;
  roles : string option;
  key : string;
  resource_id : string;
}

type stored = { storage_uri : string; media_type : string }

(* Behaviour switches a case flips. *)
type mode = {
  mutable refuses_gateway : bool;
      (** Every token is refused: the gateway's service account has lost the
          Archivist. *)
  mutable replay_delay : float;  (** The event read waits this long, s. *)
  mutable replay_fails : bool;  (** The event read answers 500. *)
  mutable replay_answer : Yojson.Safe.t option;
      (** The event read answers this body instead, whatever it is — for a case
          that needs the Archivist to answer outside its spec. It lands in
          [violations] like any off-spec reply; the case expects that, and
          clears it. *)
  mutable refuse_uploads : string option;
      (** An upload is refused by the record: answered 500 with this reason. *)
  mutable anchored_text_fails : bool;
      (** A read of anchored text answers 500. *)
  mutable hangs_up : int;
      (** This many requests, from the next, are recorded and read whole, and
          then their connection is closed with no answer. *)
  records_unanswered : (string, unit) Hashtbl.t;
      (** An upload of a storageUri here is stored and recorded, once, and then
          never answered: a resource made whose id the gateway was never told.
          Its repeat is answered. *)
  unanswered : (string, unit) Hashtbl.t;
      (** A read of a resource id, or an upload of a storageUri, here is read
          whole and never answered. *)
  answered_in_part : (string, unit) Hashtbl.t;
      (** A read of the bytes of a resource id here is answered its head and the
          first half of its bytes, and then nothing. *)
}

type t = {
  server : Serve.t;
  host : string;
  port : int;
  calls : call Dynarray.t;
  violations : string Dynarray.t;
      (** Everything the gateway or the stand-in did outside the Archivist's
          spec. *)
  content : (string, string) Hashtbl.t;  (** Bytes by storageUri. *)
  resources : (string, stored) Hashtbl.t;
  events : (string, Yojson.Safe.t list) Hashtbl.t;
      (** The record's events, per resource, as StoredEventResponse. *)
  descriptions : (string, Yojson.Safe.t) Hashtbl.t;
  annotations : (string, unit -> Yojson.Safe.t option) Hashtbl.t;
      (** What a read of a resource's annotations is answered, asked afresh at
          each read. None, or no entry, is answered 404. *)
  anchored_texts : (string, unit -> Yojson.Safe.t) Hashtbl.t;
      (** What a read of a resource's anchored text is answered, asked afresh at
          each read. A resource with none is answered [unknown], as the
          Archivist answers a resource it does not hold. *)
  uploads : upload Dynarray.t;
  mode : mode;
}

let request_problems (operation : Openapi.operation) (request : Serve.request) =
  let where = Printf.sprintf "the gateway's %s %s" request.meth request.path in
  List.filter_map
    (fun (p : Openapi.parameter) ->
      let value =
        match p.in_ with
        | Header -> Http.Header.get request.headers p.name
        | Query ->
            Option.bind (List.assoc_opt p.name request.query) (function
              | v :: _ -> Some v
              | [] -> None)
        | Path -> Some "path"
      in
      match (value, p.in_) with
      | None, _ when p.required ->
          Some (Printf.sprintf "%s carries no parameter %s" where p.name)
      | Some value, (Header | Query) -> (
          let typed =
            match int_of_string_opt value with
            | Some n when Protocol.refusals p.schema (`Int n) = [] -> `Int n
            | _ -> `String value
          in
          match Protocol.refusals p.schema typed with
          | [] -> None
          | refusals ->
              Some
                (Printf.sprintf "%s: %s=%s (%s)" where p.name value
                   (String.concat "; " refusals)))
      | _ -> None)
    (Option.value ~default:[] operation.parameters)
  @
  if
    operation.security <> []
    && not
         (match Http.Header.get request.headers "authorization" with
         | Some h -> String.starts_with ~prefix:"Bearer " h
         | None -> false)
  then [ where ^ " carries no bearer token" ]
  else []

let upload_required () =
  match Protocol.component Archivist "ResourceUpload" with
  | Schema { required = Some required; _ } -> required
  | _ -> failwith "the Archivist's ResourceUpload requires nothing"

let start (issuer : Issuer.t) =
  let calls = Dynarray.create () in
  let violations = Dynarray.create () in
  let content = Hashtbl.create 16 in
  let resources = Hashtbl.create 16 in
  let events = Hashtbl.create 16 in
  let descriptions = Hashtbl.create 16 in
  let annotations = Hashtbl.create 16 in
  let anchored_texts = Hashtbl.create 16 in
  let uploads = Dynarray.create () in
  (* Each upload recorded, by its principal and its key: what a repeat is
     answered from. *)
  let keyed = Hashtbl.create 16 in
  let mode =
    {
      refuses_gateway = false;
      replay_delay = 0.;
      replay_fails = false;
      replay_answer = None;
      refuse_uploads = None;
      anchored_text_fails = false;
      hangs_up = 0;
      records_unanswered = Hashtbl.create 4;
      unanswered = Hashtbl.create 4;
      answered_in_part = Hashtbl.create 4;
    }
  in
  let events_per_answer =
    Protocol.limit Archivist "GET" "/events/{resourceId}" "maxEvents"
  in
  let authorized (request : Serve.request) =
    match Http.Header.get request.headers "authorization" with
    | Some header
      when String.starts_with ~prefix:"Bearer " header
           && not mode.refuses_gateway ->
        Option.bind
          (Issuer.verified issuer
             (String.sub header 7 (String.length header - 7)))
          (fun jwt ->
            match Jose.Jwt.get_yojson_claim jwt "roles" with
            | Some (`List roles)
              when List.mem (`String (Protocol.service_role ())) roles ->
                Some jwt
            | _ -> None)
    | _ -> None
  in
  let handler (request : Serve.request) =
    let claims = authorized request in
    let query =
      match request.query with
      | [] -> ""
      | query -> "?" ^ Uri.encoded_of_query query
    in
    Dynarray.add_last calls
      {
        meth = request.meth;
        path = request.path ^ query;
        claims;
        headers = request.headers;
      };
    match Protocol.route Archivist request.meth request.path with
    | None ->
        Dynarray.add_last violations
          (Printf.sprintf
             "the gateway called %s %s, which the Archivist's spec does not \
              declare"
             request.meth request.path);
        Serve.Reply
          (Serve.json_reply 404 (`Assoc [ ("error", `String "not found") ]))
    | Some (template, values) -> (
        let operation = Protocol.operation Archivist request.meth template in
        List.iter
          (Dynarray.add_last violations)
          (request_problems operation request);
        let answer (reply : Serve.reply) =
          List.iter
            (fun problem ->
              Dynarray.add_last violations
                (Printf.sprintf "the stand-in Archivist, %s %s: %s" request.meth
                   request.path problem))
            (Call.non_conformance Archivist request.meth template
               {
                 status = reply.status;
                 headers = Http.Header.of_list reply.headers;
                 text = reply.body;
                 json = Call.parsed reply.body;
               });
          Serve.Reply reply
        in
        let json ?(headers = []) status body =
          let reply = Serve.json_reply status body in
          answer { reply with headers = reply.headers @ headers }
        in
        let error ?code status text =
          json status
            (`Assoc
               ([ ("error", `String text) ]
               @
               match code with
               | Some c -> [ ("code", `String c) ]
               | None -> []))
        in
        let never () = Serve.Held (fst (Eio.Promise.create ())) in
        if mode.hangs_up > 0 then (
          mode.hangs_up <- mode.hangs_up - 1;
          Serve.Dropped)
        else if operation.security <> [] && claims = None then
          let presented =
            match Http.Header.get request.headers "authorization" with
            | Some h -> String.starts_with ~prefix:"Bearer " h
            | None -> false
          in
          json 401
            ~headers:
              [
                ( "www-authenticate",
                  if presented then "Bearer error=\"invalid_token\""
                  else "Bearer" );
              ]
            (`Assoc [ ("error", `String "unauthorized") ])
        else
          let id () = snd (List.hd values) in
          let header name = Http.Header.get request.headers name in
          match template with
          | "/resources" -> (
              match
                Option.bind (header "content-type") (fun kind ->
                    Call.parts kind request.body)
              with
              | None ->
                  Dynarray.add_last violations
                    "the gateway's POST /resources is not multipart/form-data";
                  error 400 "Not multipart/form-data"
              | Some parts -> (
                  let fields =
                    List.filter_map
                      (function
                        | name, None, value -> Some (name, value) | _ -> None)
                      parts
                  and file =
                    List.find_map
                      (function _, Some _, bytes -> Some bytes | _ -> None)
                      parts
                  in
                  let field name = List.assoc_opt name fields in
                  match field "storageUri" with
                  | Some uri when Hashtbl.mem mode.unanswered uri -> never ()
                  | storage_uri -> (
                      match
                        (header "xynolog-principal", header "idempotency-key")
                      with
                      | None, _ -> error 400 "Missing Xynolog-Principal"
                      | _, None -> error 400 "Missing Idempotency-Key"
                      | Some principal, Some key -> (
                          let missing =
                            List.filter
                              (fun name ->
                                if name = "file" then file = None
                                else
                                  match field name with
                                  | None | Some "" -> true
                                  | Some _ -> false)
                              (upload_required ())
                          in
                          if missing <> [] then
                            error 400
                              ("Missing required fields: "
                             ^ String.concat ", " missing)
                          else
                            let storage_uri = Option.get storage_uri
                            and format = Option.get (field "format")
                            and file = Option.get file in
                            let base =
                              String.lowercase_ascii
                                (String.trim
                                   (List.hd (String.split_on_char ';' format)))
                            in
                            if
                              Protocol.against "SupportedMediaType"
                                (`String base)
                              <> []
                            then error 400 ("Unsupported media type: " ^ base)
                            else
                              match Hashtbl.find_opt keyed (principal, key) with
                              | Some first ->
                                  if
                                    List.sort compare first.fields
                                    = List.sort compare fields
                                    && first.file = file
                                  then
                                    json 200
                                      (`Assoc
                                         [
                                           ( "resourceId",
                                             `String first.resource_id );
                                         ])
                                  else
                                    error ~code:"unprocessable" 422
                                      "This Idempotency-Key names a resource \
                                       recorded from other fields or other \
                                       bytes"
                              | None -> (
                                  match mode.refuse_uploads with
                                  | Some reason -> error 500 reason
                                  | None ->
                                      let resource_id =
                                        "res-" ^ Fresh.uuid ()
                                      in
                                      Hashtbl.replace content storage_uri file;
                                      Hashtbl.replace resources resource_id
                                        { storage_uri; media_type = format };
                                      let recorded =
                                        {
                                          fields;
                                          file;
                                          principal;
                                          roles = header "xynolog-roles";
                                          key;
                                          resource_id;
                                        }
                                      in
                                      Dynarray.add_last uploads recorded;
                                      Hashtbl.replace keyed (principal, key)
                                        recorded;
                                      if
                                        Hashtbl.mem mode.records_unanswered
                                          storage_uri
                                      then (
                                        Hashtbl.remove mode.records_unanswered
                                          storage_uri;
                                        never ())
                                      else
                                        json 200
                                          (`Assoc
                                             [
                                               ( "resourceId",
                                                 `String resource_id );
                                             ]))))))
          | _ when Hashtbl.mem mode.unanswered (id ()) -> never ()
          | "/resources/{id}/jsonld" -> (
              match Hashtbl.find_opt descriptions (id ()) with
              | None -> error 404 "Resource not found"
              | Some description ->
                  answer
                    {
                      status = 200;
                      headers =
                        [
                          ("content-type", "application/ld+json; charset=utf-8");
                        ];
                      body = Yojson.Safe.to_string description;
                    })
          | "/resources/{id}/annotations" -> (
              match
                Option.bind
                  (Hashtbl.find_opt annotations (id ()))
                  (fun held -> held ())
              with
              | None -> error 404 "Resource not found"
              | Some answered -> json 200 answered)
          | "/resources/{id}/anchored-text" ->
              if mode.anchored_text_fails then
                error 500 "anchored-text read failed"
              else
                json 200
                  (match Hashtbl.find_opt anchored_texts (id ()) with
                  | Some held -> held ()
                  | None -> `Assoc [ ("kind", `String "unknown") ])
          | "/resources/{id}/content" -> (
              match Hashtbl.find_opt resources (id ()) with
              | None ->
                  error ~code:"resource" 404 ("Resource not found: " ^ id ())
              | Some stored -> (
                  match Hashtbl.find_opt content stored.storage_uri with
                  | None ->
                      error ~code:"representation" 404
                        ("Resource representation not found: " ^ id ())
                  | Some bytes ->
                      let reply : Serve.reply =
                        {
                          status = 200;
                          headers = [ ("content-type", stored.media_type) ];
                          body = bytes;
                        }
                      in
                      if Hashtbl.mem mode.answered_in_part (id ()) then
                        Serve.Partly (reply, String.length bytes / 2)
                      else answer reply))
          | "/events/{resourceId}" -> (
              match
                Option.bind (List.assoc_opt "fromSequence" request.query)
                  (function
                  | v :: _ -> int_of_string_opt v
                  | [] -> None)
              with
              | Some from when from >= 1 -> (
                  if mode.replay_delay > 0. then Run.sleep mode.replay_delay;
                  if mode.replay_fails then error 500 "event read failed"
                  else
                    match mode.replay_answer with
                    | Some body -> json 200 body
                    | None ->
                        let stream =
                          Option.value ~default:[]
                            (Hashtbl.find_opt events (id ()))
                        in
                        let sequence event =
                          match
                            Protocol.at event [ "metadata"; "sequenceNumber" ]
                          with
                          | Some (`Int n) -> n
                          | _ ->
                              failwith "a stored event with no sequenceNumber"
                        in
                        let page =
                          List.filteri
                            (fun at _ -> at < events_per_answer)
                            (List.filter (fun e -> sequence e >= from) stream)
                        in
                        json 200
                          (`Assoc
                             [
                               ("events", `List page);
                               ( "lastSequence",
                                 `Int
                                   (List.fold_left
                                      (fun last e -> max last (sequence e))
                                      0 stream) );
                             ]))
              | _ -> error 400 "integer fromSequence >= 1 is required")
          | _ ->
              Dynarray.add_last violations
                (Printf.sprintf
                   "the stand-in Archivist serves no %s %s, which the gateway \
                    called"
                   request.meth template);
              Serve.Reply
                (Serve.json_reply 500
                   (`Assoc [ ("error", `String "not served by the stand-in") ]))
        )
  in
  let server = Serve.start handler in
  {
    server;
    host = "127.0.0.1";
    port = server.port;
    calls;
    violations;
    content;
    resources;
    events;
    descriptions;
    annotations;
    anchored_texts;
    uploads;
    mode;
  }

let close t = t.server.stop ()

(* A persisted event as the record stores it, for replay. *)
let stored_event type_ resource_id sequence =
  `Assoc
    [
      ("id", `String (Printf.sprintf "evt-%s-%d" resource_id sequence));
      ("type", `String type_);
      ("timestamp", `String "1970-01-01T00:00:00.000Z");
      ("userId", `String "did:web:record.example:users:someone");
      ("resourceId", `String resource_id);
      ("version", `Int 1);
      ("payload", `Assoc []);
      ("metadata", `Assoc [ ("sequenceNumber", `Int sequence) ]);
    ]
