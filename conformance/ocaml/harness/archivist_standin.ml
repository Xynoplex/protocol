(* A stand-in Archivist, behind the gateway of a world: the Archivist's HTTP
   surface (specs/src/archivist/openapi.json) as far as a world with no
   record of its own answers it. It answers a read of a resource's anchored
   text as a world says, and every resource it is asked of otherwise as one
   it does not hold. It checks the caller's token as the real one does — an
   issuer token carrying the service role — and holds every request the
   gateway sends, and every reply it gives, to the Archivist's spec: anything
   else lands in [violations], which fail the case.

   Partly ported: the TypeScript stand-in also records uploads, serves bytes
   and replays events a case gives it; no case ported here asks it to. *)

type t = {
  server : Serve.t;
  host : string;
  port : int;
  violations : string Dynarray.t;
  anchored_texts : (string, unit -> Yojson.Safe.t) Hashtbl.t;
      (** What a read of a resource's anchored text is answered, asked afresh at
          each read. A resource with none is answered [unknown]. *)
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
          match Protocol.refusals p.schema (`String value) with
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

let start (issuer : Issuer.t) =
  let violations = Dynarray.create () in
  let anchored_texts = Hashtbl.create 16 in
  let authorized (request : Serve.request) =
    match Http.Header.get request.headers "authorization" with
    | Some header when String.starts_with ~prefix:"Bearer " header -> (
        match
          Issuer.verified issuer
            (String.sub header 7 (String.length header - 7))
        with
        | Some jwt -> (
            match Jose.Jwt.get_yojson_claim jwt "roles" with
            | Some (`List roles) ->
                List.mem (`String (Protocol.service_role ())) roles
            | _ -> false)
        | None -> false)
    | _ -> false
  in
  let handler (request : Serve.request) =
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
        let reply status body =
          let answer = Serve.json_reply status body in
          let checked =
            Call.non_conformance Archivist request.meth template
              {
                status;
                headers = Http.Header.of_list answer.headers;
                text = answer.body;
                json = Some body;
              }
          in
          List.iter
            (fun problem ->
              Dynarray.add_last violations
                (Printf.sprintf "the stand-in Archivist, %s %s: %s" request.meth
                   request.path problem))
            checked;
          Serve.Reply answer
        in
        let id () = snd (List.hd values) in
        if operation.security <> [] && not (authorized request) then
          reply 401 (`Assoc [ ("error", `String "unauthorized") ])
        else
          match template with
          | "/resources/{id}/anchored-text" ->
              reply 200
                (match Hashtbl.find_opt anchored_texts (id ()) with
                | Some answer -> answer ()
                | None -> `Assoc [ ("kind", `String "unknown") ])
          | "/resources/{id}/jsonld" | "/resources/{id}/annotations" ->
              reply 404 (`Assoc [ ("error", `String "Resource not found") ])
          | "/resources/{id}/content" ->
              reply 404
                (`Assoc
                   [
                     ("error", `String ("Resource not found: " ^ id ()));
                     ("code", `String "resource");
                   ])
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
  { server; host = "127.0.0.1"; port = server.port; violations; anchored_texts }

let close t = t.server.stop ()
