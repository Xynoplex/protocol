(* What every response carries (contract/TRANSPORT-HTTP.md § Every
   response, as gateway/response-headers.json states it), and the routes
   that describe the gateway and the knowledge base. *)

open Xynolog_conformance
open Support

let table () = (Protocol.tables ()).gateway_response_headers
let header name (reply : Call.reply) = Http.Header.get reply.headers name

(* How [headers] are not what every response carries: each header of
   [every] with its value, the request id, and none of [absent]. *)
let edge_problems label headers =
  let table = table () in
  List.filter_map
    (fun (row : Xynolog_protocol.Tables.Gateway_response_headers.every_item) ->
      match Http.Header.get headers row.name with
      | Some value when value = row.value -> None
      | value ->
          Some
            (Printf.sprintf "%s: %s is %s" label row.name
               (match value with
               | Some v -> Printf.sprintf "%S" v
               | None -> "absent")))
    table.every
  @ (match Http.Header.get headers table.request_id with
    | Some _ -> []
    | None -> [ Printf.sprintf "%s: no %s" label table.request_id ])
  @ List.filter_map
      (fun name ->
        Option.map
          (fun value -> Printf.sprintf "%s: %s is %S" label name value)
          (Http.Header.get headers name))
      table.absent

let security_headers (w : World.t) =
  let token = World.person w "edge" in
  let id = "res-" ^ Fresh.uuid () in
  Hashtbl.replace w.archivist.resources id
    { storage_uri = "file://" ^ id; media_type = "text/plain" };
  Hashtbl.replace w.archivist.content ("file://" ^ id) "bytes";
  let origin = World.origin w in
  let replies =
    [
      ( "health",
        Call.call origin "GET" (Protocol.path Gateway "GET" "/api/health") );
      ( "a 401",
        Call.call origin "GET" (Protocol.path Gateway "GET" "/api/users/me") );
      ("a 404", Call.call origin "GET" "/no/such/path");
      ( "content bytes",
        Call.call ~token origin "GET"
          (Protocol.path_of Gateway "GET" "/resources/{id}" [ ("id", id) ]) );
      ("a 400", World.emit w token (`Assoc []));
    ]
  in
  let problems =
    List.concat_map
      (fun (label, (reply : Call.reply)) -> edge_problems label reply.headers)
      replies
  in
  let stream_headers =
    Eio.Switch.run @@ fun sw ->
    let response, _ =
      Call.exchange ~sw
        ~headers:
          [
            ("authorization", "Bearer " ^ token);
            ("content-type", "application/json");
          ]
        ~body:
          (Yojson.Safe.to_string
             (Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
                ~global:[ "beckon:focus" ] ()))
        "POST"
        (origin ^ Protocol.path Gateway "POST" "/bus/subscribe")
    in
    Http.Response.headers response
  in
  let problems = problems @ edge_problems "the stream" stream_headers in
  Check.equal "what every response carries" (`List [])
    (`List (List.map (fun p -> `String p) problems));
  let ids =
    List.sort_uniq compare
      (List.map (fun (_, r) -> header (table ()).request_id r) replies)
  in
  Check.that "each response names its own request"
    (List.length ids = List.length replies)

let any_origin (w : World.t) =
  let table = table () in
  let preflight =
    Call.call
      ~headers:
        [
          ("origin", "https://anywhere.example");
          ("access-control-request-method", "POST");
          ("access-control-request-headers", "authorization, content-type");
        ]
      (World.origin w) "OPTIONS"
      (Protocol.path Gateway "POST" "/bus/emit")
  in
  Check.that
    (Printf.sprintf "the preflight is answered with a success, not %d"
       preflight.status)
    (preflight.status < 300);
  let allow_origin =
    List.find
      (fun (row : Xynolog_protocol.Tables.Gateway_response_headers.every_item)
         -> row.name = "Access-Control-Allow-Origin")
      table.every
  in
  let open_and_credential_less what reply =
    Check.equal
      (what ^ ": its " ^ allow_origin.name)
      (`String allow_origin.value)
      (match header allow_origin.name reply with
      | Some v -> `String v
      | None -> `Null);
    List.iter
      (fun name ->
        Check.that (what ^ ": no " ^ name) (header name reply = None))
      table.absent
  in
  open_and_credential_less "the preflight" preflight;
  open_and_credential_less "a simple request"
    (Call.call
       ~headers:[ ("origin", "https://anywhere.example") ]
       (World.origin w) "GET"
       (Protocol.path Gateway "GET" "/api/health"))

let no_docs_ui (w : World.t) =
  List.iter
    (fun path ->
      let reply =
        Call.call
          ~headers:[ ("accept", "text/html") ]
          (World.origin w) "GET" path
      in
      status path 404 reply;
      Check.that
        (Printf.sprintf "%s: %s" path (Call.shown reply.text))
        (match reply.json with
        | Some json -> Protocol.against "ErrorResponse" json = []
        | None -> false))
    [ "/api"; "/api/docs"; "/api/swagger" ]

let openapi_document (w : World.t) =
  let health =
    Call.call (World.origin w) "GET" (Protocol.path Gateway "GET" "/api/health")
  in
  let doc =
    Call.call (World.origin w) "GET"
      (Protocol.path Gateway "GET" "/api/openapi.json")
  in
  status "the OpenAPI document" 200 doc;
  conforms "GET" "/api/openapi.json" doc;
  let published = Option.get doc.json in
  Check.equal "its version"
    (Check.at (Option.get health.json) [ "version" ])
    (Check.at published [ "info"; "version" ]);
  Check.equal "its servers"
    (`List [ Check.at (World.document w) [ "publicUrl" ] ])
    (`List
       (List.map
          (fun server -> Check.at server [ "url" ])
          (Check.list published [ "servers" ])))

let resource_metadata (w : World.t) =
  let reply =
    Call.call (World.origin w) "GET"
      (Protocol.path Gateway "GET" "/.well-known/oauth-protected-resource")
  in
  status "the resource metadata" 200 reply;
  conforms "GET" "/.well-known/oauth-protected-resource" reply;
  Check.equal "the resource metadata"
    (`Assoc
       [
         ("resource", `String w.kb.resource);
         ("authorization_servers", `List [ `String w.issuer.origin ]);
         ("bearer_methods_supported", `List [ `String "header" ]);
         ("resource_name", `String w.kb.name);
       ])
    (Option.get reply.json)

let challenge (w : World.t) =
  let reply =
    Call.call (World.origin w) "GET" (Protocol.path Gateway "GET" "/api/status")
  in
  Check.equal "the challenge"
    (`String (Protocol.challenge_no_credential (World.origin w)))
    (match header "www-authenticate" reply with
    | Some v -> `String v
    | None -> `Null)

let status_route (w : World.t) =
  let token = World.person w "status-reader" in
  let before = Dynarray.length w.archivist.calls in
  let reply =
    Call.call ~token (World.origin w) "GET"
      (Protocol.path Gateway "GET" "/api/status")
  in
  status "the status" 200 reply;
  conforms "GET" "/api/status" reply;
  Check.equal "who it is asked by" (`String "status-reader@people.example")
    (Check.at (Option.get reply.json) [ "authenticatedAs" ]);
  Check.that "it asked the Archivist nothing"
    (Dynarray.length w.archivist.calls = before)

let tests implementations =
  each_plane ~make:(plain implementations) ~world:Fun.id ~close:World.close
    "every response"
    [
      case
        "carries the security headers and a request id — success, error, bytes \
         and stream alike"
        security_headers;
      case "allows any origin, and never credentials" any_origin;
      case
        "the gateway hosts no documentation UI: the OpenAPI document is what \
         it publishes"
        no_docs_ui;
      case
        "the OpenAPI document names this build's version and this gateway's \
         public URL"
        openapi_document;
      case
        "the resource metadata names this knowledge base, its issuer, and the \
         header a token rides in"
        resource_metadata;
      case
        "the challenge on a 401 points at the resource metadata on the origin \
         the caller reached"
        challenge;
      case
        "the status names the gateway and who is asking, and asks the \
         Archivist nothing"
        status_route;
    ]
