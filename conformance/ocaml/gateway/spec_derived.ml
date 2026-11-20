(* Cases generated from the spec itself: every declared operation, probed
   the ways every operation can be probed. A route added to the spec is
   covered here without anyone writing a case for it. *)

open Xynolog_conformance
open Xynolog_protocol_schema
open Support

(* The methods a probe tries on a declared path. *)
let methods = [ "GET"; "PUT"; "POST"; "DELETE"; "PATCH" ]

(* A concrete path for an OpenAPI path: every {param} filled. *)
let concrete path =
  Re.replace_string
    (Re.compile (Re.Perl.re "\\{[^}]+\\}"))
    ~by:"conformance-probe" path

(* Every operation the gateway's document declares: its method, its path
   and the operation. *)
let operations () =
  List.concat_map
    (fun (path, _) ->
      List.filter_map
        (fun meth ->
          Option.map
            (fun op -> (meth, path, op))
            (Protocol.find_operation Gateway meth path))
        methods)
    (Protocol.document Gateway).paths

let is_public (op : Openapi.operation) = op.security = []

let takes_json (op : Openapi.operation) =
  match op.request_body with
  | Some body -> List.mem_assoc "application/json" body.content
  | None -> false

(* The headers an operation requires, as a client sends them: a new
   Idempotency-Key where it takes one. A probe is then refused for what it
   probes, and never for a header it left out. A required header no probe
   knows how to fill fails here, so one added to the spec is not missed. *)
let required_headers (op : Openapi.operation) =
  List.filter_map
    (fun (p : Openapi.parameter) ->
      match p.in_ with
      | Header when p.required ->
          if p.name <> "Idempotency-Key" then
            failwith
              (Printf.sprintf
                 "the spec requires a header %s that no probe fills" p.name);
          Some ("idempotency-key", Fresh.uuid ())
      | _ -> None)
    (Option.value ~default:[] op.parameters)

(* A request with a JSON body that is [text], as it is. *)
let with_json ?token ?(headers = []) origin meth path text =
  Call.call ?token ~body:text
    ~headers:(("content-type", "application/json") :: headers)
    origin meth path

let problems_of what (reply : Call.reply) meth path =
  List.map
    (fun p -> Printf.sprintf "%s: %s" what p)
    (Call.non_conformance Gateway meth path reply)

let no_problems problems =
  Check.equal "the problems found" (`List [])
    (`List (List.map (fun p -> `String p) problems))

let hint (reply : Call.reply) =
  match Option.bind reply.json (fun json -> Protocol.at json [ "hint" ]) with
  | Some (`String hint) -> Some hint
  | _ -> None

let no_credential (w : World.t) =
  let challenge = Protocol.challenge_no_credential (World.origin w) in
  let named = (Protocol.message "no-credential").hint in
  let problems =
    List.concat_map
      (fun (meth, path, op) ->
        if is_public op then []
        else
          (* Another scheme is no credential either; and a body that is not
             JSON would be a 400 if the gateway parsed it before
             authenticating. *)
          List.concat_map
            (fun (label, authorization) ->
              let headers =
                required_headers op
                @
                match authorization with
                | Some a -> [ ("authorization", a) ]
                | None -> []
              in
              let reply =
                if takes_json op then
                  with_json ~headers (World.origin w) meth (concrete path)
                    {|{"unterminated|}
                else Call.call ~headers (World.origin w) meth (concrete path)
              in
              let at = Printf.sprintf "%s %s with %s" meth path label in
              if reply.status <> 401 then
                [ Printf.sprintf "%s: %d, not 401" at reply.status ]
              else
                problems_of at reply meth path
                @ (if header "www-authenticate" reply <> Some challenge then
                     [
                       Printf.sprintf "%s: challenge %s" at
                         (Option.value ~default:"null"
                            (header "www-authenticate" reply));
                     ]
                   else [])
                @
                if hint reply <> named then
                  [
                    Printf.sprintf "%s: no hint naming the header: %s" at
                      reply.text;
                  ]
                else [])
            [
              ("no header", None); ("a Basic header", Some "Basic dXNlcjpwYXNz");
            ])
      (operations ())
  in
  no_problems problems

let unverifiable (w : World.t) =
  let challenge = Protocol.challenge_refused (World.origin w) in
  let problems =
    List.concat_map
      (fun (meth, path, op) ->
        if is_public op then []
        else
          let headers = required_headers op in
          let reply =
            if takes_json op then
              with_json ~token:"not-a-token" ~headers (World.origin w) meth
                (concrete path) "{}"
            else
              Call.call ~token:"not-a-token" ~headers (World.origin w) meth
                (concrete path)
          in
          let at = Printf.sprintf "%s %s" meth path in
          if reply.status <> 401 then
            [ Printf.sprintf "%s: %d, not 401" at reply.status ]
          else
            problems_of at reply meth path
            @
            if header "www-authenticate" reply <> Some challenge then
              [
                Printf.sprintf "%s: challenge %s" at
                  (Option.value ~default:"null"
                     (header "www-authenticate" reply));
              ]
            else [])
      (operations ())
  in
  no_problems problems

let public (w : World.t) =
  let problems =
    List.concat_map
      (fun (meth, path, op) ->
        if not (is_public op) then []
        else
          let reply = Call.call (World.origin w) meth (concrete path) in
          let at = Printf.sprintf "%s %s" meth path in
          (if reply.status <> 200 then
             [ Printf.sprintf "%s: %d" at reply.status ]
           else [])
          @ problems_of at reply meth path
          @
          match header "www-authenticate" reply with
          | Some challenge ->
              [
                Printf.sprintf "%s: a public route challenged: %s" at challenge;
              ]
          | None -> [])
      (operations ())
  in
  no_problems problems

(* A person who also holds the service role: every protected route admits
   them. *)
let service_person (w : World.t) sub =
  World.person w
    ~claims:[ ("roles", `List [ `String (Protocol.service_role ()) ]) ]
    sub

let is_error_response (reply : Call.reply) =
  match reply.json with
  | Some json -> Protocol.against "ErrorResponse" json = []
  | None -> false

let undeclared_method (w : World.t) =
  let token = service_person w "probe" in
  let problems =
    List.concat_map
      (fun (path, _) ->
        List.filter_map
          (fun meth ->
            if Protocol.find_operation Gateway meth path <> None then None
            else
              let reply =
                if meth = "GET" then
                  Call.call ~token (World.origin w) meth (concrete path)
                else
                  Call.call ~token ~json:(`Assoc []) (World.origin w) meth
                    (concrete path)
              in
              let at = Printf.sprintf "%s %s" meth path in
              if reply.status <> 404 then
                Some (Printf.sprintf "%s: %d, not 404" at reply.status)
              else if not (is_error_response reply) then
                Some (Printf.sprintf "%s: %s" at (Call.shown reply.text))
              else None)
          methods)
      (Protocol.document Gateway).paths
  in
  no_problems problems

let unmatched_body (w : World.t) =
  let token = service_person w "probe" in
  let problems =
    List.concat_map
      (fun (meth, path, op) ->
        if not (takes_json op) then []
        else
          List.concat_map
            (fun (label, json) ->
              let reply =
                with_json ~token ~headers:(required_headers op) (World.origin w)
                  meth (concrete path) json
              in
              let at = Printf.sprintf "%s %s with %s" meth path label in
              if reply.status <> 400 then
                [ Printf.sprintf "%s: %d" at reply.status ]
              else problems_of at reply meth path)
            [ ("an array", "[]"); ("not JSON", {|{"unterminated|}) ])
      (operations ())
  in
  no_problems problems

(* A body of [size] bytes — [[]] then spaces, JSON at any size, and never
   the operation's schema — sent with its length or in chunks without one.
   With its length, the request asks to continue first and sends the body
   only if the gateway asks for it; the answer says whether it did. *)
let sized origin path token size framing required =
  let body = "[]" ^ String.make (size - 2) ' ' in
  let uri = Uri.of_string origin in
  let host = Option.get (Uri.host uri) and port = Option.get (Uri.port uri) in
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.connect ~sw (Run.net ())
      (`Tcp (Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string host), port))
  in
  let headers =
    required
    @ [
        ("authorization", "Bearer " ^ token);
        ("content-type", "application/json");
        ("host", Printf.sprintf "%s:%d" host port);
        ("connection", "close");
      ]
    @
    match framing with
    | `Length ->
        [ ("content-length", string_of_int size); ("expect", "100-continue") ]
    | `Chunked -> [ ("transfer-encoding", "chunked") ]
  in
  Eio.Flow.copy_string
    (Printf.sprintf "POST %s HTTP/1.1\r\n%s\r\n" path
       (String.concat ""
          (List.map (fun (k, v) -> Printf.sprintf "%s: %s\r\n" k v) headers)))
    socket;
  let input = Eio.Buf_read.of_flow ~max_size:max_int socket in
  let read_response () =
    match Wire.Response_io.read input with
    | `Ok response -> response
    | `Eof -> raise End_of_file
    | `Invalid why -> failwith ("an answer that is not HTTP: " ^ why)
  in
  let reply_of response =
    let reader = Wire.Response_io.make_body_reader response input in
    let text =
      Call.body_of (fun () -> Wire.Response_io.read_body_chunk reader)
    in
    {
      Call.status = Http.Status.to_int (Http.Response.status response);
      headers = Http.Response.headers response;
      text;
      json = Call.parsed text;
    }
  in
  match framing with
  | `Length ->
      let first = read_response () in
      if Http.Status.to_int (Http.Response.status first) = 100 then (
        Eio.Flow.copy_string body socket;
        (reply_of (read_response ()), true))
      else (reply_of first, false)
  | `Chunked ->
      let chunk = 64 * 1024 in
      let write () =
        let rec from at =
          if at < size then (
            let n = min chunk (size - at) in
            Eio.Flow.copy_string
              (Printf.sprintf "%x\r\n%s\r\n" n (String.sub body at n))
              socket;
            from (at + n))
        in
        from 0;
        Eio.Flow.copy_string "0\r\n\r\n" socket
      in
      ( Eio.Fiber.first
          (fun () ->
            (try write () with End_of_file | Eio.Io _ -> ());
            Eio.Fiber.await_cancel ())
          (fun () -> reply_of (read_response ())),
        false )

let too_large (w : World.t) =
  let token = service_person w "body-limit" in
  let problems =
    List.concat_map
      (fun (meth, path, (op : Openapi.operation)) ->
        if not (takes_json op) then []
        else
          let where = Printf.sprintf "%s %s" meth path in
          match
            Option.bind op.limits (fun limits ->
                List.assoc_opt "maxBodyBytes" limits)
          with
          | Some (Openapi.Count limit) ->
              List.concat_map
                (fun (framing, word) ->
                  let over, asked =
                    sized (World.origin w) (concrete path) token (limit + 1)
                      framing (required_headers op)
                  in
                  let what =
                    Printf.sprintf "%s, %d bytes by %s" where (limit + 1) word
                  in
                  (if over.status <> 413 then
                     [ Printf.sprintf "%s: %d" what over.status ]
                   else problems_of what over meth path)
                  @ (if asked then
                       [
                         Printf.sprintf
                           "%s, %d bytes by length: the gateway asked for the \
                            body its Content-Length already ruled out"
                           where (limit + 1);
                       ]
                     else [])
                  @
                  let at, _ =
                    sized (World.origin w) (concrete path) token limit framing
                      (required_headers op)
                  in
                  if at.status <> 400 then
                    [
                      Printf.sprintf
                        "%s, %d bytes by %s: %d, where reading it finds an \
                         array"
                        where limit word at.status;
                    ]
                  else [])
                [ (`Length, "length"); (`Chunked, "chunked") ]
          | _ ->
              [
                Printf.sprintf
                  "%s takes a JSON body and the spec states no maxBodyBytes"
                  where;
              ])
      (operations ())
  in
  no_problems problems

let undeclared_path (w : World.t) =
  List.iter
    (fun path ->
      let reply = Call.call (World.origin w) "GET" path in
      status path 404 reply;
      Check.that
        (path ^ ": its content type is application/json")
        (match header "content-type" reply with
        | Some kind -> String.starts_with ~prefix:"application/json" kind
        | None -> false);
      Check.that
        (Printf.sprintf "%s: %s is an ErrorResponse" path reply.text)
        (is_error_response reply))
    [ "/no/such/path"; "/api/no-such-route" ]

let request_limits (w : World.t) =
  let token = World.person w "limits" in
  let pending_max = Protocol.subscribe_max_items "pendingReplies"
  and scoped_max = Protocol.subscribe_max_items "scoped" in
  let body pending scoped =
    Bus_stream.subscribe_body ~client_id:(Fresh.uuid ())
      ~global:[ "beckon:focus" ]
      ~pending_replies:(List.init pending (fun _ -> Fresh.uuid ()))
      ~scoped:
        (List.init scoped (fun i ->
             Bus_stream.scope
               (Printf.sprintf "limit-scope-%d" i)
               [ "mark:added" ]))
      ()
  in
  let at_limit = World.subscribe w token (body pending_max scoped_max) in
  Bus_stream.close at_limit;
  List.iter
    (fun (label, over) ->
      let reply =
        Call.call ~token ~json:over (World.origin w) "POST"
          (Protocol.path Gateway "POST" "/bus/subscribe")
      in
      status label 400 reply;
      conforms "POST" "/bus/subscribe" reply)
    [
      ("pendingReplies", body (pending_max + 1) 0);
      ("scoped", body 0 (scoped_max + 1));
    ]

let tests implementations =
  each_plane ~make:(plain implementations) ~world:Fun.id ~close:World.close
    "every operation the spec declares"
    [
      case
        "a protected operation refuses a request with no credential — before \
         reading its body — with 401, a challenge, and a hint naming the \
         header"
        no_credential;
      case
        "a protected operation refuses a credential it cannot verify: 401 with \
         error=\"invalid_token\""
        unverifiable;
      case "a public operation answers without a credential, as declared" public;
      case
        "a method the spec does not declare on a path it does is a 404 \
         ErrorResponse, whoever asks"
        undeclared_method;
      case
        "a JSON request body that does not match its schema is a 400 \
         ErrorResponse"
        unmatched_body;
      case
        "a JSON body larger than maxBodyBytes is a 413 ErrorResponse — unread \
         when its length says so — and one of exactly maxBodyBytes is read"
        too_large;
      case "a path the spec does not declare is a 404 ErrorResponse"
        undeclared_path;
      case
        "every request limit the spec states is enforced, and not one below it"
        request_limits;
    ]
