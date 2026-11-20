(* Plain HTTP from the harness, on cohttp's codecs, and the one check
   every response can be put through: that its status is declared for the
   route, and its body and headers are what the declaration says. *)

type reply = {
  status : int;
  headers : Http.Header.t;
  text : string;
  json : Yojson.Safe.t option;  (** The body, when it is JSON. *)
}

let parsed text =
  match Yojson.Safe.from_string text with
  | json -> Some json
  | exception Yojson.Json_error _ -> None

(* cohttp-eio 6.3.0's client hands back a body flow whose read copies from
   the start of a chunk, not from where the last read stopped, whenever the
   chunk is larger than the buffer it reads into (Reader_flow.single_read,
   src/utils.ml): a body that arrives in one chunk of more than 4 KiB comes
   back corrupted. So the harness's client is cohttp's codecs on a socket of
   its own, as its server is: [exchange] answers the response's head and its
   body, a chunk at a time, as cohttp's reader decodes it. *)
let exchange ~sw ?(headers = []) ?body meth url =
  let uri = Uri.of_string url in
  (* An IPv6 host, bare: getaddrinfo takes it so, and a Host header in
     brackets. *)
  let host =
    match Uri.host uri with
    | Some host ->
        if String.starts_with ~prefix:"[" host then
          String.sub host 1 (String.length host - 2)
        else host
    | None -> failwith (url ^ " names no host")
  in
  let port =
    match Uri.port uri with
    | Some port -> port
    | None -> failwith (url ^ " names no port")
  in
  let address =
    match
      Eio.Net.getaddrinfo_stream ~service:(string_of_int port) (Run.net ()) host
    with
    | address :: _ -> address
    | [] -> failwith ("no address for " ^ host)
  in
  let socket = Eio.Net.connect ~sw (Run.net ()) address in
  let headers =
    [
      ( "host",
        if String.contains host ':' then Printf.sprintf "[%s]:%d" host port
        else Printf.sprintf "%s:%d" host port );
      ("connection", "close");
    ]
    @ (match body with
      | Some text -> [ ("content-length", string_of_int (String.length text)) ]
      | None -> [])
    @ headers
  in
  let request =
    Http.Request.make
      ~meth:(Http.Method.of_string meth)
      ~headers:(Http.Header.of_list headers)
      (Uri.path_and_query uri)
  in
  Eio.Buf_write.with_flow socket (fun output ->
      Wire.Request_io.write ~flush:true
        (fun writer -> Option.iter (Wire.Request_io.write_body writer) body)
        request output);
  let input = Eio.Buf_read.of_flow ~max_size:max_int socket in
  match Wire.Response_io.read input with
  | `Ok response ->
      let reader = Wire.Response_io.make_body_reader response input in
      (response, fun () -> Wire.Response_io.read_body_chunk reader)
  | `Eof -> raise End_of_file
  | `Invalid why ->
      failwith
        (Printf.sprintf "%s %s answered what is not HTTP: %s" meth url why)

(* The whole of a body, chunk by chunk. *)
let body_of next =
  let text = Buffer.create 4096 in
  let rec chunks () =
    match next () with
    | Cohttp.Transfer.Chunk chunk ->
        Buffer.add_string text chunk;
        chunks ()
    | Final_chunk chunk -> Buffer.add_string text chunk
    | Done -> ()
  in
  chunks ();
  Buffer.contents text

let call ?token ?json ?body ?(headers = []) origin meth path =
  let headers =
    (match token with
      | Some t -> [ ("authorization", "Bearer " ^ t) ]
      | None -> [])
    @ (match json with
      | Some _ -> [ ("content-type", "application/json") ]
      | None -> [])
    @ headers
  in
  let body =
    match json with Some j -> Some (Yojson.Safe.to_string j) | None -> body
  in
  Eio.Switch.run @@ fun sw ->
  let response, next = exchange ~sw ~headers ?body meth (origin ^ path) in
  let text = body_of next in
  {
    status = Http.Status.to_int (Http.Response.status response);
    headers = Http.Response.headers response;
    text;
    json = parsed text;
  }

(* A call that may find nothing listening: None then. *)
let reach ?token origin meth path =
  match call ?token origin meth path with
  | reply -> Some reply
  | exception (Eio.Io _ | End_of_file) -> None

let shown text =
  if String.length text > 200 then String.sub text 0 200 else text

(* What a body of an error must never carry: a stack frame, a source path or
   the name of a secret. *)
let internals =
  Re.compile
    (Re.Pcre.re
       {|\bat [^\s]+ \(|\.[cm]?[jt]s:\d+|node_modules|/Users/|/home/|JWT_SECRET|CLIENT_SECRET|})

(* How the reply is not what [which]'s document declares for [meth
   template]. Empty is conformant. *)
let non_conformance which meth template reply =
  match Protocol.response which meth template reply.status with
  | None ->
      [
        Printf.sprintf
          "%s %s answered %d, which the spec does not declare (body: %s)" meth
          template reply.status (shown reply.text);
      ]
  | Some declared ->
      let content =
        match declared.content with
        | None -> []
        | Some media_types -> (
            let got =
              match Http.Header.get reply.headers "content-type" with
              | Some value ->
                  String.lowercase_ascii
                    (String.trim (List.hd (String.split_on_char ';' value)))
              | None -> ""
            in
            let media =
              match List.assoc_opt got media_types with
              | Some found -> Some (got, found)
              | None ->
                  Option.map
                    (fun found -> ("*/*", found))
                    (List.assoc_opt "*/*" media_types)
            in
            match media with
            | None ->
                [
                  Printf.sprintf "%d came as %s, not %s (body: %s)" reply.status
                    (if got = "" then "no content type" else got)
                    (String.concat " or " (List.map fst media_types))
                    (shown reply.text);
                ]
            | Some (name, media) when String.ends_with ~suffix:"json" name -> (
                match reply.json with
                | None ->
                    [
                      Printf.sprintf
                        "%d declared %s but the body is not JSON: %s"
                        reply.status name (shown reply.text);
                    ]
                | Some _
                  when reply.status >= 400 && Re.execp internals reply.text ->
                    [
                      Printf.sprintf
                        "%d body carries internals — a stack frame, a source \
                         path or a secret's name: %s"
                        reply.status (shown reply.text);
                    ]
                | Some json -> (
                    match Protocol.refusals media.schema json with
                    | [] -> []
                    | refusals ->
                        [
                          Printf.sprintf
                            "%d body does not match the declared schema: %s"
                            reply.status
                            (String.concat "; " refusals);
                        ]))
            | Some _ -> [])
      in
      let headers =
        List.concat_map
          (fun (name, (header : Xynolog_protocol_schema.Openapi.header)) ->
            match Http.Header.get reply.headers name with
            | None ->
                [ Printf.sprintf "%d carries no %s header" reply.status name ]
            | Some value -> (
                match Protocol.refusals header.schema (`String value) with
                | [] -> []
                | refusals ->
                    [
                      Printf.sprintf "%d %s: %S does not match its schema (%s)"
                        reply.status name value
                        (String.concat "; " refusals);
                    ]))
          (Option.value ~default:[] declared.headers)
      in
      content @ headers

(* A multipart/form-data body, as a browser or an SDK writes one: each part a
   text field, or a file with its name and media type. Its content type, with
   the boundary, and the body. *)
type part =
  | Field of string * string
  | File of { name : string; filename : string; media : string; bytes : string }

let multipart parts =
  let boundary = "conformance-" ^ Fresh.hex 12 in
  let body = Buffer.create 1024 in
  List.iter
    (fun part ->
      Buffer.add_string body ("--" ^ boundary ^ "\r\n");
      (match part with
      | Field (name, value) ->
          Printf.bprintf body
            "Content-Disposition: form-data; name=\"%s\"\r\n\r\n%s" name value
      | File { name; filename; media; bytes } ->
          Printf.bprintf body
            "Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"\r\n\
             Content-Type: %s\r\n\
             \r\n\
             %s"
            name filename media bytes);
      Buffer.add_string body "\r\n")
    parts;
  Buffer.add_string body ("--" ^ boundary ^ "--\r\n");
  ("multipart/form-data; boundary=" ^ boundary, Buffer.contents body)

(* The parts of a multipart/form-data body whose content type is
   [content_type], each its name, its filename when it is a file, and its
   bytes: None when it is not one. *)
let parts content_type body =
  let parameter name header =
    List.find_map
      (fun piece ->
        match String.index_opt piece '=' with
        | Some eq when String.trim (String.sub piece 0 eq) = name ->
            let value =
              String.trim
                (String.sub piece (eq + 1) (String.length piece - eq - 1))
            in
            Some
              (if String.length value >= 2 && value.[0] = '"' then
                 String.sub value 1 (String.length value - 2)
               else value)
        | _ -> None)
      (String.split_on_char ';' header)
  in
  match
    ( String.trim (List.hd (String.split_on_char ';' content_type)),
      parameter "boundary" content_type )
  with
  | "multipart/form-data", Some boundary ->
      let delimiter = Re.compile (Re.str ("\r\n--" ^ boundary)) in
      let sections = Re.split_delim delimiter ("\r\n" ^ body) in
      Some
        (List.filter_map
           (fun section ->
             match Re.exec_opt (Re.compile (Re.str "\r\n\r\n")) section with
             | None -> None
             | Some found ->
                 let head = String.sub section 0 (Re.Group.start found 0)
                 and bytes =
                   String.sub section (Re.Group.stop found 0)
                     (String.length section - Re.Group.stop found 0)
                 in
                 let header name =
                   List.find_map
                     (fun line ->
                       match String.index_opt line ':' with
                       | Some colon
                         when String.lowercase_ascii
                                (String.trim (String.sub line 0 colon))
                              = name ->
                           Some
                             (String.trim
                                (String.sub line (colon + 1)
                                   (String.length line - colon - 1)))
                       | _ -> None)
                     (String.split_on_char '\n' head)
                 in
                 Option.bind (header "content-disposition") (fun disposition ->
                     Option.map
                       (fun name ->
                         (name, parameter "filename" disposition, bytes))
                       (parameter "name" disposition)))
           (List.tl sections))
  | _ -> None
