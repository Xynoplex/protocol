(* The vector index, read and changed by the suite's own calls: Qdrant, over
   its REST interface, on cohttp-eio. The index's writer is what the suite
   judges, so nothing here is a Smelter's code.

   What the index is, specs/src/vectors/payload-cases.json states: for each
   knowledge base, collections named by the SHA-256 of its DID, a hyphen and
   the collection's word, with what a point's payload holds. The words and
   every payload member a point is found by are read from that table; the
   tokens each service is handed are the claims specs/src/vectors/token-cases.json
   states for the knowledge base, signed HS256 with the Qdrant's API key.

   Partly ported: the reads and the clearing the ported cases use. *)

let port = 6333
let token_variable = "QDRANT_TOKEN"

type database = { host : string; key : string }

let member = Yojson.Safe.Util.member
let payload_table = "vectors/payload-cases.json"

let raw file =
  match Protocol.json_of file with
  | Some json -> json
  | None -> failwith ("specs/src has no " ^ file)

(* The words of every collection the table states a knowledge base has. *)
let words () = Yojson.Safe.Util.keys (member "collections" (raw payload_table))

let collection_names did =
  let hashed = Digestif.SHA256.(to_hex (digest_string did)) in
  List.map (fun word -> (word, hashed ^ "-" ^ word)) (words ())

(* The payloads the table states for the points of a collection. *)
let stated_payloads word =
  List.concat_map
    (fun case ->
      match word with
      | "resources" -> (
          match member "resourceStored" case with
          | `Null -> []
          | stored ->
              List.map (member "payload")
                (Yojson.Safe.Util.to_list (member "points" stored)))
      | "annotations" -> (
          match member "annotationStored" case with
          | `Null -> []
          | c -> [ member "payload" (member "point" c) ])
      | "declined" -> (
          match member "declineStored" case with
          | `Null -> []
          | c -> [ member "payload" (member "point" c) ])
      | other -> failwith ("the harness reads no collection " ^ other))
    (Yojson.Safe.Util.to_list (member "cases" (raw payload_table)))

(* A payload member the table states for every point of a collection. *)
let payload_member word name =
  match stated_payloads word with
  | [] ->
      failwith
        (Printf.sprintf "specs/src/%s states no point of the %s" payload_table
           word)
  | payloads ->
      if List.for_all (fun p -> member name p <> `Null) payloads then name
      else
        failwith
          (Printf.sprintf
             "specs/src/%s does not state %s in every point of the %s"
             payload_table name word)

(* The claims token-cases.json states for the knowledge base [did], by the
   service: each case's collection names held to the naming arithmetic. *)
let claims did service =
  let cases =
    Yojson.Safe.Util.to_list (member "cases" (raw "vectors/token-cases.json"))
  in
  match List.find_opt (fun c -> member "kb" c = `String did) cases with
  | None -> failwith ("vectors/token-cases.json states no claims for " ^ did)
  | Some case ->
      let named = List.map snd (collection_names did) in
      List.iter
        (fun access ->
          match member "collection" access with
          | `String name when not (List.mem name named) ->
              failwith
                ("vectors/token-cases.json names " ^ name
               ^ ", which the naming arithmetic does not give " ^ did)
          | _ -> ())
        (match member "access" (member "librarian" (member "claims" case)) with
        | `List l -> l
        | _ -> []);
      member service (member "claims" case)

let base64url text =
  Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet text

(* A token for the Qdrant whose API key is [key]: a JWT of [claims] and no
   others, signed HS256 with the key's bytes. *)
let minted key claims =
  let signed =
    base64url
      (Yojson.Safe.to_string
         (`Assoc [ ("alg", `String "HS256"); ("typ", `String "JWT") ]))
    ^ "."
    ^ base64url (Yojson.Safe.to_string claims)
  in
  signed ^ "."
  ^ base64url Digestif.SHA256.(to_raw_string (hmac_string ~key signed))

let origin database = Printf.sprintf "http://%s:%d" database.host port

let ask database meth path body =
  Call.call ?json:body
    ~headers:[ ("api-key", database.key) ]
    (origin database) meth path

let answered what (reply : Call.reply) =
  if reply.status <> 200 then
    failwith
      (Printf.sprintf "Qdrant answered %d when asked to %s: %s" reply.status
         what (Call.shown reply.text));
  member "result" (Option.get reply.json)

let collections_at database =
  List.map
    (fun c -> Yojson.Safe.Util.(member "name" c |> to_string))
    (Yojson.Safe.Util.to_list
       (member "collections"
          (answered "list its collections"
             (ask database "GET" "/collections" None))))

type index = { database : database; names : (string * string) list }

let index database did = { database; names = collection_names did }

let named index word =
  match List.assoc_opt word index.names with
  | Some name -> name
  | None ->
      failwith
        (Printf.sprintf "specs/src/%s states no collection of %s" payload_table
           word)

(* Every point of a collection that a filter takes, a page at a time; none
   of a collection that is not there. *)
let scroll index word filter =
  let name = named index word in
  let rec page offset acc =
    let reply =
      ask index.database "POST"
        ("/collections/" ^ name ^ "/points/scroll")
        (Some
           (`Assoc
              ([
                 ("limit", `Int 256);
                 ("with_payload", `Bool true);
                 ("with_vector", `Bool false);
               ]
              @ (match filter with Some f -> [ ("filter", f) ] | None -> [])
              @ match offset with Some o -> [ ("offset", o) ] | None -> [])))
    in
    if reply.status = 404 then []
    else
      let result = answered ("list the points of " ^ name) reply in
      let points = acc @ Yojson.Safe.Util.to_list (member "points" result) in
      match member "next_page_offset" result with
      | `Null -> points
      | next -> page (Some next) points
  in
  page None []

let where key value =
  `Assoc
    [
      ( "must",
        `List
          [
            `Assoc
              [
                ("key", `String key);
                ("match", `Assoc [ ("value", `String value) ]);
              ];
          ] );
    ]

let payload point = member "payload" point
let points index word = scroll index word None

(* The points of one resource: of the resources, its chunks in order; of
   the annotations, the annotations on it. *)
let points_of index word resource_id =
  let found =
    scroll index word
      (Some (where (payload_member word "resourceId") resource_id))
  in
  if word = "resources" then
    let chunk = payload_member "resources" "chunkIndex" in
    List.sort
      (fun a b -> compare (member chunk (payload a)) (member chunk (payload b)))
      found
  else found

(* The point of one annotation: None while the index holds none. *)
let annotation_point index annotation_id =
  match
    scroll index "annotations"
      (Some (where (payload_member "annotations" "annotationId") annotation_id))
  with
  | [] -> None
  | [ point ] -> Some point
  | points ->
      failwith
        (Printf.sprintf "the index holds %d points of the annotation %s"
           (List.length points) annotation_id)

(* Clear the database: every collection of every knowledge base gone, as a
   wiped volume leaves it. *)
let clear index =
  List.iter
    (fun name ->
      ignore
        (answered
           ("delete the collection " ^ name)
           (ask index.database "DELETE" ("/collections/" ^ name) None)))
    (collections_at index.database)
