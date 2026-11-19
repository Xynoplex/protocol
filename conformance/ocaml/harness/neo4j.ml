(* A Neo4j, read by the harness two ways: over Bolt, on neodriver_eio, and
   over HTTP, on Neo4j's Query API (POST /db/<database>/query/v2), each
   with the same basic auth. A record is a list of values in field order,
   each as the Query API's plain JSON writes it: a node as its elementId,
   labels and properties, a relationship as its elementId, type, endpoints
   and properties, and every scalar as JSON does. Both reads of one
   statement must agree; [agreed] fails naming where they do not. *)

module Driver = Neodriver_eio.Driver
module Values = Neodriver_core.Values

type t = {
  host : string;
  user : string;
  password : string;
  database : string;
  driver : Driver.t;
}

let bolt_port = 7687
let http_port = 7474

(* The database that holds a knowledge base's graph: `kb-` and the first 60
   lower-case hex digits of the SHA-256 of its DID, as
   graph/database-cases.json says, and held to each of its cases on first
   use. *)
let database_arithmetic kb =
  "kb-" ^ String.sub Digestif.SHA256.(to_hex (digest_string kb)) 0 60

let databases_checked =
  lazy
    (List.iter
       (fun (c : Xynolog_protocol.Tables.Graph_database_cases.cases_item) ->
         if database_arithmetic c.kb <> c.database then
           failwith
             (Printf.sprintf
                "graph/database-cases.json: \"%s\": the arithmetic gives %s, \
                 the case says %s"
                c.why (database_arithmetic c.kb) c.database))
       (Protocol.tables ()).graph_database_cases.cases)

let database_of kb =
  Lazy.force databases_checked;
  database_arithmetic kb

let failed what error =
  failwith
    (Printf.sprintf "neo4j: %s: %s" what
       (Neodriver_core.Errors.to_string error))

let connect ~host ~password ~database =
  let user = "neo4j" in
  let uri = Printf.sprintf "bolt://%s:%d" host bolt_port in
  match
    Driver.connect ~uri
      ~auth:
        (Neodriver_eio.Conn.basic_auth ~principal:user ~credentials:password ())
      (Run.net ())
      (Eio.Stdenv.mono_clock (Run.env ()))
      (Run.sw ())
  with
  | Ok driver -> { host; user; password; database; driver }
  | Error error -> failed ("connecting to " ^ uri) error

let close t = Driver.close t.driver

let rec json_of_value : Values.t -> Yojson.Safe.t = function
  | Null -> `Null
  | Bool b -> `Bool b
  | Int n -> `Intlit (Int64.to_string n)
  | Float f -> `Float f
  | String s -> `String s
  | List values -> `List (List.map json_of_value values)
  | Map members -> properties members
  | Node node ->
      `Assoc
        [
          ("elementId", `String node.element_id);
          ("labels", `List (List.map (fun l -> `String l) node.labels));
          ("properties", properties node.properties);
        ]
  | Relationship r ->
      `Assoc
        [
          ("elementId", `String r.element_id);
          ("startNodeElementId", `String r.start);
          ("endNodeElementId", `String r.end_);
          ("type", `String r.rel_type);
          ("properties", properties r.properties);
        ]
  | other ->
      failwith
        ("neo4j: a value the harness does not compare: "
       ^ Values.to_string other)

and properties members =
  `Assoc (List.map (fun (key, value) -> (key, json_of_value value)) members)

(* One JSON form for both reads: an object's members in key order, and
   every integer as its digits, whichever way the JSON reader kept it. *)
let rec normal : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Int n -> `Intlit (string_of_int n)
  | `Assoc members ->
      `Assoc
        (List.sort
           (fun (a, _) (b, _) -> compare a b)
           (List.map (fun (k, v) -> (k, normal v)) members))
  | `List values -> `List (List.map normal values)
  | other -> other

let parameter_value : Yojson.Safe.t -> Values.t =
  let rec value = function
    | `Null -> Values.Null
    | `Bool b -> Bool b
    | `Int n -> Int (Int64.of_int n)
    | `Intlit digits -> Int (Int64.of_string digits)
    | `Float f -> Float f
    | `String s -> String s
    | `List items -> List (List.map value items)
    | `Assoc members -> Map (List.map (fun (k, v) -> (k, value v)) members)
  in
  value

(* The records of [statement] over Bolt. *)
let over_bolt t statement parameters =
  match
    Driver.execute_query t.driver ~query:statement
      ~parameters:(List.map (fun (k, v) -> (k, parameter_value v)) parameters)
  with
  | Ok eager ->
      List.map
        (fun record -> List.map (fun v -> normal (json_of_value v)) record)
        eager.records
  | Error error -> failed statement error

(* The records of [statement] over the Query API. *)
let over_http t statement parameters =
  let reply =
    Call.call
      ~headers:
        [
          ( "authorization",
            "Basic " ^ Base64.encode_string (t.user ^ ":" ^ t.password) );
          ("accept", "application/json");
        ]
      ~json:
        (`Assoc
           [
             ("statement", `String statement); ("parameters", `Assoc parameters);
           ])
      (Printf.sprintf "http://%s:%d" t.host http_port)
      "POST"
      (Printf.sprintf "/db/%s/query/v2" t.database)
  in
  match
    ( reply.status,
      Option.bind reply.json (fun json -> Protocol.at json [ "data"; "values" ])
    )
  with
  | (200 | 202), Some (`List records) ->
      List.map
        (function
          | `List values -> List.map normal values
          | _ -> failwith "neo4j: a record that is not a list")
        records
  | status, _ ->
      failwith
        (Printf.sprintf "neo4j: the Query API answered %s with %d: %s" statement
           status (Call.shown reply.text))

(* Run [statement] over Bolt alone: what it changes. *)
let run t ?(parameters = []) statement =
  ignore (over_bolt t statement parameters : Yojson.Safe.t list list)

(* The records of [statement], read both ways, which must agree. *)
let agreed t ?(parameters = []) statement =
  let bolt = over_bolt t statement parameters
  and http = over_http t statement parameters in
  let shown records =
    Yojson.Safe.to_string (`List (List.map (fun r -> `List r) records))
  in
  if bolt <> http then
    failwith
      (Printf.sprintf
         "neo4j: %s read over Bolt and over the Query API disagree:\n\
          Bolt: %s\n\
          HTTP: %s"
         statement
         (Call.shown (shown bolt))
         (Call.shown (shown http)));
  bolt

(* Empty the database, as a wiped volume leaves it. *)
let clear t = run t "MATCH (n) DETACH DELETE n"
