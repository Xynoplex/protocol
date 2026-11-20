(* The graph, as the weaver suite reads it: its own Cypher, on a connection
   of its own, each statement built from graph/schema.json (the labels, the
   keys and lookups, the relationships and the stub), and its own reading of
   each row (Graph_codec). Each statement is read over Bolt and over the
   Query API, which must agree. It writes nothing but what a case does
   behind a Weaver's back: emptying the graph, and losing an annotation. *)

module Schema = Xynolog_protocol.Tables.Graph_schema

let schema () = (Protocol.tables ()).graph_schema
let member = Yojson.Safe.Util.member

let node kind : Schema.resource =
  let nodes = (schema ()).nodes in
  match kind with
  | "resource" -> nodes.resource
  | "annotation" -> nodes.annotation
  | "entityType" -> nodes.entity_type
  | other -> failwith ("graph/schema.json names no kind of node " ^ other)

(* A property a reader finds a node of [kind] by, which the schema must say
   it is. *)
let lookup kind property =
  if not (List.mem property (node kind).lookups) then
    failwith
      (Printf.sprintf "graph/schema.json finds no %s by %s" kind property);
  property

(* A node pattern: its variable, its kind's label, and the property it is
   matched by, bound to a parameter of the same name. *)
let pattern ?(var = "") ?by kind =
  Printf.sprintf "(%s:%s%s)" var (node kind).label
    (match by with
    | Some (property, parameter) ->
        Printf.sprintf " {%s: $%s}" property parameter
    | None -> "")

(* A relationship from one pattern to another, each of the kind the schema
   says the relationship goes from and to. *)
let related (r : Schema.belongs_to) (from_kind, from) (to_kind, to_) =
  if r.from <> from_kind || r.to_ <> to_kind then
    failwith
      (Printf.sprintf
         "graph/schema.json has %s go from %s to %s, and the reader asks it \
          from %s to %s"
         r.type_ r.from r.to_ from_kind to_kind);
  Printf.sprintf "%s-[:%s]->%s" from r.type_ to_

let relationships () = (schema ()).relationships

(* A node the graph holds: no stub. *)
let held var =
  let stub = (schema ()).stub in
  Printf.sprintf "coalesce(%s.%s, false) = false" var stub.marker

type t = Neo4j.t

let connect ~host ~password ~database : t =
  Neo4j.connect ~host ~password ~database

let asked t statement parameters =
  Neo4j.agreed t
    ~parameters:(List.map (fun (k, v) -> (k, `String v)) parameters)
    statement

let row column = function
  | `Assoc _ as row -> row
  | other ->
      failwith
        (Printf.sprintf
           "the graph answered %s as something other than a node's properties: \
            %s"
           column
           (Yojson.Safe.to_string other))

let strings column = function
  | `List items ->
      List.map
        (function
          | `String s -> s
          | other ->
              failwith
                (Printf.sprintf
                   "the graph answered %s with something other than a string: \
                    %s"
                   column
                   (Yojson.Safe.to_string other)))
        items
  | other ->
      failwith
        (Printf.sprintf
           "the graph answered %s as something other than a list: %s" column
           (Yojson.Safe.to_string other))

(* An annotation, however it was matched as [a], with the names its tag
   edges go to. *)
let tagged () =
  Printf.sprintf
    "OPTIONAL MATCH %s WITH a, collect(tag.%s) AS tags RETURN properties(a) AS \
     row, tags"
    (related (relationships ()).tagged_as ("annotation", "(a)")
       ("entityType", pattern ~var:"tag" "entityType"))
    (node "entityType").key

let annotations t matched parameters =
  List.map
    (function
      | [ r; tags ] ->
          Graph_codec.read_annotation (row "row" r) (strings "tags" tags)
      | _ -> failwith "the graph answered an annotation with other columns")
    (asked t (matched ^ " " ^ tagged ()) parameters)

let resources t statement parameters =
  List.map
    (function
      | [ r ] -> Graph_codec.read_resource (row "row" r)
      | _ -> failwith "the graph answered a resource with other columns")
    (asked t statement parameters)

let key kind = (node kind).key

(* A resource the graph holds, or None. *)
let resource t resource_id =
  match
    resources t
      (Printf.sprintf "MATCH %s WHERE %s RETURN properties(d) AS row"
         (pattern ~var:"d" ~by:(key "resource", "id") "resource")
         (held "d"))
      [ ("id", resource_id) ]
  with
  | [] -> None
  | [ one ] -> Some one
  | _ -> failwith ("the graph holds more than one resource " ^ resource_id)

let id_of resource = Yojson.Safe.Util.to_string (member "@id" resource)

(* The ids of the resources the graph holds under a name. *)
let resources_named t name =
  List.map id_of
    (resources t
       (Printf.sprintf "MATCH %s WHERE %s RETURN properties(d) AS row"
          (pattern ~var:"d" ~by:(lookup "resource" "name", "name") "resource")
          (held "d"))
       [ ("name", name) ])

(* An annotation the graph holds, or None. *)
let annotation t annotation_id =
  match
    annotations t
      ("MATCH " ^ pattern ~var:"a" ~by:(key "annotation", "id") "annotation")
      [ ("id", annotation_id) ]
  with
  | [] -> None
  | [ one ] -> Some one
  | _ -> failwith ("the graph holds more than one annotation " ^ annotation_id)

(* The annotations on a resource, in no promised order. *)
let annotations_on t resource_id =
  annotations t
    ("MATCH "
    ^ pattern ~var:"a" ~by:(lookup "annotation" "resourceId", "id") "annotation"
    )
    [ ("id", resource_id) ]

(* The annotations, on any resource, that link to a resource. *)
let references_to t resource_id =
  annotations t
    ("MATCH "
    ^ related (relationships ()).references
        ("annotation", pattern ~var:"a" "annotation")
        ("resource", pattern ~by:(key "resource", "id") "resource"))
    [ ("id", resource_id) ]

(* The resources a resource's annotations link to, each with the ids of the
   annotations that link to it. *)
let links_from t resource_id =
  let r = relationships () in
  List.map
    (function
      | [ d; by ] -> (Graph_codec.read_resource (row "row" d), strings "by" by)
      | _ -> failwith "the graph answered a link with other columns")
    (asked t
       (Printf.sprintf
          "MATCH %s, %s WHERE %s WITH d, collect(a.%s) AS by RETURN \
           properties(d) AS row, by"
          (related r.belongs_to
             ("annotation", pattern ~var:"a" "annotation")
             ("resource", pattern ~by:(key "resource", "id") "resource"))
          (related r.references ("annotation", "(a)")
             ("resource", pattern ~var:"d" "resource"))
          (held "d") (key "annotation"))
       [ ("id", resource_id) ])

(* How many resources the graph holds have each entity type. *)
let entity_type_counts t =
  List.map
    (function
      | [ `String type_; `Intlit count ] -> (type_, int_of_string count)
      | _ -> failwith "the graph answered a count with other columns")
    (asked t
       (Printf.sprintf
          "MATCH %s WHERE %s UNWIND d.%s AS type RETURN type, count(*) AS count"
          (pattern ~var:"d" "resource")
          (held "d")
          (lookup "resource" "entityTypes"))
       [])

(* ── What a case does behind a Weaver's back ─────────────────────────── *)

(* Empty the graph: every node, and with it every relationship. *)
let clean t = Neo4j.clear t

(* Take one annotation out of the graph, relationships and all, as one lost
   behind a Weaver's back is. *)
let lose t annotation_id =
  Neo4j.run t
    ~parameters:[ ("id", `String annotation_id) ]
    (Printf.sprintf "MATCH %s DETACH DELETE a"
       (pattern ~var:"a" ~by:(key "annotation", "id") "annotation"))

let close = Neo4j.close
