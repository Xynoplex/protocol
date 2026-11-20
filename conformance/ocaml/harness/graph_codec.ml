(* How the weaver suite reads the graph: what a stored row is read as, an
   annotation or a resource, and what the graph is meant to hold of an
   annotation the record holds.

   The suite reads the graph with a reader of its own, held to the case
   table the Weaver's codec is held to (graph/codec-cases.json, by
   [cases_checked]), so that it judges a Weaver by something other than the
   Weaver's own code.

   A reader manufactures nothing. A row without a field it must have is
   refused by the field's name, and a field a row does not hold is left out
   of what is read. Each value a row holds as JSON, and each thing read, is
   held to the spec's schema before it is answered. *)

let member = Yojson.Safe.Util.member

(* A property of a row as text: None where the row holds none. *)
let text of_ (row : Yojson.Safe.t) key =
  match member key row with
  | `Null -> None
  | `String s -> Some s
  | other ->
      failwith
        (Printf.sprintf "%s: the stored %s is not text: %s" of_ key
           (Yojson.Safe.to_string other))

(* A row's property that holds JSON, parsed and held to the gateway's
   component [schema]. *)
let held of_ key stored schema =
  match Yojson.Safe.from_string stored with
  | exception Yojson.Json_error _ ->
      failwith
        (Printf.sprintf "%s: the stored %s is not JSON: %s" of_ key stored)
  | value -> (
      match Protocol.against schema value with
      | [] -> value
      | _ ->
          failwith
            (Printf.sprintf "%s: the stored %s is not what the codec writes: %s"
               of_ key stored))

(* [value] held to the gateway's component [schema], as what [of_] is. *)
let checked of_ schema (value : Yojson.Safe.t) =
  match Protocol.against schema value with
  | [] -> value
  | refusals ->
      failwith
        (Printf.sprintf "%s: the row is not %s the spec allows: %s" of_
           (if schema = "Annotation" then "an annotation" else "a resource")
           (String.concat "; " refusals))

(* What the graph holds of an annotation, each part as the value it is. *)
type held = {
  id : string;
  resource_id : string;
  motivation : string;
  creator : Yojson.Safe.t;
  created : string;
  selector : Yojson.Safe.t option;
  source : string option;  (** The resource the annotation links to. *)
  modified : string option;
  generator : Yojson.Safe.t option;
}

(* The annotation those parts and its entity tags are. Tags are edges, and
   a store answers edges in no promised order, so they are held in one:
   sorted, and then the link. A tag with no name is no tag. *)
let annotation_of held tags =
  let tagged =
    List.filter_map
      (fun tag ->
        if tag = "" then None
        else
          Some
            (`Assoc
               [
                 ("type", `String "TextualBody");
                 ("value", `String tag);
                 ("purpose", `String "tagging");
               ]))
      (List.sort compare tags)
  and linked =
    match held.source with
    | Some source ->
        [
          `Assoc
            [
              ("type", `String "SpecificResource");
              ("source", `String source);
              ("purpose", `String "linking");
            ];
        ]
    | None -> []
  in
  let body = tagged @ linked in
  let optional key = function Some value -> [ (key, value) ] | None -> [] in
  `Assoc
    ([
       ("@context", `String "http://www.w3.org/ns/anno.jsonld");
       ("type", `String "Annotation");
       ("id", `String held.id);
       ("motivation", `String held.motivation);
       ( "target",
         `Assoc
           (("source", `String held.resource_id)
           :: optional "selector" held.selector) );
       ("creator", held.creator);
       ("created", `String held.created);
     ]
    @ (if body = [] then [] else [ ("body", `List body) ])
    @ optional "modified" (Option.map (fun m -> `String m) held.modified)
    @ optional "generator" held.generator)

(* An annotation's row and the names its tag edges go to, read as the
   annotation. The creator, the selector and the generator are held as
   JSON. *)
let read_annotation (row : Yojson.Safe.t) tags =
  let id =
    match text "Annotation" row "id" with
    | Some id when id <> "" -> id
    | _ -> failwith "Annotation missing required field: id"
  in
  let of_ = "Annotation " ^ id in
  let required key =
    match text of_ row key with
    | Some value when value <> "" -> value
    | _ -> failwith (Printf.sprintf "%s missing required field: %s" of_ key)
  in
  let motivation = required "motivation" in
  if Protocol.against "Motivation" (`String motivation) <> [] then
    failwith
      (Printf.sprintf "%s: the stored motivation is not one the spec names: %s"
         of_ motivation);
  let json key schema =
    Option.map (fun stored -> held of_ key stored schema) (text of_ row key)
  in
  checked of_ "Annotation"
    (annotation_of
       {
         id;
         resource_id = required "resourceId";
         motivation;
         creator = held of_ "creator" (required "creator") "Agent";
         created = required "created";
         selector = json "selector" "AnnotationSelector";
         source = text of_ row "source";
         modified = text of_ row "modified";
         generator = json "generator" "OneOrMoreAgents";
       }
       tags)

(* An agent as the graph holds it: who, and not what a person is called. A
   person's name is the directory's, joined onto a read and changed there,
   so the graph holds none. Software is held as the record gives it. *)
let who = function
  | `Assoc members when List.assoc_opt "@type" members = Some (`String "Person")
    ->
      `Assoc (List.remove_assoc "name" members)
  | agent -> agent

(* The items of an annotation's body: none, one, or a list. *)
let body_items annotation =
  match member "body" annotation with
  | `Null -> []
  | `List items -> items
  | item -> [ item ]

let string_at json key =
  match member key json with `String s -> Some s | _ -> None

(* What the graph is meant to hold of an annotation the record holds. The
   graph keeps what its queries need and no more, so a case holds the graph
   to this and never to the record's annotation itself. *)
let intended (annotation : Yojson.Safe.t) =
  let id = Option.value (string_at annotation "id") ~default:"" in
  let creator =
    match member "creator" annotation with
    | `Null -> failwith ("Annotation " ^ id ^ " missing required field: creator")
    | creator -> creator
  in
  let target = member "target" annotation in
  let resource_id, selector =
    match target with
    | `String source -> (source, None)
    | target ->
        ( Option.get (string_at target "source"),
          match member "selector" target with
          | `Null -> None
          | selector -> Some selector )
  in
  let items = body_items annotation in
  let source =
    List.find_map
      (fun item ->
        if string_at item "type" = Some "SpecificResource" then
          string_at item "source"
        else None)
      items
  and tags =
    List.filter_map
      (fun item ->
        match
          ( string_at item "type",
            string_at item "purpose",
            string_at item "value" )
        with
        | Some "TextualBody", Some "tagging", Some value when value <> "" ->
            Some value
        | _ -> None)
      items
  in
  annotation_of
    {
      id;
      resource_id;
      motivation = Option.get (string_at annotation "motivation");
      creator = who creator;
      created = Option.get (string_at annotation "created");
      selector;
      source;
      modified = string_at annotation "modified";
      generator =
        (match member "generator" annotation with
        | `Null -> None
        | `List agents -> Some (`List (List.map who agents))
        | agent -> Some (who agent));
    }
    tags

(* A resource's row, read as the resource, with its one representation
   rebuilt. Who it is by is held as JSON. [false] and an empty list are
   values a row holds; only a field that is not there, or is the empty
   text, is missing. *)
let read_resource (row : Yojson.Safe.t) =
  let id =
    match member "id" row with
    | `String id when id <> "" -> id
    | _ -> failwith "Resource missing required field: id"
  in
  let of_ = "Resource " ^ id in
  let missing key =
    failwith (Printf.sprintf "%s missing required field: %s" of_ key)
  in
  let required key =
    match member key row with
    | `String value when value <> "" -> value
    | _ -> missing key
  in
  let name = required "name" in
  let entity_types =
    match member "entityTypes" row with
    | `List items
      when List.for_all (function `String _ -> true | _ -> false) items ->
        `List items
    | _ -> missing "entityTypes"
  in
  let media_type = required "format" in
  let archived =
    match member "archived" row with
    | `Bool archived -> archived
    | _ -> missing "archived"
  in
  let date_created = required "created" in
  let creator = required "creator" in
  let checksum = required "contentChecksum" in
  let storage_uri =
    match member "storageUri" row with
    | `String uri when uri <> "" -> [ ("storageUri", `String uri) ]
    | _ -> []
  in
  checked of_ "ResourceDescriptor"
    (`Assoc
       [
         ("@context", `String "https://schema.org/");
         ("@id", `String id);
         ("name", `String name);
         ("entityTypes", entity_types);
         ( "representations",
           `List
             [
               `Assoc
                 ([
                    ("mediaType", `String media_type);
                    ("checksum", `String checksum);
                    ("rel", `String "original");
                  ]
                 @ storage_uri);
             ] );
         ("archived", `Bool archived);
         ("dateCreated", `String date_created);
         ("wasAttributedTo", held of_ "creator" creator "OneOrMoreAgents");
       ])

(* What the graph is meant to hold of a resource the record holds: its one
   representation, with no byte size; a resource with no archived flag is
   not archived, and one with no entity types has none. A resource the
   graph could not read back is refused, by the field it lacks. *)
let intended_resource (resource : Yojson.Safe.t) =
  let id = Option.value (string_at resource "@id") ~default:"" in
  let missing key =
    failwith (Printf.sprintf "Resource %s missing required field: %s" id key)
  in
  let required key =
    match member key resource with
    | `Null | `String "" -> missing key
    | value -> value
  in
  let representation =
    match member "representations" resource with
    | `List (first :: _) -> first
    | `Assoc _ as one -> one
    | _ -> missing "representations"
  in
  let held key =
    match member key representation with
    | `Null | `String "" -> missing key
    | value -> (key, value)
  in
  `Assoc
    [
      ("@context", required "@context");
      ("@id", required "@id");
      ("name", required "name");
      ( "entityTypes",
        match member "entityTypes" resource with `Null -> `List [] | t -> t );
      ( "representations",
        `List
          [
            `Assoc
              ([ held "mediaType"; held "checksum"; held "rel" ]
              @
              match member "storageUri" representation with
              | `Null -> []
              | uri -> [ ("storageUri", uri) ]);
          ] );
      ( "archived",
        match member "archived" resource with `Null -> `Bool false | a -> a );
      ("dateCreated", required "dateCreated");
      ("wasAttributedTo", required "wasAttributedTo");
    ]

(* The reader held to graph/codec-cases.json: the parts a reader has (a row
   read as an annotation, a row read as a resource, and what the graph is
   meant to hold of an annotation the record holds), each case as the table
   writes it. The parts that are a writer's are run by the writer's codec.
   Every case is of one part, and each part read here has cases. *)
let cases_checked =
  lazy
    (let table = "graph/codec-cases.json" in
     let cases =
       match
         Option.bind (Protocol.json_of table) (fun t ->
             Protocol.at t [ "cases" ])
       with
       | Some (`List cases) -> cases
       | _ -> failwith (table ^ " holds no cases")
     in
     let wrong = ref [] in
     let say why fmt =
       Printf.ksprintf (fun said -> wrong := (why ^ ": " ^ said) :: !wrong) fmt
     in
     let read why given read expected =
       let outcome =
         match read () with
         | json -> Ok json
         | exception Failure said -> Error said
       in
       match (member "refused" given, outcome) with
       | `String word, Error said ->
           if not (Re.execp (Re.compile (Re.str word)) said) then
             say why "refused with %S, which does not name %s" said word
       | `String word, Ok json ->
           say why "read as %s, where it is refused naming %s"
             (Yojson.Safe.to_string json)
             word
       | _, Error said -> say why "refused: %s" said
       | _, Ok json ->
           if Check.canonical json <> Check.canonical (member expected given)
           then
             say why "read as %s, where the table says %s"
               (Yojson.Safe.to_string json)
               (Yojson.Safe.to_string (member expected given))
     in
     let parts =
       [
         "annotationStored"; "annotationRead"; "resourceStored"; "resourceRead";
       ]
     in
     let counted = Hashtbl.create 3 in
     List.iter
       (fun case ->
         let why = Yojson.Safe.Util.to_string (member "why" case) in
         match case with
         | `Assoc [ ("why", _); (part, given) ]
         | `Assoc [ (part, given); ("why", _) ] -> (
             Hashtbl.replace counted part ();
             match part with
             | "annotationStored" ->
                 read why given
                   (fun () -> intended (member "annotation" given))
                   "holds"
             | "annotationRead" ->
                 read why given
                   (fun () ->
                     read_annotation
                       (member "properties" given)
                       (List.map Yojson.Safe.Util.to_string
                          (Yojson.Safe.Util.to_list (member "tags" given))))
                   "annotation"
             | "resourceStored" ->
                 read why given
                   (fun () -> intended_resource (member "resource" given))
                   "holds"
             | "resourceRead" ->
                 read why given
                   (fun () -> read_resource (member "properties" given))
                   "resource"
             | _ -> ())
         | _ -> say why "is not of one part")
       cases;
     List.iter
       (fun part ->
         if not (Hashtbl.mem counted part) then say part "has no case")
       parts;
     match List.rev !wrong with
     | [] -> ()
     | wrong ->
         failwith
           (table ^ ": the harness's reading of the graph disagrees:\n"
          ^ String.concat "\n" wrong))
