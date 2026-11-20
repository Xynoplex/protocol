(* The index a Smelter keeps as the record changes (SMELTER.md
   § What each event does, § Indexing a resource, § Indexing an annotation,
   § What it decides): what a text becomes in it when it is recorded,
   updated, archived and unarchived, and an annotation when it is added, has
   its body changed and is removed; what each point holds, as
   specs/src/vectors/payload-cases.json states it; what the Smelter says it
   decided of a resource, and of which bytes; and what it leaves as it was
   when it cannot read a resource's bytes or have its text embedded. With
   it, that an event it could not apply does not keep it from the next
   (§ Order). *)

open Xynolog_conformance
open Support

(* A text comfortably longer than two chunks of the default size, 512. A
   chunk's size counts tokens, and a token is four code points
   (specs/src/text/chunk-cases.json): two chunks are 4,096 code points, and
   this is more than 5,600 whatever it is about. No two of its sentences are
   the same, so no two of its chunks are. *)
let long_text about =
  String.concat " "
    (List.init 120 (fun n ->
         Printf.sprintf "Sentence %d of %s is one of a hundred and twenty."
           (n + 1) about))

let resources (w : W.t) ?vectors id =
  Qdrant.points_of ?vectors w.index "resources" id

let annotations (w : W.t) ?vectors id =
  Qdrant.points_of ?vectors w.index "annotations" id

let none what points = Check.equal what (`List []) (`List points)

(* Record one event a Smelter follows, and wait until its health counts one
   more event applied than it did before: what [record] answers. A case
   records through this, or waits as long itself, so that the count read
   before an event is of every event recorded earlier, and the wait is for
   this one. *)
let once_applied (w : W.t) record =
  let before = W.events_processed (W.health w) in
  let answer = record () in
  ignore (W.applied w (before + 1) : Yojson.Safe.t);
  answer

(* Record a text, and wait until the Smelter says it is indexed, under the
   checksum of those bytes: the resource's id. *)
let indexed (w : W.t) text =
  let resource = once_applied w (fun () -> W.created w text plain) in
  Check.equal "the decision"
    (decision resource (W.sha256 text) "indexed")
    (W.settled w resource);
  resource

let only_point what (w : W.t) resource text entity_types =
  let points = resources w ~vectors:true resource in
  Check.equal what
    (`List [ chunk_point resource 0 text (W.sha256 text) entity_types ])
    (stated_all points);
  points

let asked_since (w : W.t) mark = Ollama_embed.since w.provider mark

let recorded (w : W.t) =
  (* A text within one chunk is that chunk, whole. Two of its characters
     are outside the basic plane, and are carried as themselves. *)
  let text =
    "A treble clef \xf0\x9d\x84\x9e opens the staff, and a note \
     \xf0\x9f\x8e\xb5 follows it.\n\
     The staff has five lines.\n"
  in
  let before = W.events_processed (W.health w) in
  let resource = W.created w text plain in
  Check.equal "the decision"
    (decision resource (W.sha256 text) "indexed")
    (W.settled w resource);
  (* The points are read as soon as the decision is heard, with no wait: it
     is signalled once they are written. *)
  let points = only_point "the points" w resource text [] in
  Check.that "the point's vector is its text's" (embeds (List.hd points) text);
  ignore (W.applied w (before + 1) : Yojson.Safe.t)

let cut (w : W.t) =
  let text = long_text "a text that is cut" in
  let asked = Ollama_embed.mark w.provider in
  let resource = indexed w text in
  let points = resources w ~vectors:true resource in
  let chunks = List.map payload_text points in
  (* No chunk is longer than its size, so a text longer than two has more
     than two. Where each was cut is the chunk table's to say. *)
  Check.that "the text is more than two points" (List.length points > 2);
  Check.equal "the points"
    (`List
       (List.mapi
          (fun index chunk ->
            chunk_point resource index chunk (W.sha256 text) [])
          chunks))
    (stated_all points);
  List.iter
    (fun chunk ->
      Check.that ("the text holds the chunk " ^ chunk) (mentions chunk text))
    chunks;
  (* The provider was sent those very texts, in their order, and each one's
     vector is on its own point. *)
  Check.equal "the texts the provider was sent" (strs chunks)
    (strs
       (List.concat_map
          (fun (r : Ollama_embed.recorded) -> r.texts)
          (asked_since w asked)));
  Check.equal "the points whose vector is not their text's" (`List [])
    (stated_all
       (List.filter
          (fun point -> not (embeds point (payload_text point)))
          points))

let updated (w : W.t) =
  let resource = indexed w (long_text "a text that grows shorter") in
  Check.that "the text is more than two points"
    (List.length (resources w resource) > 2);
  let shorter = "What is left of it is one chunk.\n" in
  ignore (once_applied w (fun () -> W.updated w resource shorter) : string);
  Check.equal "the second decision"
    (decision resource (W.sha256 shorter) "indexed")
    (W.settled w ~nth:2 resource);
  let points = only_point "the points" w resource shorter [] in
  Check.that "the point's vector is its text's"
    (embeds (List.hd points) shorter)

let archived (w : W.t) =
  let text = "The Sauk crossed the Mississippi in April.\n" in
  let resource = indexed w text in
  let annotation =
    once_applied w (fun () -> W.annotated w resource "the Mississippi")
  in
  let held = annotations w ~vectors:true resource in
  Check.equal "the annotations' points" (strs [ annotation ])
    (annotation_ids held);
  once_applied w (fun () -> W.archived w resource);
  none "the resource's points once archived" (resources w resource);
  none "the annotations' points once archived" (annotations w resource);
  (* Unarchived, the resource is indexed as any is, and says so; then each
     annotation the record holds on it. *)
  once_applied w (fun () -> W.unarchived w resource);
  Check.equal "the second decision"
    (decision resource (W.sha256 text) "indexed")
    (W.settled w ~nth:2 resource);
  let points = only_point "the points once unarchived" w resource text [] in
  Check.that "the point's vector is its text's" (embeds (List.hd points) text);
  let back = annotations w ~vectors:true resource in
  Check.equal "the annotations' points once unarchived" (stated_all held)
    (stated_all back);
  Check.that "the annotation's vector is its quotation's"
    (embeds (List.hd back) "the Mississippi")

let annotated_while_archived (w : W.t) =
  let text = "Keokuk stayed west of the river that spring.\n" in
  let resource = indexed w text in
  once_applied w (fun () -> W.archived w resource);
  (* The record takes the annotation, and the Smelter applies its event: a
     start would take such a point out, so none is made. *)
  let annotation = once_applied w (fun () -> W.annotated w resource "Keokuk") in
  none "the annotations' points" (annotations w resource);
  once_applied w (fun () -> W.unarchived w resource);
  Check.equal "the second decision"
    (decision resource (W.sha256 text) "indexed")
    (W.settled w ~nth:2 resource);
  let back = annotations w ~vectors:true resource in
  Check.equal "the annotations' points once unarchived" (strs [ annotation ])
    (annotation_ids back);
  Check.that "the annotation's vector is its quotation's"
    (embeds (List.hd back) "Keokuk")

let point_of (w : W.t) annotation =
  match Qdrant.annotation_point ~vectors:true w.index annotation with
  | Some point -> point
  | None -> failwith ("the index holds no point of the annotation " ^ annotation)

let annotation_added (w : W.t) =
  let resource =
    indexed w "Black Hawk led the Sauk, and the war ended in 1832.\n"
  in
  let annotation =
    once_applied w (fun () ->
        W.annotated w ~motivation:"linking"
          ~body:(`List [ W.tagging "Person"; W.tagging "Leader" ])
          resource "Black Hawk")
  in
  (* The table's point of an annotation: its id, its resource's, its
     motivation, the entity types its body states in their order, and the
     text it quotes. *)
  let point = point_of w annotation in
  Check.equal "the annotation's point"
    (annotation_point annotation resource "linking" [ "Person"; "Leader" ]
       "Black Hawk")
    (stated point);
  Check.that "the annotation's vector is its quotation's"
    (embeds point "Black Hawk");
  Check.equal "how many annotations' points the resource has" (`Int 1)
    (`Int (List.length (annotations w resource)));
  once_applied w (fun () -> W.unannotated w resource annotation);
  none "the annotations' points once it is removed" (annotations w resource)

let body_tagged (w : W.t) =
  let resource =
    indexed w "Keokuk signed the treaty of 1832 at Fort Armstrong.\n"
  in
  let annotation =
    once_applied w (fun () ->
        W.annotated w ~motivation:"linking"
          ~body:(`List [ W.tagging "Person" ])
          resource "Keokuk")
  in
  let asked = Ollama_embed.mark w.provider in
  once_applied w (fun () -> W.body_tagged w resource annotation [ "Leader" ]);
  (* The same point of the same text: only its entity types are other than
     they were, in the order its body now states them. *)
  let point = point_of w annotation in
  Check.equal "the annotation's point"
    (annotation_point annotation resource "linking" [ "Person"; "Leader" ]
       "Keokuk")
    (stated point);
  Check.that "the annotation's vector is its quotation's"
    (embeds point "Keokuk");
  Check.that "the provider was asked nothing" (asked_since w asked = [])

let quotes_nothing (w : W.t) =
  let resource =
    indexed w "Wabokieshiek counselled the band to cross the river.\n"
  in
  let quoting =
    once_applied w (fun () -> W.annotated w resource "Wabokieshiek")
  in
  let whole = once_applied w (fun () -> W.annotated_whole w resource) in
  ignore (once_applied w (fun () -> W.annotated w resource " \n\t") : string);
  (* Each of the three has been applied, and the resource's one point of an
     annotation is of the one that quotes text. *)
  let points = annotations w ~vectors:true resource in
  Check.equal "the annotations' points" (strs [ quoting ])
    (annotation_ids points);
  once_applied w (fun () -> W.unannotated w resource whole);
  Check.equal "the annotations' points once the whole one is removed"
    (`List points)
    (`List (annotations w ~vectors:true resource))

let entity_types_rewritten (w : W.t) =
  let resource = indexed w (long_text "a text that is given entity types") in
  let before = resources w ~vectors:true resource in
  Check.that "the text is more than two points" (List.length before > 2);
  let asked = Ollama_embed.mark w.provider in
  (* Each point as it was, its vector too, but for its entity types. *)
  let with_types types = `List (List.map (with_entity_types types) before) in
  once_applied w (fun () -> W.tagged w resource "Person");
  once_applied w (fun () -> W.tagged w resource "Event");
  Check.equal "how many entity types the record holds of it" (`Int 2)
    (`Int (List.length (W.entity_types w resource)));
  Check.equal "the points"
    (with_types (W.entity_types w resource))
    (`List (resources w ~vectors:true resource));
  once_applied w (fun () -> W.untagged w resource "Person");
  Check.equal "the entity types the record holds of it" (strs [ "Event" ])
    (strs (W.entity_types w resource));
  Check.equal "the points" (with_types [ "Event" ])
    (`List (resources w ~vectors:true resource));
  Check.that "the provider was asked nothing" (asked_since w asked = [])

let tagged_without_points (w : W.t) =
  let text = "Saukenuk stood where the Rock River meets the Mississippi.\n" in
  let resource = indexed w text in
  once_applied w (fun () -> W.archived w resource);
  none "the points once archived" (resources w resource);
  once_applied w (fun () -> W.tagged w resource "Place");
  none "the points once given an entity type" (resources w resource);
  (* It is indexed next when it is unarchived, with the entity types the
     record holds of it then. *)
  once_applied w (fun () -> W.unarchived w resource);
  Check.equal "the points once unarchived"
    (`List [ chunk_point resource 0 text (W.sha256 text) [ "Place" ] ])
    (stated_all (resources w resource))

let image (w : W.t) =
  let resource = once_applied w (fun () -> W.created w png "image/png") in
  Check.equal "the decision"
    (decision ~reason:"no-extractor" resource (W.sha256 png) "skipped")
    (W.settled w resource);
  none "the points" (resources w resource)

let blank (w : W.t) =
  let blank = " \n\t\n" in
  let resource = once_applied w (fun () -> W.created w blank plain) in
  Check.equal "the decision"
    (decision ~reason:"empty" resource (W.sha256 blank) "skipped")
    (W.settled w resource);
  none "the points" (resources w resource);
  let had = indexed w "A text that is indexed, and then emptied.\n" in
  Check.equal "how many points it had" (`Int 1)
    (`Int (List.length (resources w had)));
  let emptied = "\n\n   \n" in
  ignore (once_applied w (fun () -> W.updated w had emptied) : string);
  Check.equal "the second decision"
    (decision ~reason:"empty" had (W.sha256 emptied) "skipped")
    (W.settled w ~nth:2 had);
  none "the points once emptied" (resources w had)

let skips_logged (w : W.t) =
  let image = "No picture, though it is recorded as one." in
  let blank = "\t\n \n" in
  let skips =
    [
      ( once_applied w (fun () -> W.created w image "image/png"),
        "no-extractor",
        "image/png",
        W.sha256 image );
      ( once_applied w (fun () -> W.created w blank plain),
        "empty",
        plain,
        W.sha256 blank );
    ]
  in
  (* Each is logged as smelter/messages.json's not-indexed says, at its
     level. *)
  List.iter
    (fun (resource, reason, media_type, checksum) ->
      ignore
        (logged_line
           (fun () -> W.output w)
           "not-indexed"
           [
             ("resourceId", resource);
             ("reason", reason);
             ("mediaType", media_type);
             ("checksum", checksum);
           ]
          : Yojson.Safe.t))
    skips

let unembedded (w : W.t) =
  let text = "Keokuk kept his band west of the river.\n" in
  let resource = indexed w text in
  let before = only_point "the points" w resource text [] in
  (* The provider fails every request that carries the new text. *)
  let refused = "vectors the provider will not give" in
  w.provider.choose <-
    Some
      (fun (request : Ollama_embed.recorded) ->
        if List.exists (mentions refused) request.texts then
          Some
            (Ollama_embed.Fails
               (500, {|{"error":"the stand-in gives no vectors for this text"}|}))
        else None);
  let asked = Ollama_embed.mark w.provider in
  ignore (W.updated w resource ("A text of " ^ refused ^ ".\n") : string);
  Run.eventually
    "the provider to be asked for the vectors of the new text, and to fail the \
     request"
    20.0 (fun () ->
      List.find_opt
        (fun (r : Ollama_embed.recorded) -> r.status = Some 500)
        (asked_since w asked))
  |> ignore;
  (* Nothing is said of the new bytes, and the points are still those of the
     old. *)
  W.unsettled w ~nth:2 resource;
  Check.equal "the points" (`List before)
    (`List (resources w ~vectors:true resource))

let unreadable (w : W.t) =
  let text = "Black Hawk dictated his life to an interpreter.\n" in
  let resource = indexed w text in
  let before = only_point "the points" w resource text [] in
  (* The bytes are taken from where the record says they are stored: the
     Archivist has none to serve. An unarchive is then recorded, which makes
     a Smelter index the resource and so read its bytes. The resource was
     never archived, so its points are in the index when that read fails. *)
  Sys.remove (W.content_file w resource);
  let written = List.length (W.output w) in
  W.unarchived w resource;
  (* The failure is logged, as smelter/messages.json's content-unread says,
     in a line written since. *)
  ignore
    (logged_line ~timeout:20.0
       (fun () -> List.filteri (fun at _ -> at >= written) (W.output w))
       "content-unread"
       [ ("resourceId", resource) ]
      : Yojson.Safe.t);
  (* It goes on: to the next event of that resource, which is applied after
     the one that failed, and to another resource's. *)
  let quoted = W.annotated w resource "an interpreter" in
  let next =
    W.created w "Recorded after bytes that could not be read.\n" plain
  in
  Run.eventually
    "the point of an annotation recorded after the bytes could not be read" 20.0
    (fun () -> Qdrant.annotation_point w.index quoted)
  |> ignore;
  Check.like "the next resource's decision"
    (`Assoc [ ("outcome", str "indexed") ])
    (W.settled w next);
  Check.equal "how many points the next resource has" (`Int 1)
    (`Int (List.length (resources w next)));
  (* Nothing was decided of the bytes it could not read, and the resource's
     points are those of the bytes it last read. *)
  W.unsettled w ~nth:2 resource;
  Check.equal "the points" (`List before)
    (`List (resources w ~vectors:true resource))

let tests implementations =
  in_world implementations "index"
    ~category:[ "a Smelter, as the record changes" ]
    [
      Case.case
        "indexes a text that is recorded: it says `indexed` of exactly the \
         bytes recorded, once the points the table states are written"
        recorded;
      Case.case
        "cuts a text longer than one chunk into several, has each embedded, \
         and writes each as a point of its own, numbered from 0"
        cut;
      Case.case
        "indexes a resource again when its content is updated: the new bytes \
         are settled under their own checksum, and no point of the old text is \
         left"
        updated;
      Case.case
        "takes an archived resource out of the index with the annotations on \
         it, and puts both back when it is unarchived"
        archived;
      Case.case
        "gives no point to an annotation made on an archived resource, and \
         gives it one when the resource is unarchived"
        annotated_while_archived;
      Case.case
        "indexes an annotation that is added as one point, of the text it \
         quotes, and takes the point out when the annotation is removed"
        annotation_added;
      Case.case
        "rewrites the entity types on an annotation's point when its body is \
         given a tag, and embeds nothing"
        body_tagged;
      Case.case
        "gives no point to an annotation that quotes no text, or only white \
         space; and removing one that has no point changes nothing"
        quotes_nothing;
      Case.case
        "rewrites the entity types on every point of a resource when one is \
         added or removed, and asks its provider nothing"
        entity_types_rewritten;
      Case.case
        "leaves a resource that has no points with none when it is given an \
         entity type, and writes the type when the resource is next indexed"
        tagged_without_points;
      Case.case
        "skips an image, whose media type has no text: it says `skipped` for \
         `no-extractor`, and the image has no points"
        image;
      Case.case
        "skips a text of nothing but white space, for `empty`; and a resource \
         that had points loses them when it is updated to such a text"
        blank;
      Case.case
        "logs every skip as a warning, with the resource, the reason, the \
         media type and the checksum"
        skips_logged;
      Case.case
        "writes no point until every chunk is embedded: a resource whose new \
         text could not be embedded keeps the points it had"
        unembedded;
      Case.case
        "decides nothing of bytes it cannot read: it signals nothing and \
         leaves the index as it was, and goes on to apply what is recorded \
         next"
        unreadable;
    ]
