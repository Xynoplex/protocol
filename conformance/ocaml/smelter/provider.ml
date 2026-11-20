(* provider.test.ts: how a Smelter asks its embedding provider for vectors
   (SMELTER.md § The embedding provider), as far as that shows from outside:
   how many texts go in one request, that an Ollama is asked for one thing at
   a time, and what becomes of a resource when a request for it fails. With
   it, that what a failed event left undone is found when the Smelter next
   starts (§ Order).

   The provider is the suite's stand-in for an Ollama, and what an Ollama is
   asked is specs/src/embedding/requests.json's. What a Smelter does when
   that provider has not fetched its model yet is boot's. That a request has
   a time of its own is not held here, and nothing of Voyage. *)

open Xynolog_conformance
open Support

(* How an Ollama is asked, as embedding/requests.json states it. *)
let ollama () =
  match
    List.assoc_opt "ollama" (Protocol.tables ()).embedding_requests.providers
  with
  | Some asked -> asked
  | None -> failwith "embedding/requests.json states no ollama"

(* How many texts one request carries, at most. *)
let slice () = (ollama ()).texts_per_request

(* How long a text is that is cut into more chunks than one request
   carries. The world's Smelter cuts by a chunk size of 512 and an overlap
   of 64, which specs/src/text/chunk-cases.json counts in code points, four
   to one: a chunk is taken from a window of 2,048, and the next begins 256
   before where it was cut. A cut after a sentence's full stop may be one
   past its window, so no chunk begins more than 1,793 after the one before,
   and the last begins within 2,048 of the text's end. A text is therefore
   more than [slice] chunks when it is longer than ([slice] - 1) x 1,793 +
   2,048 code points. This is that, and a margin. *)
let more_than_a_slice () = ((slice () - 1) * 1_793) + 2_048 + 50_000

(* A text of numbered sentences, long enough to be cut into more chunks than
   one request carries. No two of its sentences are the same, so no two of
   its chunks are, and a vector given to another chunk than its own would
   show. *)
let numbered about =
  let wanted = more_than_a_slice () in
  let text = Buffer.create (wanted + 100) in
  let rec add number =
    if Buffer.length text < wanted then (
      Buffer.add_string text
        (Printf.sprintf "Sentence %d of %s says only that it is sentence %d. "
           number about number);
      add (number + 1))
  in
  add 1;
  String.trim (Buffer.contents text)

let resources (w : W.t) ?vectors id =
  Qdrant.points_of ?vectors w.index "resources" id

let texts_since (w : W.t) mark =
  List.map
    (fun (r : Ollama_embed.recorded) -> strs r.texts)
    (Ollama_embed.since w.provider mark)

let statuses_since (w : W.t) mark =
  List.map
    (fun (r : Ollama_embed.recorded) ->
      match r.status with Some s -> `Int s | None -> `Null)
    (Ollama_embed.since w.provider mark)

let indexed what (w : W.t) ?timeout id =
  Check.like what
    (`Assoc [ ("outcome", str "indexed") ])
    (W.settled w ?timeout id)

let sliced (w : W.t) =
  let slice = slice () in
  let before = Ollama_embed.mark w.provider in
  let id =
    W.created w (numbered "the sliced text") plain ~name:"More than one slice"
  in
  (* There is more to read, cut and write than for the short texts other
     cases record: it is given longer. *)
  indexed "the decision" w ~timeout:40.0 id;
  let points = resources w ~vectors:true id in
  let chunks = List.map payload_text points in
  Check.that "the text is more chunks than one request carries"
    (List.length chunks > slice);
  Check.that "no two chunks are the same"
    (List.length (List.sort_uniq compare chunks) = List.length chunks);
  (* The requests are the chunks, in their order, [slice] to a request until
     they run out. *)
  let rec slices = function
    | [] -> []
    | chunks ->
        let taken = List.filteri (fun at _ -> at < slice) chunks in
        strs taken :: slices (List.filteri (fun at _ -> at >= slice) chunks)
  in
  let sent = texts_since w before in
  Check.equal "how many texts each request carried"
    (`List
       (List.map
          (fun s -> `Int (List.length (Yojson.Safe.Util.to_list s)))
          (slices chunks)))
    (`List
       (List.map
          (fun s -> `Int (List.length (Yojson.Safe.Util.to_list s)))
          sent));
  Check.equal "the requests" (`List (slices chunks)) (`List sent);
  (* Each chunk has the vector of its own text, though its text went in one
     request among many texts, and in one request of several. *)
  Check.equal "the chunks whose vector is not their text's" (`List [])
    (`List
       (List.filter_map
          (fun point ->
            if embeds point (payload_text point) then None
            else
              Some
                (Check.at (payload point)
                   [ Qdrant.payload_member "resources" "chunkIndex" ]))
          points))

let one_at_a_time (w : W.t) =
  Check.equal
    "how many requests embedding/requests.json says an Ollama is asked at once"
    (`Int 1) (`Int (ollama ()).at_once);
  let first = "A text whose vector the provider is slow to give." in
  let second =
    "A text recorded while the provider has not yet answered for another."
  in
  let before = Ollama_embed.mark w.provider in
  (* The next request is accepted and not answered; any after it is
     answered at once. *)
  Ollama_embed.script w.provider [ Ollama_embed.Hold ];
  let slow = W.created w first plain ~name:"Asked for first" in
  Run.eventually "the provider to be asked for the first text's vector" 10.0
    (fun () -> if Ollama_embed.held w.provider = 1 then Some () else None);
  (* A second resource is recorded while that request is unanswered. Were a
     request made for it now, it would be answered, and the resource
     indexed: nothing is decided of it, and the provider has been asked
     once. *)
  let waiting = W.created w second plain ~name:"Asked for second" in
  W.unsettled w waiting;
  Check.equal "the requests"
    (`List [ strs [ first ] ])
    (`List (texts_since w before));
  Check.equal "how many requests are held" (`Int 1)
    (`Int (Ollama_embed.held w.provider));
  (* Answered well inside the time a Smelter gives a request, the first is
     indexed, and then the second is asked for. *)
  Ollama_embed.release w.provider;
  indexed "the first decision" w slow;
  indexed "the second decision" w waiting;
  Check.equal "the requests"
    (`List [ strs [ first ]; strs [ second ] ])
    (`List (texts_since w before));
  Check.equal "how many points the first has" (`Int 1)
    (`Int (List.length (resources w slow)));
  Check.equal "how many points the second has" (`Int 1)
    (`Int (List.length (resources w waiting)))

let busy (w : W.t) =
  let text = "A text whose first request finds the provider busy.\n" in
  let before = Ollama_embed.mark w.provider in
  let busy = ref 1 in
  (* The first request for this text is answered 503, and the same request
     asked again is answered with its vectors. *)
  w.provider.choose <-
    Some
      (fun (request : Ollama_embed.recorded) ->
        if List.mem text request.texts && !busy > 0 then (
          decr busy;
          Some (Ollama_embed.Fails (503, {|{"error":"server busy"}|})))
        else None);
  let resource =
    W.created w text plain ~name:"Recorded while the provider was busy"
  in
  indexed "the decision" w resource;
  Check.equal "the statuses its requests were answered with"
    (`List [ `Int 503; `Int 200 ])
    (`List
       (List.filter_map
          (fun (r : Ollama_embed.recorded) ->
            if List.mem text r.texts then
              Some (match r.status with Some s -> `Int s | None -> `Null)
            else None)
          (Ollama_embed.since w.provider before)));
  Check.equal "how many points it has" (`Int 1)
    (`Int (List.length (resources w resource)))

let batch_fails (w : W.t) =
  let text = numbered "the refused text" in
  (* The text's last sentence is in its last chunk and in no other, so the
     request that carries it is the last of the batch. *)
  let last =
    let marker = "Sentence " in
    let rec from at =
      if at < 0 then failwith "the text holds no sentence"
      else if String.sub text at (String.length marker) = marker then
        String.sub text at (String.length text - at)
      else from (at - 1)
    in
    from (String.length text - String.length marker)
  in
  let said = List.length (W.output w) in
  let before = Ollama_embed.mark w.provider in
  w.provider.choose <-
    Some
      (fun (request : Ollama_embed.recorded) ->
        if List.exists (mentions last) request.texts then
          Some (Ollama_embed.Fails (500, {|{"error":"the model failed"}|}))
        else None);
  let refused = W.created w text plain ~name:"Refused part way" in
  Run.eventually "the provider to have refused the last request of the batch"
    40.0 (fun () ->
      if List.mem (`Int 500) (statuses_since w before) then Some () else None);
  (* The request before it was answered with its vectors: the batch is failed
     by one request of two. *)
  Check.equal "the statuses its requests were answered with"
    (`List [ `Int 200; `Int 500 ])
    (`List (statuses_since w before));
  (* It says which provider it asked, for which model, what status it was
     answered with, and which slice of the batch that was: as
     smelter/messages.json's event-failed says. *)
  ignore
    (logged_line
       (fun () -> List.filteri (fun at _ -> at >= said) (W.output w))
       "event-failed"
       [
         ("resourceId", refused);
         ("provider", "ollama");
         ("model", W.embedding_model);
         ("status", "500");
       ]
      : Yojson.Safe.t);
  (* The failure stops that resource's log alone: what is recorded next, of
     another resource, is indexed. *)
  let next =
    W.created w "A text recorded after a request for another text failed." plain
      ~name:"Recorded after the refusal"
  in
  indexed "the next decision" w next;
  (* Nothing is decided of the resource whose request failed, and it has no
     points: not those of the chunks its first request embedded. *)
  W.unsettled w refused;
  Check.equal "its points" (`List []) (`List (resources w refused));
  (* What that event left undone is found when the Smelter next starts, with
     a provider that answers. *)
  ignore (W.stop w : int option);
  w.provider.choose <- None;
  W.start w;
  Check.like "its reconcile"
    (`Assoc
       [
         ("phase", str "done");
         ( "summary",
           `Assoc
             [ ("resourcesEmbedded", count 1); ("resourcesFailed", count 0) ] );
       ])
    (Check.at (W.health w) [ "reconcile" ]);
  indexed "its decision" w refused;
  Check.that "it has more points than one request carries"
    (List.length (resources w refused) > slice ())

let tests implementations =
  in_world implementations "provider"
    ~category:[ "a Smelter, asking its embedding provider" ]
    [
      Case.case
        (Printf.sprintf
           "cuts a batch of texts into slices of %d, each one request, and \
            gives each text the vector that came back for it"
           (slice ()))
        sliced;
      Case.case
        "makes one request at a time of an Ollama, however many resources are \
         being indexed"
        one_at_a_time;
      Case.case
        "asks its provider again when it says it is busy, and indexes the \
         resource with no restart"
        busy;
      Case.case
        "gives a resource no points when a request of its batch fails, and \
         finds the resource again when it next starts"
        batch_fails;
    ]
