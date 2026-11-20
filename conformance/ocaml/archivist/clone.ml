(* Clone tokens (ARCHIVIST.md § Clone tokens): a token issued for a resource,
   looked up, and spent on a copy. *)

open Xynolog_conformance
open Support
open Case

let token_of issued =
  match Protocol.at issued [ "token" ] with
  | Some (`String token) -> token
  | _ -> failwith ("no token was issued: " ^ Check.shown issued)

let seconds_from_now moment =
  match Ptime.of_rfc3339 moment with
  | Ok (at, _, _) -> Ptime.to_float_s at -. Unix.gettimeofday ()
  | Error _ -> failwith (moment ^ " is no RFC 3339 moment")

let made_with_a_token w =
  let owner = A.person w "owner" in
  let cloner = A.person w "cloner" in
  let types = `List (List.map str (entity_types 1)) in
  let source =
    A.created w owner.did
      [
        ("name", "Original");
        ("storageUri", "file://clones/original.md");
        ("entityTypes", Yojson.Safe.to_string types);
      ]
      "original\n"
  in
  let issued =
    A.ask cloner "yield:clone-token-requested"
      (`Assoc [ ("resourceId", str source) ])
  in
  let token = token_of issued in
  Check.that
    ("the token's form: " ^ token)
    (Check.matches_re "^clone_[0-9a-f]{32}$" token);
  like "the token's resource"
    (`Assoc [ ("@id", str source); ("name", str "Original") ])
    (Check.at issued [ "resource" ]);
  let expires_at = Check.at issued [ "expiresAt" ] in
  let lifetime =
    seconds_from_now
      (match expires_at with
      | `String at -> at
      | other -> failwith (Check.shown other ^ " is no moment"))
  in
  Check.that
    (Printf.sprintf "the token lives more than 14 minutes: %g s" lifetime)
    (lifetime > 14.0 *. 60.0);
  Check.that
    (Printf.sprintf "the token lives at most 15 minutes: %g s" lifetime)
    (lifetime <= 15.0 *. 60.0);
  (* Looking a token up does not spend it. *)
  for _ = 1 to 2 do
    like "the token looked up"
      (`Assoc
         [
           ("sourceResource", `Assoc [ ("@id", str source) ]);
           ("expiresAt", expires_at);
         ])
      (A.ask cloner "yield:clone-resource-requested"
         (`Assoc [ ("token", str token) ]))
  done;
  let copy = "a copy, edited\n" in
  let uri = "file://clones/copy.md" in
  A.write w uri copy;
  let clone =
    A.string_member "resourceId"
      (A.ask cloner "yield:clone-create"
         (`Assoc
            [
              ("token", str token);
              ("name", str "Copy");
              ("storageUri", str uri);
              ("contentChecksum", str (sha256 copy));
              ("byteSize", `Int (String.length copy));
              ("format", str "text/markdown");
            ]))
  in
  like "the copy's event"
    (`Assoc
       [
         ("type", str "yield:cloned");
         ("userId", str cloner.did);
         ( "payload",
           `Assoc
             [
               ("name", str "Copy");
               ("storageUri", str uri);
               ("contentChecksum", str (sha256 copy));
               ("parentResourceId", str source);
               ("entityTypes", types);
               ("creator", `Assoc [ ("@id", str cloner.did) ]);
               ("wasAttributedTo", `List [ `Assoc [ ("@id", str cloner.did) ] ]);
             ] );
         ("metadata", `Assoc [ ("sequenceNumber", `Int 1) ]);
       ])
    (List.hd (A.stored w clone));
  like "the copy's view"
    (`Assoc
       [
         ("name", str "Copy");
         ("sourceResourceId", str source);
         ("entityTypes", types);
         ( "representations",
           `List
             [
               `Assoc
                 [ ("storageUri", str uri); ("checksum", str (sha256 copy)) ];
             ] );
       ])
    (Check.at (Option.get (A.view w clone)) [ "resource" ]);
  Check.equal "the copy's storage-uri entry"
    (`Assoc [ ("uri", str uri); ("resourceId", str clone) ])
    (Option.get (A.storage_uri_entry w uri));
  (* The source is as it was. *)
  Check.equal "the source's archived" (`Bool false)
    (Check.at (Option.get (A.view w source)) [ "resource"; "archived" ]);
  (* The token is spent. *)
  Check.equal "the spent token looked up"
    (str (refusal "invalid-token" []))
    (str
       (A.refused cloner "yield:clone-resource-requested"
          (`Assoc [ ("token", str token) ])))

let made_by_an_upload w =
  let owner = A.person w "owner" in
  let source =
    A.created w owner.did
      [ ("name", "To replace"); ("storageUri", "file://clones/to-replace.md") ]
      "old\n"
  in
  let token =
    token_of
      (A.ask owner "yield:clone-token-requested"
         (`Assoc [ ("resourceId", str source) ]))
  in
  let clone =
    A.created w owner.did
      [
        ("name", "Replacement");
        ("storageUri", "file://clones/replacement.md");
        ("cloneToken", token);
        ("archiveOriginal", "true");
      ]
      "new\n"
  in
  like "the copy's event"
    (`Assoc
       [
         ("type", str "yield:cloned");
         ("userId", str owner.did);
         ("payload", `Assoc [ ("parentResourceId", str source) ]);
       ])
    (List.hd (A.stored w clone));
  Run.eventually "the source to be archived" 5.0 (fun () ->
      match
        Protocol.at (Option.get (A.view w source)) [ "resource"; "archived" ]
      with
      | Some (`Bool true) -> Some ()
      | _ -> None);
  like "the source's last event"
    (`Assoc [ ("type", str "mark:archived"); ("userId", str owner.did) ])
    (List.hd (List.rev (A.stored w source)))

let refused_without_a_source w =
  let cloner = A.person w "cloner" in
  let unissued = "clone_" ^ String.make 32 '0' in
  Check.equal "a token for a resource it does not hold"
    (str (refusal "clone-source-not-found" []))
    (str
       (A.refused cloner "yield:clone-token-requested"
          (`Assoc [ ("resourceId", str (String.make 32 'd')) ])));
  Check.equal "a token it did not issue, looked up"
    (str (refusal "invalid-token" []))
    (str
       (A.refused cloner "yield:clone-resource-requested"
          (`Assoc [ ("token", str unissued) ])));
  Check.equal "a token it did not issue, spent"
    (str (refusal "invalid-token" []))
    (str
       (A.refused cloner "yield:clone-create"
          (`Assoc
             [
               ("token", str unissued);
               ("name", str "x");
               ("storageUri", str "file://clones/x.md");
               ("contentChecksum", str (sha256 "x"));
               ("byteSize", `Int 1);
               ("format", str "text/markdown");
             ])));
  let uri = "file://clones/gone.md" in
  let gone =
    A.created w cloner.did [ ("name", "Gone"); ("storageUri", uri) ] "gone\n"
  in
  ignore
    (A.ask cloner "mark:archive"
       (`Assoc [ ("resourceId", str gone); ("storageUri", str uri) ])
      : Yojson.Safe.t);
  Check.equal "a token for a resource whose content is gone"
    (str (refusal "clone-content-not-found" []))
    (str
       (A.refused cloner "yield:clone-token-requested"
          (`Assoc [ ("resourceId", str gone) ])))

let tests implementations =
  with_archivist implementations "a clone"
    [
      case "is made with a token, as a copy that names its source"
        made_with_a_token;
      case
        "is made by an upload that carries the token, archiving the source \
         when asked"
        made_by_an_upload;
      case
        "is refused without a token that names a resource whose content is \
         there"
        refused_without_a_source;
    ]
