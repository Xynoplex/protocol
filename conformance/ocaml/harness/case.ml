(* A suite's case, before it is a test: its name, its bound, and how it
   runs on what its file shares. *)

type 'w t = {
  name : string;
  timeout : float;
  solo : string option;
  known : string option;
  run : 'w -> unit;
}

(* A case: [timeout] is its own bound, in seconds, as vitest's; its world's
   start is given a bound of its own on top. [solo], the reason, runs it in
   order with the run's other solo cases, in one process, before any other:
   for a case judged on what the cases before it did. [known] is why the
   implementation the suite is held to fails it, as a ruling of the
   protocol's records: the case is expected to fail, and fails the run once
   it passes. *)
let case ?(timeout = 60.0) ?solo ?known name run =
  { name; timeout; solo; known; run }

(* The test of [c] under [category]: [body] runs it. *)
let test ~category c body =
  Testo.create ~category ~max_duration:(c.timeout +. 60.0) ?solo:c.solo
    ?expected_outcome:(Option.map (fun why -> Testo.Should_fail why) c.known)
    c.name body
