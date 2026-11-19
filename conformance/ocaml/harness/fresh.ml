(* Values no run has used before: secrets, keys and ids. *)

let seeded = lazy (Mirage_crypto_rng_unix.use_default ())

let bytes n =
  Lazy.force seeded;
  Mirage_crypto_rng.generate n

let hex n = Ohex.encode (bytes n)

(* A version 4 UUID, as crypto.randomUUID writes one. *)
let uuid () =
  let b = Bytes.of_string (bytes 16) in
  Bytes.set b 6 (Char.chr (0x40 lor (Char.code (Bytes.get b 6) land 0x0f)));
  Bytes.set b 8 (Char.chr (0x80 lor (Char.code (Bytes.get b 8) land 0x3f)));
  let h = Ohex.encode (Bytes.to_string b) in
  String.concat "-"
    [
      String.sub h 0 8;
      String.sub h 8 4;
      String.sub h 12 4;
      String.sub h 16 4;
      String.sub h 20 12;
    ]
