(* A schema's [pattern], read as ECMA-262 reads it with the [u] flag:
   unanchored, over the text's code points.

   ocaml-re matches bytes, so a text is matched as its classes: each code
   point one byte that every pattern Protocol may write sorts as the code
   point would be sorted. An ASCII character stands for itself, but a
   carriage return, which [.] does not match, stands as a line feed. Any other
   line terminator stands as a line feed, any other white space as a tab, and
   any other code point as a byte no class names but a negated one. That holds
   for a pattern of printable ASCII whose only escaped letters are [d], [D],
   [s], [S], [w] and [W], and whose only group that is not plain is [(?:]: a
   pattern outside that is refused. *)

let white_space = function
  | 0x00A0 | 0x1680 | 0x202F | 0x205F | 0x3000 | 0xFEFF -> true
  | c -> c >= 0x2000 && c <= 0x200A

let line_terminator = function 0x2028 | 0x2029 -> true | _ -> false

let classes text =
  let out = Buffer.create (String.length text) in
  let n = String.length text in
  let rec go i =
    if i < n then (
      let lead = Char.code text.[i] in
      if lead < 0x80 then (
        Buffer.add_char out (if lead = 0x0D then '\n' else text.[i]);
        go (i + 1))
      else
        let width, bits =
          if lead >= 0xF0 then (4, lead land 0x07)
          else if lead >= 0xE0 then (3, lead land 0x0F)
          else (2, lead land 0x1F)
        in
        let code = ref bits in
        for k = 1 to width - 1 do
          code := (!code lsl 6) lor (Char.code text.[i + k] land 0x3F)
        done;
        Buffer.add_char out
          (if line_terminator !code then '\n'
           else if white_space !code then '\t'
           else '\x80');
        go (i + width))
  in
  go 0;
  Buffer.contents out

(* Why [pattern] is outside what [matches] reads as ECMA-262 does, if it
   is. *)
let refusal pattern =
  let n = String.length pattern in
  let rec scan i =
    if i >= n then None
    else
      let c = pattern.[i] in
      if c < ' ' || c > '~' then
        Some "holds a character that is not printable ASCII"
      else if c = '\\' then
        if i + 1 >= n then Some "ends in a lone backslash"
        else
          match pattern.[i + 1] with
          | 'd' | 'D' | 's' | 'S' | 'w' | 'W' -> scan (i + 2)
          | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as e ->
              Some
                (Printf.sprintf
                   "escapes %c, which ECMA-262 and ocaml-re read apart" e)
          | _ -> scan (i + 2)
      else if c = '(' && i + 1 < n && pattern.[i + 1] = '?' then
        if i + 2 < n && pattern.[i + 2] = ':' then scan (i + 3)
        else Some "has a group that is neither plain nor (?:"
      else scan (i + 1)
  in
  match scan 0 with
  | Some _ as found -> found
  | None -> (
      match Re.Perl.re pattern with
      | _ -> None
      | exception _ -> Some "is not a regular expression ocaml-re reads")

let compiled = Hashtbl.create 64

let matches pattern text =
  let re =
    match Hashtbl.find_opt compiled pattern with
    | Some re -> re
    | None ->
        let re = Re.compile (Re.Perl.re pattern) in
        Hashtbl.replace compiled pattern re;
        re
  in
  Re.execp re (classes text)
