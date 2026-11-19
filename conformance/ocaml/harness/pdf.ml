(* A PDF with one page for each entry of [pages], each page drawing its lines
   of text from the top down, written object by object with the byte
   offsets its cross-reference table states, so every implementation is
   given the same bytes. Its text is drawn in Helvetica, in ASCII alone. *)

let literal text =
  String.concat ""
    (List.map
       (function
         | ('\\' | '(' | ')') as mark -> Printf.sprintf "\\%c" mark
         | c -> String.make 1 c)
       (List.of_seq (String.to_seq text)))

let of_pages pages =
  let count = List.length pages in
  let page_numbers = List.init count (fun at -> 4 + (at * 2)) in
  let objects =
    [
      "<< /Type /Catalog /Pages 2 0 R >>";
      Printf.sprintf "<< /Type /Pages /Kids [%s] /Count %d >>"
        (String.concat " " (List.map (Printf.sprintf "%d 0 R") page_numbers))
        count;
      "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding \
       /WinAnsiEncoding >>";
    ]
    @ List.concat
        (List.mapi
           (fun at lines ->
             let drawn =
               if lines = [] then ""
               else
                 String.concat "\n"
                   ([ "BT"; "/F1 12 Tf"; "16 TL"; "72 720 Td" ]
                   @ List.map
                       (fun line -> Printf.sprintf "(%s) Tj T*" (literal line))
                       lines
                   @ [ "ET" ])
             in
             [
               Printf.sprintf
                 "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] \
                  /Resources << /Font << /F1 3 0 R >> >> /Contents %d 0 R >>"
                 (5 + (at * 2));
               Printf.sprintf "<< /Length %d >>\nstream\n%s\nendstream"
                 (String.length drawn) drawn;
             ])
           pages)
  in
  let written = Buffer.create 1024 in
  Buffer.add_string written "%PDF-1.4\n";
  let offsets =
    List.mapi
      (fun at object_ ->
        let offset = Buffer.length written in
        Buffer.add_string written
          (Printf.sprintf "%d 0 obj\n%s\nendobj\n" (at + 1) object_);
        offset)
      objects
  in
  let table = Buffer.length written in
  let size = List.length objects + 1 in
  Buffer.add_string written
    (Printf.sprintf "xref\n0 %d\n0000000000 65535 f \n" size);
  List.iter
    (fun offset ->
      Buffer.add_string written (Printf.sprintf "%010d 00000 n \n" offset))
    offsets;
  Buffer.add_string written
    (Printf.sprintf
       "trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n" size
       table);
  Buffer.contents written
