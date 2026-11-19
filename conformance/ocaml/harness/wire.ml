(* HTTP/1.1 on the wire: cohttp's request and response codecs, over
   cohttp-eio's IO, which the harness's server (Serve) and its client (Call)
   each run on a socket of their own. *)

module Request_io = Cohttp.Request.Private.Make (Cohttp_eio.Private.IO)
module Response_io = Cohttp.Response.Private.Make (Cohttp_eio.Private.IO)
