(* File: fcgi_protocol.ml

   Copyright (C) 2016-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/
 *)

module BE = EndianBytes.BigEndian_unsafe
open Result


module type IO = sig
  type +'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  type ic
  type oc

  val read_into : ic -> Bytes.t -> int -> int -> int t
  val write_from : oc -> Bytes.t -> int -> int -> int t
  val flush : oc -> unit t
end

module type RecordIO = sig
  module IO : IO
  type ty = | Management | Begin_request | Abort_Request | End_Request
            | Params | Stdin | Stdout | Stderr | Data
            | Get_values | Get_values_result | Unknown

  val ( >>=? ) :
    ('a, 'b) result IO.t -> ('a -> ('c, 'b) result IO.t) -> ('c, 'b) result IO.t

  type ic
  val make_input : IO.ic -> ic
  val read : ic -> (unit, [`EOF]) result IO.t
  val input_type : ic -> ty
  val id : ic -> int
  val data : ic -> Bytes.t
  val length : ic -> int

  type oc
  val make_output : IO.oc -> oc
  val set_type : oc -> ty -> unit
  val set_id : oc -> int -> unit
  val send : oc -> (unit, [`Write_error]) result IO.t
end

module Make_RecordIO(IO: IO) = struct
  module IO = IO

  let fcgi_version = '\001'

  type ty = | Management | Begin_request | Abort_Request | End_Request
            | Params | Stdin | Stdout | Stderr | Data
            | Get_values | Get_values_result | Unknown

  let char_of_ty = function
    | Management -> '\000'
    | Begin_request -> '\001'
    | Abort_Request -> '\002'
    | End_Request -> '\003'
    | Params -> '\004'
    | Stdin -> '\005'
    | Stdout -> '\006'
    | Stderr -> '\007'
    | Data -> '\008'
    | Get_values -> '\009'
    | Get_values_result -> '\010'
    | Unknown -> '\011'

  let ty_of_int = [| Management; Begin_request; Abort_Request; End_Request;
                     Params; Stdin; Stdout; Stderr; Data;
                     Get_values; Get_values_result; Unknown |]

  let ( >>= ) = IO.( >>= )

  let ( >>=? ) m f =
    m >>= (function Ok v -> f v | Error _ as e -> IO.return e)

  let data_len_max = 0xFFFF + 0xFF

  type ic = {
      ic: IO.ic;
      mutable ty: ty;  (* input type, cache for easy access *)
      mutable id: int; (* input ID, cache for easy access *)
      head: Bytes.t; (* buffer for the 8 first bytes of the FCGI record *)
      data: Bytes.t; (* data + padding buffer *)
      mutable length: int; (* _data_ length *)
      mutable closed: bool;
    }

  let input_type ic = ic.ty
  let id ic = ic.id
  let data ic = ic.data
  let length ic = ic.length

  let make_input ic =
    let head = Bytes.create 8 in
    let data = Bytes.create data_len_max in
    { ty = Unknown;  id = (-1);
      ic;  head;  data;  length = 0;  closed = false  }

  let rec really_read ic buf ~ofs len =
    if len <= 0 then IO.return(Ok()) (* even if closed *)
    else (
      IO.read_into ic buf ofs len >>= fun nread ->
      if nread > 0  then really_read ic buf (ofs + nread) (len - nread)
      else IO.return(Error `EOF) (* Input EOF; desired [len] > 0 *)
    )

  (* FIXME: we want to be able to choose the buffer according to the ID. *)
  (* WARNING: you do not want several [ic] on a single [IO.ic] because
     there are several reads and you do not want the header to be read
     by some function and the data by another. *)
  let read ic =
    really_read ic.ic ic.head ~ofs:0 8 >>=? fun () ->
    let ty = BE.get_uint8 ic.head 1 in
    ic.ty <- (if ty <= 11 then ty_of_int.(ty) else Unknown);
    ic.id <- BE.get_uint16 ic.head 2;
    let content_length = BE.get_uint16 ic.head 4 in
    ic.length <- content_length;
    let padding_length = BE.get_uint8 ic.head 6 in
    really_read ic.ic ic.data ~ofs:0 (content_length + padding_length)


  type oc = {
      oc: IO.oc;
      buf: Bytes.t; (* output buffer *)
      mutable ofs: int; (* Output _data_ offset; data length = oofs - 8 *)
    }

  let make_output oc =
    let buf = Bytes.create (8 + data_len_max) in
    Bytes.set buf 0 fcgi_version;  (* never modified *)
    { oc;  buf;  ofs = 8 }

  let set_type oc ty =
    Bytes.set oc.buf 1 (char_of_ty ty)

  let set_id oc id =
    assert(0 <= id && id <= 0xFFFF);
    BE.set_int16 oc.buf 2 id

  let send oc =
    let content_length = oc.ofs - 8 in
    if content_length <= 0 then IO.return(Ok()) (* nothing to do *)
    else (
      let rem = content_length land 0x7 (* = mod 8 *) in
      let padding_length = if rem = 0 then 0 else 8 - rem in
      BE.set_int16 oc.buf 4 content_length;
      BE.set_int8 oc.buf 6 padding_length;
      let to_write = 8 + content_length + padding_length in
      IO.write_from oc.oc oc.buf 0 to_write >>= fun written ->
      IO.return(if written <> to_write then Error `Write_error
                else Ok())
    )
end


module type CLIENT = sig
  module IO : IO



  val handle_connection :
    ?max_reqs: int ->
    (Cohttp.Request.t -> Cohttp_lwt_body.t ->
     (Cohttp.Response.t * Cohttp_lwt_body.t) IO.t) ->
    IO.ic -> IO.oc -> unit IO.t
end

module Client(P: RecordIO) = struct
  module IO = P.IO

  type t


  let handle_connection ?(max_reqs=1) f ic oc =

    IO.return()
end


module type SERVER = sig
  module IO : IO

  type t
  val handle_connection : ?keep_conn: bool -> IO.ic -> IO.oc -> t
  val get_values : t -> string list -> (string * string) list IO.t
  val close : ?force:bool -> t -> (unit, exn) result IO.t
end

module Server(P: RecordIO) = struct
  module IO = P.IO

  type t = {
      ic: P.ic;
      oc: P.oc;
    }


  let handle_connection ?(keep_conn=false) ic oc =
    let ic = P.make_input ic in
    let oc = P.make_output oc in
    { ic; oc }

  let get_values t names =
    IO.return []

  let close ?(force=false) t =
    IO.return(Ok())

end
