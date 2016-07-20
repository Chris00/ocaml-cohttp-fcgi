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

  (* FIXME: what about these functions raisong exceptions? *)
  val read_into : ic -> Bytes.t -> int -> int -> int t
  val write_from : oc -> Bytes.t -> int -> int -> int t
  val flush : oc -> unit t
end

module type RecordIO = sig
  module IO : IO
  type ty = | Begin_request | Abort_Request | End_Request
            | Params | Stdin | Stdout | Stderr | Data
            | Get_values | Get_values_result | Unknown

  val ( >>=? ) :
    ('a, 'b) result IO.t -> ('a -> ('c, 'b) result IO.t) -> ('c, 'b) result IO.t

  type ic
  type head = {
      ty: ty;
      id: int;
      content_length: int;
      padding_length: int;
    }
  val make_input : IO.ic -> ic
  val create_data : unit -> Bytes.t
  val read_head : ic -> (head, [`EOF]) result IO.t
  val read_into : ic -> head -> Bytes.t -> (unit, [`EOF]) result IO.t
  val input_type : ic -> ty
  val id : ic -> int

  val create_record : unit -> Bytes.t
  val set_type : Bytes.t -> ty -> unit
  val set_id : Bytes.t -> int -> unit
  val write_from : IO.oc -> Bytes.t -> content_length:int
                   -> (unit, [`Write_error]) result IO.t
end

type record_ty =
  | Begin_request | Abort_Request | End_Request
  | Params | Stdin | Stdout | Stderr | Data
  | Get_values | Get_values_result | Unknown

let char_of_ty = function
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

let ty_of_int = [| Unknown; Begin_request; Abort_Request; End_Request;
                   Params; Stdin; Stdout; Stderr; Data;
                   Get_values; Get_values_result; Unknown |]

let ty_of_int i = if i <= 0 || i > 11 then Unknown
                  else ty_of_int.(i)

module Make_RecordIO(IO: IO) = struct
  module IO = IO

  let fcgi_version = '\001'

  type ty = record_ty =
    | Begin_request | Abort_Request | End_Request
    | Params | Stdin | Stdout | Stderr | Data
    | Get_values | Get_values_result | Unknown

  let ( >>= ) = IO.( >>= )

  let ( >>=? ) m f =
    m >>= (function Ok v -> f v | Error _ as e -> IO.return e)

  let data_len_max = 0xFFFF + 0xFF (* content + padding *)

  type ic = {
      ic: IO.ic;
      head: Bytes.t; (* buffer for the 8 first bytes of the FCGI record *)
      mutable closed: bool;
    }

  type head = {
      ty: ty;
      id: int;
      content_length: int;
      padding_length: int;
    }

  let make_input ic =
    let head = Bytes.create 8 in
    { ic;  head;  closed = false  }

  let create_data () =
    Bytes.create data_len_max

  let rec really_read ic buf ~ofs len =
    if len <= 0 then IO.return(Ok()) (* even if closed *)
    else (
      IO.read_into ic buf ofs len >>= fun nread ->
      if nread > 0  then really_read ic buf (ofs + nread) (len - nread)
      else IO.return(Error `EOF) (* Input EOF; desired [len] > 0 *)
    )

  let read_head ic =
    really_read ic.ic ic.head ~ofs:0 8 >>=? fun () ->
    let ty = ty_of_int(BE.get_uint8 ic.head 1) in
    let id = BE.get_uint16 ic.head 2 in
    let content_length = BE.get_uint16 ic.head 4 in
    let padding_length = BE.get_uint8 ic.head 6 in
    IO.return(Ok {ty; id; content_length; padding_length})

  let read_into ic head data =
    really_read ic.ic data ~ofs:0 (head.content_length + head.padding_length)

  let create_record () =
    let buf = Bytes.create (8 + data_len_max) in
    Bytes.set buf 0 fcgi_version;  (* should never be modified *)
    buf

  let set_type buf ty =
    Bytes.set buf 1 (char_of_ty ty)

  let set_id buf id =
    assert(0 <= id && id <= 0xFFFF);
    BE.set_int16 buf 2 id

  let write_from oc buf ~content_length =
    assert(content_length <= 0xFFFF);
    if content_length <= 0 then IO.return(Ok()) (* nothing to do *)
    else (
      let rem = content_length land 0x7 (* = mod 8 *) in
      let padding_length = if rem = 0 then 0 else 8 - rem in
      BE.set_int16 buf 4 content_length;
      BE.set_int8 buf 6 padding_length;
      let to_write = 8 + content_length + padding_length in
      IO.write_from oc buf 0 to_write >>= fun written ->
      IO.flush oc >>= fun () ->
      IO.return(if written <> to_write then Error `Write_error
                else Ok())
    )
end

(** Gets the length of a name or value and the offset of the next
    entry.  This new offset is set to [len] if the range [ofs] .. [ofs
    + len - 1] is not large enough. *)
let get_length buf ofs len =
  let b = BE.get_uint8 buf ofs in
  if b lsr 7 = 0 then (b, ofs + 1)
  else if ofs + 3 >= len then (-1, len)
  else (
    let b2 = BE.get_uint8 buf (ofs + 1) in
    let b10 = BE.get_uint16 buf (ofs + 2) in
    let l = ((b land 0x7F) lsl 24) lor (b2 lsl 16) lor b10 in
    (l, ofs + 4)
  )

(** Read the name-value pairs in buf.[ofs .. ofs+len-1] — assumed to
    be a valid range.  If the lengths of the keys or values are
    incompatible with the length of the range, return the keys so far.
    FIXME: do we want to return an error?  The application may not do
    much about it anyway. *)
let rec map_name_value buf ofs len f nv =
  if ofs >= len then nv
  else (
    let n_len, ofs = get_length buf ofs len in
    if ofs >= len then nv
    else (
      let v_len, ofs = get_length buf ofs len in
      let ofs_value = ofs + n_len in
      let ofs_next = ofs_value + v_len in
      if ofs_next > len then nv
      else (
        let name = String.uppercase_ascii(Bytes.sub_string buf ofs n_len) in
        let value = Bytes.sub_string buf ofs_value v_len in
        let nv = f name value :: nv in
        map_name_value buf ofs_next len f nv
      )
    )
  )


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



  let handle_connection ?(max_reqs=1) ?(values=fun _ -> None) f ic oc =

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
      oc: IO.oc;
    }


  let handle_connection ?(keep_conn=false) ic oc =
    let ic = P.make_input ic in
    { ic; oc }

  let get_values t names =
    IO.return []

  let close ?(force=false) t =
    IO.return(Ok())

end
