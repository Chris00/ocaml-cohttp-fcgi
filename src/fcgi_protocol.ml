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
  val make_input : IO.ic -> ic
  val create_data : unit -> Bytes.t
  val read_into : ic -> Bytes.t -> (int, [`EOF]) result IO.t
  val input_type : ic -> ty
  val id : ic -> int

  val create_record : unit -> Bytes.t
  val set_type : Bytes.t -> ty -> unit
  val set_id : Bytes.t -> int -> unit
  val write_from : IO.oc -> Bytes.t -> data_len:int
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

  let data_len_max = 0xFFFF + 0xFF

  type ic = {
      ic: IO.ic;
      mutable ty: ty;  (* input type, cache for easy access *)
      mutable id: int; (* input ID, cache for easy access *)
      head: Bytes.t; (* buffer for the 8 first bytes of the FCGI record *)
      mutable closed: bool;
    }

  let input_type ic = ic.ty
  let id ic = ic.id

  let make_input ic =
    let head = Bytes.create 8 in
    { ty = Unknown;  id = (-1);
      ic;  head;  closed = false  }

  let create_data () =
    Bytes.create data_len_max

  let rec really_read ic buf ~ofs len =
    if len <= 0 then IO.return(Ok()) (* even if closed *)
    else (
      IO.read_into ic buf ofs len >>= fun nread ->
      if nread > 0  then really_read ic buf (ofs + nread) (len - nread)
      else IO.return(Error `EOF) (* Input EOF; desired [len] > 0 *)
    )

  (* WARNING: you do not want several [ic] on a single [IO.ic] because
     there are several reads and you do not want the header to be read
     by some function and the data by another. *)
  let read_into ic data =
    really_read ic.ic ic.head ~ofs:0 8 >>=? fun () ->
    let ty = BE.get_uint8 ic.head 1 in
    ic.ty <- ty_of_int ty;
    ic.id <- BE.get_uint16 ic.head 2;
    let content_length = BE.get_uint16 ic.head 4 in
    let padding_length = BE.get_uint8 ic.head 6 in
    really_read ic.ic data ~ofs:0 (content_length + padding_length)
    >>=? fun () ->
    IO.return(Ok content_length)


  let create_record () =
    let buf = Bytes.create (8 + data_len_max) in
    Bytes.set buf 0 fcgi_version;  (* should never be modified *)
    buf

  let set_type buf ty =
    Bytes.set buf 1 (char_of_ty ty)

  let set_id buf id =
    assert(0 <= id && id <= 0xFFFF);
    BE.set_int16 buf 2 id

  let write_from oc buf ~data_len =
    assert(data_len <= 0xFFFF);
    if data_len <= 0 then IO.return(Ok()) (* nothing to do *)
    else (
      let rem = data_len land 0x7 (* = mod 8 *) in
      let padding_length = if rem = 0 then 0 else 8 - rem in
      BE.set_int16 buf 4 data_len;
      BE.set_int8 buf 6 padding_length;
      let to_write = 8 + data_len + padding_length in
      IO.write_from oc buf 0 to_write >>= fun written ->
      IO.flush oc >>= fun () ->
      IO.return(if written <> to_write then Error `Write_error
                else Ok())
    )
end


module type CLIENT = sig
  module IO : IO

  val handle_connection :
    ?max_reqs: int ->
    ?values:(string -> string option) ->
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
