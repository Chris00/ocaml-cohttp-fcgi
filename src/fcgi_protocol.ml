(* File: fcgi_protocol.ml

   Copyright (C) 2016-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/
 *)

module BE = EndianBytes.BigEndian_unsafe
module ID = Cohttp_fcgi_ID
open Result


module type IO = sig
  type +'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val all : unit t list -> unit t

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
  val char_of_ty : ty -> char

  val ( >>=? ) :
    ('a, 'b) result IO.t -> ('a -> ('c, 'b) result IO.t) -> ('c, 'b) result IO.t

  type ic
  val make_input : IO.ic -> ic
  type head = private {
      ty: ty;
      id: int;
      content_length: int;
      padding_length: int;
    }
  val read_head : ic -> (head, [> `EOF]) result IO.t
  val read_into : ic -> head -> Bytes.t -> (unit, [> `EOF]) result IO.t
  val create_data : unit -> Bytes.t

  val create_record : unit -> Bytes.t
  val set_type : Bytes.t -> ty -> unit
  val set_id : Bytes.t -> int -> unit
  val write_from : IO.oc -> Bytes.t -> content_length:int
                   -> (unit, [> `Write_error]) result IO.t
end

module Make_RecordIO(IO: IO) = struct
  module IO = IO

  let fcgi_version = '\001'

  type ty =
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
    Bytes.create (8 + data_len_max)

  let set_type buf ty =
    Bytes.set buf 1 (char_of_ty ty)

  let set_id buf id =
    assert(0 <= id && id <= 0xFFFF);
    BE.set_int16 buf 2 id

  let write_from oc buf ~content_length =
    assert(content_length <= 0xFFFF);
    if content_length <= 0 then IO.return(Ok()) (* nothing to do *)
    else (
      Bytes.set buf 0 fcgi_version;
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

let management_id = 0

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
    be a valid range.  FIXME: If the lengths of the keys or values are
    too large for the range, that means that the rest of the key or
    value will be given on the next name-value record. *)
let rec fold_name_value buf ~ofs len f acc =
  if ofs >= len then acc
  else (
    let n_len, ofs = get_length buf ofs len in
    if ofs >= len then acc
    else (
      let v_len, ofs = get_length buf ofs len in
      let ofs_value = ofs + n_len in
      let ofs_next = ofs_value + v_len in
      if ofs_next > len then acc
      else (
        let name = Bytes.sub_string buf ofs n_len in
        let value = Bytes.sub_string buf ofs_value v_len in
        let acc = f name value acc in
        fold_name_value buf ~ofs:ofs_next len f acc
      )
    )
  )

(** Encode [length] according to the FastCGI spec, set it in [buf] at
    position [ofs] and return the new offset.  Raise an exception if
    [buf] is not long enough or if [length] does not fit the FastCGI
    format. *)
let encode_length_exn buf ~ofs len length =
  if length < 0x80 then (
    if ofs >= len then failwith "encode_length_exn";
    BE.set_int8 buf ofs length;
    ofs + 1
  )
  else if length < 0x8000_0000 then (
    if ofs + 3 >= len then failwith "encode_length_exn";
    BE.set_int16 buf ofs ((length lsr 16) lor 0x8000);
    BE.set_int16 buf (ofs + 2) (length land 0xFFFF);
    ofs + 4
  )
  else failwith "encode_length_exn"

(** Encode the name-value pair [(k,v)] to [buf] at position [ofs] and
    return the offset for the next value.  If there is not enough
    space in [buf], an exception is raised. *)
let add_name_val_exn buf ~ofs len k v =
  let k_len = String.length k in
  let v_len = String.length v in
  let ofs = encode_length_exn buf ~ofs len k_len in
  let ofs = encode_length_exn buf ~ofs len v_len in
  let v_ofs = ofs + k_len in
  let next_ofs = v_ofs + v_len in
  if next_ofs > len then failwith "add_name_val_exn"
  else (
    String.blit k 0 buf ofs k_len;
    String.blit v 0 buf v_ofs v_len;
    next_ofs
  )


module type CLIENT = sig
  module IO : IO

  type config = {
      max_conns: int;
      max_reqs: int;
      mpxs_conns: bool;
    }

  val default_config : config

  val handle_connection :
    config ->
    (Cohttp.Request.t -> Cohttp_lwt_body.t ->
     (Cohttp.Response.t * Cohttp_lwt_body.t) IO.t) ->
    IO.ic -> IO.oc -> unit IO.t
end

module Client(P: RecordIO) = struct
  module IO = P.IO

  let ( >>= ) = IO.( >>= )
  let ( >>=? ) = P.( >>=? )

  type config = {
      max_conns: int;
      max_reqs: int;
      mpxs_conns: bool;
    }

  (* Minimal capabilities. *)
  let default_config = { max_conns = 1;  max_reqs = 1;  mpxs_conns = false }

  (* Resources to handle a request. *)
  type request_state = Params | Body

  type request = {
      mutable req: Cohttp.Request.t;
      mutable state: request_state;
      body: Buffer.t; (* FIXME: versatility of storage must be implemented. *)
    }

  (* State of an accept()ed connection, managing concurrent requests. *)
  type t = {
      ic: P.ic;
      oc: IO.oc;
      buf: Bytes.t; (* Main loop buffer *)
      mutable n_reqs: int; (* Number of requests processed on this connection *)
      mutable close_conn: bool; (* whether the client should close the conn *)
      req: request ID.map; (* resources to handle requests *)
    }

  let rec set_config_pairs config nv buf ~ofs len =
    match nv with
    | name :: tl ->
       let name = String.uppercase name in
       (try
          if String.equal name "FCGI_MAX_CONNS" then
            let ofs = add_name_val_exn buf ~ofs len "FCGI_MAX_CONNS"
                        (string_of_int config.max_conns) in
            set_config_pairs config tl buf ~ofs len
          else if String.equal name "FCGI_MAX_REQS" then
            let ofs = add_name_val_exn buf ~ofs len "FCGI_MAX_REQS"
                        (string_of_int config.max_reqs) in
            set_config_pairs config tl buf ~ofs len
          else if String.equal name "FCGI_MPXS_CONNS" then
            let ofs = add_name_val_exn buf ~ofs len "FCGI_MPXS_CONNS"
                        (if config.mpxs_conns then "1" else "0") in
            set_config_pairs config tl buf ~ofs len
          else (* skip not understood name *)
            set_config_pairs config tl buf ~ofs len
        with _ -> (* problems with name-value pair.  Skip it *)
          set_config_pairs config tl buf ~ofs len)
    | [] -> ofs

  let add_name name _ acc = name :: acc

  let reply_to_get_values t config head =
    P.read_into t.ic head t.buf >>=? fun () ->
    let nv = fold_name_value t.buf ~ofs:0 head.P.content_length add_name [] in
    P.set_id t.buf management_id;
    P.set_type t.buf P.Get_values_result;
    let ofs = set_config_pairs config nv t.buf 8 (Bytes.length t.buf) in
    P.write_from t.oc t.buf ~content_length:(ofs - 8)

  let send_unknown_type t ty =
    P.set_id t.buf management_id;
    P.set_type t.buf P.Unknown;
    Bytes.set t.buf 8 (P.char_of_ty ty);
    P.write_from t.oc t.buf ~content_length:8

  type protocol_status = Request_complete
                       | Cant_mpx_conn
                       | Overloaded
                       | Unknown_role

  let send_end_request t ~id exit_code protocol_status =
    P.set_id t.buf id;
    P.set_type t.buf P.End_Request;
    BE.set_int32 t.buf 8 exit_code;
    Bytes.set t.buf 12 (match protocol_status with
                        | Request_complete -> '\000'
                        | Cant_mpx_conn -> '\001'
                        | Overloaded -> '\002'
                        | Unknown_role -> '\003');
    P.write_from t.oc t.buf ~content_length:0

  let handle_begin_request t head =
    P.read_into t.ic head t.buf >>=? fun () ->
    let close_conn = (BE.get_uint8 t.buf 2) land 0x1 = 0 in
    if BE.get_uint16 t.buf 0 = 1 (* Responder *) then (
      (* Setup some structures to handle the following records of this
         request, i.e., with the same ID as this one. *)
      t.n_reqs <- t.n_reqs + 1;
      t.close_conn <- t.close_conn || close_conn;
      (* If a previous request with the same ID exists, the server
         should not have send a begin_request with the same ID.  Drop
         this one with no warning. *)
      if not(ID.exists t.req head.P.id) then (
        let req = { Cohttp.Request.headers = Cohttp.Header.init();
                    meth = `GET;
                    resource = "";
                    version = `HTTP_1_1;
                    encoding = Cohttp.Transfer.Unknown } in
        let empty_request = { req;
                              state = Params;
                              body = Buffer.create 1024 } in
        ID.add t.req head.P.id empty_request
      );
      IO.return(Ok()))
    else
      (* Authorizer and filter roles not supported at the moment (very
         few web servers implement them). *)
      send_end_request t head.P.id 0l Unknown_role

  let handle_params t head f =
    P.read_into t.ic head t.buf >>=? fun () ->
    if ID.exists t.req head.P.id then
      let r = ID.find_exn t.req head.P.id in
      if r.state = Params then
        if head.P.content_length <> 0 then (
          (* FIXME: the params name & values may be split across
             several records. *)
          let h = r.req.Cohttp.Request.headers in
          let h = fold_name_value t.buf ~ofs:0 head.P.content_length
                    (fun n v h -> Cohttp.Header.add h n v) h in
          r.req <- { r.req with Cohttp.Request.headers = h };
          IO.return(Ok())
        )
        else ( (* Stream of parameters closed.  Launch the callback. *)
          r.state <- Body;
          (* FIXME: should connect to a pipe for the body. *)
          f r.req `Empty >>= fun (resp, body) ->
          (* FIXME: write response. *)
          IO.return(Ok())
        )
      else (* Not expecting params for that ID.  Drop record. *)
        IO.return(Ok())
    else
      (* Cannot process params when no Begin_request was sent.  Drop
         the record with no warning. *)
      IO.return(Ok())

  let rec handle_connection_loop t config f =
    P.read_head t.ic >>=? fun head ->
    if head.P.id = 0 then (
      (* Management record *)
      match head.P.ty with
      | P.Get_values ->
         reply_to_get_values t config head >>=? fun () ->
         handle_connection_loop t config f
      | _ -> send_unknown_type t head.P.ty
    )
    else (
      match head.P.ty with
      | P.Begin_request ->
         handle_begin_request t head >>=? fun () ->
         handle_connection_loop t config f
      | P.Abort_Request ->
         
         handle_connection_loop t config f
      | P.Params ->
         handle_params t head f >>=? fun () ->
         handle_connection_loop t config f
      | P.Stdin ->
         
         handle_connection_loop t config f
      | P.Data ->
         
         handle_connection_loop t config f
      | P.End_Request | P.Stdout | P.Stderr | P.Get_values
        | P.Get_values_result | P.Unknown ->
         (* As a client, we should not receive those request types. *)
         (* Read and discard data of the record: *)
         P.read_into t.ic head t.buf >>=? fun () ->
         send_unknown_type t head.P.ty >>=? fun () ->
         handle_connection_loop t config f
    )

  let handle_connection config f ic oc =
    let ic = P.make_input ic in
    let buf = P.create_record() in
    let t = { ic; oc; buf;  n_reqs = 0;  close_conn = false;
              req = ID.empty() } in
    handle_connection_loop t config f >>= function
    | Ok _ -> assert false (* should not exit normally *)
    | Error(`EOF | `Write_error) ->
       (* The server closed the connection.  Nothing much to do but give up. *)
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
      buf: Bytes.t;  (* buffer for the main handling *)
      keep_conn: bool;
    }

  let handle_connection ?(keep_conn=false) ic oc =
    let ic = P.make_input ic in
    let buf = P.create_record() in
    { ic; oc; buf; keep_conn }

  let send_begin_request t ~id =
    P.set_id t.buf id;
    P.set_type t.buf P.Begin_request;
    BE.set_int16 t.buf 8 1; (* Responder role; FIXME: support other roles? *)
    Bytes.set t.buf 10 (if t.keep_conn then '\001' else '\000');
    P.write_from t.oc t.buf ~content_length:8

  let send_abort_request t ~id =
    P.set_id t.buf id;
    P.set_type t.buf P.Abort_Request;
    P.write_from t.oc t.buf ~content_length:0

  let get_values t names =
    IO.return []

  let close ?(force=false) t =
    IO.return(Ok())

end
