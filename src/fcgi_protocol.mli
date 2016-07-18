(* File: fcgi_protocol.mli

   Copyright (C) 2016-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/
 *)

(** Generic FastCGI protocol library.

    A pure OCaml library that implements both server and client for
    the FastCGI protocol, abstracting over how input/output is
    performed. *)

open Result

(** Input/Output monad. *)
module type IO = sig
  type +'a t
  (** ['a t] represents a blocking state monad. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** The bind operator.  [m >>= f] passes the result of [m] to the
      function [f] and return the outcome of [f]. *)

  val return : 'a -> 'a t
  (** [return a] constructs a constant IO value.  *)

  type ic
  (** Represents an input channel.  It is assumed that it is buffered
      so, say, reading small amounts of bytes is not overly costly. *)

  type oc
  (** Represents an output channel. *)

  val read_into : ic -> Bytes.t -> int -> int -> int t
  (** [read_into ic buffer offset length] reads up to [length] bytes,
      stores them in buffer at position [offset], and returns the number
      of bytes read.  Note: [read_into] should not raise a (monadic
      or not) exception when reaching the end of file, it should
      returns a length of 0 instead. *)

  val write_from : oc -> Bytes.t -> int -> int -> int t
  (** [write_from oc buffer offset length] writes up to [length] bytes
      to [oc], from [buffer] at position [offset] and returns the
      number of bytes actually written.  *)

  val flush : oc -> unit t
  (** [flush oc] will return when all previously buffered content from
      calling {!write_from} have been written to the output channel [oc]. *)
end

(** Interface defining how to exchange records (the building blocks of
    the FastCGI protocol) between the server and the client. *)
module type RecordIO = sig
  (** The IO monad used to send/receive FastCGI records. *)
  module IO : IO

  (** FastCGI record types. *)
  type ty = | Management | Begin_request | Abort_Request | End_Request
            | Params | Stdin | Stdout | Stderr | Data
            | Get_values | Get_values_result | Unknown

  val ( >>=? ) :
    ('a, 'b) result IO.t -> ('a -> ('c, 'b) result IO.t) -> ('c, 'b) result IO.t
  (** [>>=?] is similar to [>>=] except that the errors are passed along. *)


  type ic
  (** Represent an input channel with resources into which FastCGI
      records can be read. *)

  val make_input : IO.ic -> ic
  (** [make_input ic] allocates the resources to read FastCGI records
      on [ic]. *)

  val read : ic -> (unit, [`EOF]) result IO.t
  (** [read ic] gets the next record on input, overwriting the current
      record held in [ic].  No more bytes than those making the
      FastCGI record are read from the underlying [IO.ic] channel.
      That means that several [ic] may be created *)

  val input_type : ic -> ty
  (** Type of the last read FastCGI record. *)

  val id : ic -> int
  (** ID of the last read FastCGI record. *)

  val data : ic -> Bytes.t
  (** [data ic] return the byte sequence that contains the data of the
      last read FastCGI record.  While the returned value may be
      longer, only the bytes at indices 0 .. [length ic] are valid.  *)

  val length : ic -> int
  (** {i Data} length of the last read FastCGI record. *)


  type oc
  (** Represent an output channel on which FastCGI records can be sent. *)

  val make_output : IO.oc -> oc
  (** [make_output oc] allocates the resources to send FastCGI records
      to [oc]. *)

  val set_type : oc -> ty -> unit
  val set_id : oc -> int -> unit

  val send : oc -> (unit, [`Write_error]) result IO.t
  (** [send oc] sends the FastCGI record held in [oc]. *)
end

module Make_RecordIO(IO: IO) : RecordIO with module IO = IO


module type CLIENT = sig
  module IO : IO

  val handle_connection :
    ?max_reqs: int ->
    (Cohttp.Request.t -> Cohttp_lwt_body.t ->
     (Cohttp.Response.t * Cohttp_lwt_body.t) IO.t) ->
    IO.ic -> IO.oc -> unit IO.t
  (** [handle_connection f ic oc] given an accept()ed connection in
      the form of two channels [ic] and [oc], handle incoming FastCGI
      requests. *)
  ;;
end

(** Build a FastCGI client (i.e., a FastCGI application). *)
module Client(P: RecordIO) : CLIENT with module IO = P.IO


module type SERVER = sig
  module IO : IO

  type t
  (** Represents a connection to a FastCGI client (possibly
      multiplexing requests). *)

  val handle_connection : ?keep_conn: bool -> IO.ic -> IO.oc -> t
  (** [make_connection ic oc] create the resources to speak to a
      FastCGI application through the channels [ic] and [oc].

      @param keep_conn *)

  val get_values : t -> string list -> (string * string) list IO.t
  (** [get_values t names] send a request the the FastCGI client to
      get the values of the variables in [names].  Note that the
      standard names "FCGI_MAX_CONNS", "FCGI_MAX_REQS" and
      "FCGI_MPXS_CONNS" are always included—so [get_values t []] will
      return the values for these three names if the client understand
      them.  *)


  val close : ?force:bool -> t -> (unit, exn) result IO.t
  ;;
end

(** Build a FastCGI server. *)
module Server(P: RecordIO) : SERVER with module IO = P.IO
