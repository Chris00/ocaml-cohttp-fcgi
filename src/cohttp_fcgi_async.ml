

open Async.Std

(** The IO module is specialized for the [Lwt] monad. *)
module IO = struct
  type +'a t = 'a Deferred.t
  let ( >>= ) = Deferred.bind
  let return = Deferred.return
  let all = Deferred.all_unit

  type ic = Reader.t
  type oc = Writer.t

  let read_into ic buf pos len = Reader.read ic ~pos ~len buf
  let write_from oc buf pos len = Writer.write ~pos ~len oc buf
  let flush = Writer.flushed

  let create_pipe () = () (* FIXME *)
end
