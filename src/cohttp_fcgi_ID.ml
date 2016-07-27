(* File: cohttp_fcgi_ID.ml

   Copyright (C) 2016-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/
 *)

(* FIXME: an implementation fast for small IDs would be good.  *)
type 'a map = (int, 'a) Hashtbl.t

let empty () = Hashtbl.create 16

let exists t id = Hashtbl.mem t id

let add t id v = Hashtbl.add t id v

let find_exn (t: _ map) id = Hashtbl.find t id
