(* File: cohttp_fcgi_lwt.mli

   Copyright (C) 2016-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/
 *)

(* Utilities *)

let rev_split is_cut s =
  let rec seek_cut acc i0 i1 =
    if i1 >= String.length s then
      String.sub s i0 (i1 - i0) :: acc
    else if is_cut(String.unsafe_get s i1) then
      skip (String.sub s i0 (i1 - i0) :: acc) (i1 + 1) (i1 + 1)
    else
      seek_cut acc i0 (i1 + 1)
  and skip acc i0 i1 =
    if i1 >= String.length s then acc
    else if is_cut(String.unsafe_get s i1) then skip acc i0 (i1 + 1)
    else seek_cut acc i1 (i1 + 1) in
  skip [] 0 0


(** The IO module is specialized for the [Lwt] monad. *)
module IO = struct
  type +'a t = 'a Lwt.t
  let ( >>= ) = Lwt.bind
  let return = Lwt.return
  let all = Lwt.join

  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel

  let read_into = Lwt_io.read_into
  let write_from = Lwt_io.write_from
  let flush = Lwt_io.flush

  let create_pipe () = () (* FIXME *)
end

module Record = Fcgi_protocol.Make_RecordIO(IO)


module type REQUEST = sig
  include Cohttp.S.Request

end

module Client = struct
  include Fcgi_protocol.Client(Record)

  let fcgi_web_server_addrs =
    try
      let addrs = rev_split ((=) ',') (Unix.getenv "FCGI_WEB_SERVER_ADDRS") in
      List.map Unix.inet_addr_of_string addrs
    with
      Not_found | Failure _ -> []

  let default_allow = function
    | Unix.ADDR_UNIX _ -> true
    | Unix.ADDR_INET(addr,_) ->
       fcgi_web_server_addrs = [] || List.mem addr fcgi_web_server_addrs


  let rec run_loop config allow sock f =
    let open IO in
    Lwt_unix.accept sock >>= fun (fd, server) ->
    if allow server then (
      (* FIXME: What about exceptions possibly raised? *)
      let ic = Lwt_io.of_fd Lwt_io.Input fd in
      let oc = Lwt_io.of_fd Lwt_io.Output fd in
      all [handle_connection config f ic oc;
           run_loop config allow sock f]
    )
    else
      run_loop config allow sock f


  let run ?(config=default_config) ?(allow=default_allow)
        ?socket ?sockaddr ?port f =
    (* FIXME: Under M$win, the web server communicates with a FCGI
       script that it launches by means of a named pipe [fd]
       (contrarily to the spec).  The requests are all sent through
       that pipe.  Thus there is a single connection. *)
    let sock = match (socket, sockaddr, port) with
      | (Some s, _, _) -> s
      | (None, None, None) ->
         (* FastCGI launched by the web server *)
         Lwt_unix.stdin
      | (None, Some _, _)
        | (None, _, Some _) ->
         let saddr = match (sockaddr, port) with
           | (Some sa, _) -> sa
           | (None, Some p) -> Unix.ADDR_INET(Unix.inet_addr_loopback, p)
           | (None, None) -> assert false in
         (* FastCGI on a distant machine, listen on the given socket. *)
         let sock =
           Lwt_unix.socket (Unix.domain_of_sockaddr saddr) Unix.SOCK_STREAM 0 in
         Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
         Lwt_unix.bind sock saddr;
         Lwt_unix.listen sock config.max_conns;
         sock
    in
    run_loop config allow sock f
end
