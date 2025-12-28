open! Core
open! Import

module Context = struct
  type t =
    | Real of
        { sock : Unix.file_descr
        ; buffer_for_recv : Bytes.t
        }

  let real ?(buffer_for_recv_size = 1024) ?listen_address ~port () =
    let sock = Unix.socket PF_INET SOCK_DGRAM 0 in
    let listen_address =
      Option.value_map
        listen_address
        ~default:Unix.inet_addr_any
        ~f:Unix.inet_addr_of_string
    in
    Unix.bind sock (ADDR_INET (listen_address, port));
    let buffer_for_recv = Bytes.create buffer_for_recv_size in
    Real { sock; buffer_for_recv }
  ;;

  let cleanup = function
    | Real { sock; buffer_for_recv = _ } -> Unix.close sock
  ;;
end

module T = struct
  type 'a t = { get : Context.t -> 'a }

  let return value = { get = Fn.const value }

  let bind { get } ~f =
    { get =
        (fun context ->
          let v = get context in
          let { get } = f v in
          get context)
    }
  ;;

  let map = `Define_using_bind
end

include T
include Monad.Make (T)

module Let_syntax = struct
  include Let_syntax

  module Let_syntax = struct
    include Let_syntax
  end
end

open Let_syntax

let eval_real ?buffer_for_recv_size ?listen_address ~port { get } =
  let context = Context.real ?buffer_for_recv_size ?listen_address ~port () in
  try
    let result = get context in
    Context.cleanup context;
    Ok result
  with
  | exn ->
    Context.cleanup context;
    Or_error.of_exn exn
;;

module Expert = struct
  let thunk f = { get = (fun (_ : Context.t) -> f ()) }
end

module Time = struct
  let now =
    { get =
        (function
          | Real { sock = _; buffer_for_recv = _ } -> Time_ns.now ())
    }
  ;;

  let wait span =
    { get =
        (function
          | Real { sock = _; buffer_for_recv = _ } ->
            Unix.sleepf (Time_ns.Span.to_sec span))
    }
  ;;
end

module Io = struct
  let print input ~f =
    { get =
        (function
          | Real { sock = _; buffer_for_recv = _ } -> f input)
    }
  ;;

  let print_endline string = print string ~f:print_endline
  let print_string string = print string ~f:print_string
  let print_s message = print message ~f:print_s
end

module Udp = struct
  let send_raw message ~pos ~len ~sockaddr =
    { get =
        (function
          | Real { sock; buffer_for_recv = _ } ->
            Unix.sendto sock message pos len [] sockaddr)
    }
  ;;

  let send message_str ~sockaddr =
    let message = Bytes.of_string message_str in
    send_raw message ~pos:0 ~len:(String.length message_str) ~sockaddr
  ;;

  let recv_raw =
    { get =
        (fun (context : Context.t) ->
          match context with
          | Real { sock; buffer_for_recv } ->
            let message_length =
              Unix.recv sock buffer_for_recv 0 (Bytes.length buffer_for_recv) []
            in
            Bytes.sub buffer_for_recv ~pos:0 ~len:message_length)
    }
  ;;

  let recv =
    let%map message_raw = recv_raw in
    Bytes.to_string message_raw
  ;;
end
