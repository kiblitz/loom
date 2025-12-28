open! Core
open! Import

module Context : sig
  type t
end

type 'a t

include Monad.S with type 'a t := 'a t

val eval_real
  :  ?buffer_for_recv_size:int
  -> ?listen_address:string
  -> port:int
  -> 'a t
  -> 'a Or_error.t

module Expert : sig
  val thunk : (unit -> 'a) -> 'a t
end

module Io : sig
  val print_endline : string -> unit t
  val print_string : string -> unit t
  val print_s : Sexp.t -> unit t
end

module Udp : sig
  val send_raw : Bytes.t -> pos:int -> len:int -> sockaddr:Unix.sockaddr -> int t
  val send : string -> sockaddr:Unix.sockaddr -> int t
  val recv_raw : Bytes.t t
  val recv : string t
end
