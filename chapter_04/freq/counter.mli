open Base

type t
(** A collection of string frequency counts*)

val empty : t
(** The empty set of frequency counts*)

val touch : t -> string -> t

val to_list : t -> (string * int) list
