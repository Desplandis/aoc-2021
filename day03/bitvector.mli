type t

val length: t -> int

val set: t -> int -> bool -> t
val get: t -> int -> bool

val init: int -> (int -> bool) -> t

val not: t -> t

val to_int: t -> int

(* Fold the bitvector from the highest bit to the lowest bit *)
val foldi: (int -> 'a -> bool -> 'a) -> 'a -> t -> 'a

val pp: Format.formatter -> t -> unit
