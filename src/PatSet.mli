type t = {
  k: int;
  v: int;
  l: t;
  r: t;
}
(*val empty : t

val is_empty : t -> bool
val is_singleton : t -> bool
val equal : t -> t -> bool
val compare : t -> t -> int

val singleton : int -> t

val add : int -> t -> t
val mem : int -> t -> bool
val remove : int -> t -> t

val union : t -> t -> t
val diff : t -> t -> t

val iter : (int -> unit) -> t -> unit
val rev_iter : (int -> unit) -> t -> unit

val low_level_insert_mask : int -> int -> t -> t*)

include API.SET
  with type elt = int
   and type t := t

val extract_unique_suffix : t -> t -> t * t
val extract_shared_suffix : t -> t -> t * (t * t)

(**[check] is used only during testing. *)
val check : t -> unit
