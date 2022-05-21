(** A collection of functions that helps translate strings and stuff
	to objects that can be operated on *)

val encode_string : string -> Z.t
(** [encode_string s] encodes [s] into a number *)

val decode_string : Z.t -> string
(** [decode_string n] decodes [n] into a string *)

val str_to_bytes : string -> int list
(** [str_to_bytes s] is the byte represenation of [s] *)

val bytes_to_str : int list -> string
(** [bytes_to_str b] is the string represenation of [b] *)

val pack : int -> Z.t list -> Z.t
(** [pack w] packs the list with width [w] *)

val unpack : int -> int -> Z.t -> Z.t list
(** [unpack w l d] is the list result of unpacking [d] with
	width [w] and len [l] *)