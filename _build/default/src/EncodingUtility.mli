(** A collection of functions that helps translate strings and stuff
	to objects that can be operated on *)

val encode_string : string -> Z.t 

val decode_string : Z.t -> string

val str_to_bytes : string -> int list 

val bytes_to_str : int list -> string 

val pack : int -> Z.t list -> Z.t 

val unpack : int -> int -> Z.t -> Z.t list