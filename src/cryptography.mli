(** A collection of functions to generate shared keys + encrypt/decrypt messages *)

val generate_secret: Elliptic_curve.field -> int  -> Elliptic_curve.point -> Elliptic_curve.point
(** [generate_secret f n p] is the point resultant of multiplying [p] with itself [n] times over field [f]/ *)