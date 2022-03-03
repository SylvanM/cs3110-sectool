(** A collection of functions helping with file I/O *)

open Elliptic_curve

exception FileDoesNotExist of string

let read_private_key (f : string) : Z.t =
  raise (Failure "Unimplemented: read_private_key")

let read_public_key (f : string) : Elliptic_curve.point =
  raise (Failure "Unimplemented: read_public_key")

let read_domain_params (f : string) : Elliptic_curve.field =
  raise (Failure "Unimplemented: read_domain_params")

let write_private_key (d : Z.t) (f : string) =
  raise (Failure "Unimplemented: write_private_key")

let write_public_key (p : point) (f : string) =
  raise (Failure "Unimplemented: write_public_key")

let write_domain_params (dom : field) (f : string)  =
  raise (Failure "Unimplemented: write_domain_params")