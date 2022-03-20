(** A collection of functions helping with file I/O *)

open Elliptic_curve
open Stdio

let dir_prefix = "data" ^ Filename.dir_sep

exception FileDoesNotExist of string
exception Malformed of string

(** HELPERS *)

let rec to_int (l : string list) : int list =
  match l with
  | h::t -> (int_of_string h)::(to_int t)
  | [] -> []

(** *)

let read_private_key (f : string) : int =
  let list = Stdio.In_channel.read_lines (dir_prefix ^ f) in
    match list with
    | exception e -> raise (FileDoesNotExist (f ^ " does not exist"))
    | h::t -> int_of_string h
    | _ -> raise (Malformed "Incorrectly Formatted")

let read_public_key (f : string) : Elliptic_curve.point =
  let list = Stdio.In_channel.read_lines (dir_prefix ^ f) in
    match list with
    | exception e -> raise (FileDoesNotExist (f ^ " does not exist"))
    | [] -> raise (Malformed "Empty File")
    | h::t ->
      let str_list = String.split_on_char ' ' h in
        let int_list = to_int str_list in
          match int_list with
          | x::y::t -> { x=x; y=y}
          | _::[] | [] -> raise (Malformed "Empty File")

let read_domain_params (f : string) : Elliptic_curve.field =
  let list = Stdio.In_channel.read_lines (dir_prefix ^ f) in
    match list with
    | exception e -> raise (FileDoesNotExist (f ^ " does not exist"))
    | [] -> raise (Malformed "Empty File")
    | h::t ->
      let str_list = String.split_on_char ' ' h in
        let int_list = to_int str_list in
          match int_list with
          | p::a::b::c::d::g::n::h::t -> create_field (p,a,b,c,d,g,n,h)
          | _::[] | [] -> raise (Malformed "Empty File")
          | h::t -> raise (Malformed "Incorrectly Formatted")

let write_private_key (d : int) (f : string) =
  Stdio.Out_channel.write_lines (dir_prefix ^ f) [string_of_int d]

let write_public_key (p : point) (f : string) =
  Stdio.Out_channel.write_lines (dir_prefix ^ f) [string_of_point p]

let write_domain_params (dom : field) (f : string)  =
  Stdio.Out_channel.write_lines (dir_prefix ^ f) [string_of_field dom]