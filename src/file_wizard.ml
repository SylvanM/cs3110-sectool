(** A collection of functions helping with file I/O *)

open Elliptic_curve
open Stdio

let dir_prefix = "data" ^ Filename.dir_sep

exception FileDoesNotExist of string
exception Malformed of string

(** HELPERS *)

let strs_to_z (l : string list) : Z.t list =
  List.map Z.of_string l

(** *)

let read_private_key (f : string) : Z.t =
  let list = Stdio.In_channel.read_lines (dir_prefix ^ f) in
    match list with
    | exception e -> raise (FileDoesNotExist (f ^ " does not exist"))
    | h::t -> Z.of_string h
    | _ -> raise (Malformed "Incorrectly Formatted")

let read_public_key (f : string) : Elliptic_curve.point =
  let list = Stdio.In_channel.read_lines (dir_prefix ^ f) in
    match list with
    | exception e -> raise (FileDoesNotExist (f ^ " does not exist"))
    | [] -> raise (Malformed "Empty File")
    | h::t ->
      let str_list = String.split_on_char ' ' h in
        let int_list = strs_to_z str_list in
          match int_list with
          | x::y::t -> make_point (x, y)
          | _::[] | [] -> raise (Malformed "Empty File")

let read_domain_params (f : string) : Elliptic_curve.field =
  let list = Stdio.In_channel.read_lines (dir_prefix ^ f) in
    match list with
    | exception e -> raise (FileDoesNotExist (f ^ " does not exist"))
    | [] -> raise (Malformed "Empty File")
    | h::t ->
      let str_list = String.split_on_char ' ' h in
        let int_list = strs_to_z str_list in create_field int_list

let write_private_key (d : Z.t) (f : string) =
  Stdio.Out_channel.write_lines (dir_prefix ^ f) [d |> Z.to_string]

let write_public_key (p : point) (f : string) =
  Stdio.Out_channel.write_lines (dir_prefix ^ f) [string_of_point p]

let write_domain_params (dom : field) (f : string)  =
  Stdio.Out_channel.write_lines (dir_prefix ^ f) [string_of_field dom]