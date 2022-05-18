(** A collection of functions helping with file I/O *)

open ED25519
open Stdio

let dir_prefix = "data" ^ Filename.dir_sep

exception FileDoesNotExist of string
exception Malformed of string

(** HELPERS *)

let strs_to_z (l : string list) : Z.t list =
  List.map Z.of_string l

(** *)

let read_file (f : string) : string list =
  match Stdio.In_channel.read_lines f with
  | exception e -> raise (FileDoesNotExist (f ^ " does not exist"))
  | [] -> raise (Malformed "Empty File")
  | h::t -> String.split_on_char ' ' h

let read_private_key (f : string) : Z.t =
  let list = Stdio.In_channel.read_lines f in
    match list with
    | exception e -> raise (FileDoesNotExist (f ^ " does not exist"))
    | h::t -> Z.of_string h
    | _ -> raise (Malformed "Incorrectly Formatted")

let read_public_key (f : string) : Z.t =
  let str_list = read_file f in
  let int_list = strs_to_z str_list in
    match int_list with
    | x :: t -> x
    | [] -> raise (Malformed "Empty File")

let write_private_key (d : Z.t) (f : string) =
  Stdio.Out_channel.write_lines (dir_prefix ^ f) [d |> Z.to_string]

let write_public_key (p : Z.t) (f : string) =
  Stdio.Out_channel.write_lines (dir_prefix ^ f) [Z.to_string p]
