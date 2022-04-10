(* Note: You may introduce new code anywhere in this file. *)

type object_phrase = string list

type command =
  | List of object_phrase
  | Read of object_phrase
  | Private
  | Public
  | Secret
  | Quit
  | Draw

exception Empty

exception Malformed

let parse_double con ls =
    let rec get_phrase c l =
    (match l with
    | [] -> raise Malformed
    | h::t ->
        if h = c then t
        else get_phrase c t) in
    let rev = List.rev ls in
    (List.rev (get_phrase con rev), get_phrase con ls)

let parse str =
    let words = String.split_on_char ' ' str in
    let is_invalid x = String.contains x '\n' || x = "" in
    let words_filtered = List.filter (fun x -> not (is_invalid x)) words in
    match words_filtered with
    | "list"::t -> List t
    | "read"::t -> Read t
    | "private"::t -> Private
    | "public"::t -> Public
    | "secret"::t -> Secret
    | ["draw"] -> Draw
    | ["quit"] -> Quit
    | [""] -> raise Empty
    | _ -> raise Malformed
