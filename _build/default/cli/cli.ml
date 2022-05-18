open Printf
open Cmdliner
open Sectool
open Z

(** Record types *)

type private_conf = {
  bitsize: int;
  name: string option;
}

type public_conf = {
  private_input: string option;
  name: string option;
}

type secret_conf = {
  private_input: string option;
  public_input: string option;
  name: string option;
}

type cmd_conf =
  | Private of private_conf
  | Public of public_conf
  | Secret of secret_conf

(** Run *)

let opt_string opt =
  match opt with
  | None -> "none"
  | Some s -> s

let run cmd_conf =
  match cmd_conf with
  | Private conf ->
      let result = Sectool.Ecdh.generate_private_key conf.bitsize in
      let result_string = Z.to_string result in
      let name = opt_string conf.name in
      Sectool.File_wizard.write_private_key result name;
      printf
      "\

Private configuration:
|  bitsize: %i
|  name: %s
|  result: %s

"
      conf.bitsize
      name
      result_string
  | Public conf ->
      let private_key = Sectool.File_wizard.read_private_key (opt_string conf.private_input) in
      let result = Sectool.Ecdh.compute_public_key private_key in
      let result_string = Sectool.ED25519.string_of_point result in
      let name = opt_string conf.name in
      Sectool.File_wizard.write_public_key result name;
      printf
      "\

Public configuration:
|  private input: %s
|  name: %s
|  result: %s

"
      (opt_string conf.private_input)
      name
      result_string
  | Secret conf ->
      let private_key = Sectool.File_wizard.read_private_key (opt_string conf.private_input) in
      let public_key = Sectool.File_wizard.read_public_key (opt_string conf.public_input) in
      let result = Sectool.Ecdh.compute_shared_secret private_key public_key in
      let result_string = Sectool.ED25519.string_of_point result in
      let name = opt_string conf.name in
      Sectool.File_wizard.write_public_key result name;
      printf
      "\

Secret configuration:
|  private input: %s
|  public input: %s
|  name: %s
|  result: %s

 "
      (opt_string conf.private_input)
      (opt_string conf.public_input)
      name
      result_string

(** Terms *)

let filename_term =
  let info =
    Arg.info []
      ~docv:"NAME"
      ~doc:"Example of an optional argument without a default."
  in
  Arg.value (Arg.pos 0 (Arg.some Arg.string) None info)

let private_input_term =
  let info =
    Arg.info ["p";"priv";"private"]
      ~docv:"FILE"
      ~doc:"Example of an anonymous argument at a fixed position."
  in
  Arg.value (Arg.opt (Arg.some Arg.file) None info)

let public_input_term =
  let info =
    Arg.info ["P";"pub";"public"]
      ~docv:"FILE"
      ~doc:"Example of an anonymous argument at a fixed position."
  in
  Arg.value (Arg.opt (Arg.some Arg.file) None info)

let bitsize_term =
  let default = 1 in
  let info =
    Arg.info ["n"; "bitsize"]
      ~docv:"NUM"
      ~doc:"Example of an optional argument with a default.
            The value of \\$\\(docv\\) is $(docv)."
  in
  Arg.value (Arg.opt Arg.int default info)

(** PRIVATE *)

let private_term run =
  let combine name bitsize =
    let conf = Private {
      bitsize;
      name
    } in
    run conf
  in
  Term.(const combine
        $ filename_term
        $ bitsize_term
       )

let private_doc = "[some headline for private]"

let private_man = [
  `S Manpage.s_description;
  `P "[multiline overview of private]";
]

let private_cmd run =
  let info =
    Cmd.info "private"
      ~doc:private_doc
      ~man:private_man
  in
  Cmd.v info (private_term run)

(** PUBLIC *)

let public_term run =
  let combine name private_input =
    let conf = Public {
      private_input;
      name
    } in
    run conf
  in
  Term.(const combine
        $ filename_term
        $ private_input_term
       )

let public_doc = "[some headline for public]"

let public_man = [
  `S Manpage.s_description;
  `P "[multiline overview of public]";
]

let public_cmd run =
  let info =
    Cmd.info "public"
      ~doc:public_doc
      ~man:public_man
  in
  Cmd.v info (public_term run)

(** SECRET *)

let secret_term run =
  let combine name private_input public_input =
    let conf = Secret {
      private_input;
      public_input;
      name
    } in
    run conf
  in
  Term.(const combine
        $ filename_term
        $ private_input_term
        $ public_input_term
       )

let secret_doc = "[some headline for secret]"

let secret_man = [
  `S Manpage.s_description;
  `P "[multiline overview of secret]";
]

let secret_cmd run =
  let info =
    Cmd.info "secret"
      ~doc:secret_doc
      ~man:secret_man
  in
  Cmd.v info (secret_term run)

(** Root *)

let root_doc = "[some headline for the main command]"

let root_man = [
  `S Manpage.s_description;
  `P "[multiline overview of the main command]";
]

let root_term =
  Term.ret (Term.const (`Help (`Pager, None)))

let root_info =
  Cmd.info "sectool-cli"
    ~doc:root_doc
    ~man:root_man

let subcommands run = [
  private_cmd run;
  public_cmd run;
  secret_cmd run;
]

(** Parse *)

let parse_command_line_and_run (run : cmd_conf -> unit) =
  Cmd.group root_info (subcommands run) |> Cmd.eval |> exit

let safe_run conf =
  try run conf
  with
  | Failure msg ->
      eprintf "Error: %s\n%!" msg;
      exit 1
  | e ->
      let trace = Printexc.get_backtrace () in
      eprintf "Error: exception %s\n%s%!"
        (Printexc.to_string e)
        trace

let main () =
  Printexc.record_backtrace true;
  parse_command_line_and_run safe_run

let () = main ()
