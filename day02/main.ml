open Types

type pos = { x: int; y: int; aim: int }

let pp_instr fmt = function
  | Up i -> Format.fprintf fmt "Up %d" i
  | Down i -> Format.fprintf fmt "Up %d" i
  | Forward i -> Format.fprintf fmt "Forward %d" i

let parse chan =
  match Lexing.from_channel chan |> Parser.instrs Lexer.token
  with instrs -> Some(instrs)
  | exception Lexer.SyntaxError(token, start, cur) ->
    Format.eprintf "Line %d, characters %d-%d: Invalid token '%s'\n"
      start.pos_lnum (start.pos_cnum - start.pos_bol)
      (cur.pos_cnum - cur.pos_bol) token;
    None

let move pos = function
  | Down i -> { pos with y = pos.y + i }
  | Up i -> { pos with y = pos.y - i }
  | Forward i -> { pos with x = pos.x + i }

let update pos = function
  | Down i -> { pos with aim = pos.aim + i }
  | Up i -> { pos with aim = pos.aim - i }
  | Forward i -> { pos with
      x = pos.x + i;
      y = pos.y + (i * pos.aim)
  }

let execute_instrs f start instrs =
  List.fold_left f start instrs |>
  fun pos -> pos.x * pos.y

let () =
  let instrs =
    let chan = open_in "input" in
    let instrs = parse chan in
  (* Format.printf "%a\n"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_instr)
    instrs; *)
    close_in chan; instrs
  in
  let pos = { x = 0; y = 0; aim = 0 } in
  Format.printf "Score 1: %a\n"
    (Format.pp_print_option Format.pp_print_int)
    (Option.map (execute_instrs move pos) instrs);
  Format.printf "Score 2: %a\n"
    (Format.pp_print_option Format.pp_print_int)
    (Option.map (execute_instrs update pos) instrs)
