{
  open Parser

  let lex_error lexbuf =
    let open Lexing in
    let spos = lexeme_start_p lexbuf in
    let cpos = lexeme_end_p lexbuf in
    Format.eprintf "Line %d, characters %d-%d: Invalid token '%s'\n"
      spos.pos_lnum (spos.pos_cnum - spos.pos_bol)
      (cpos.pos_cnum - cpos.pos_bol) (lexeme lexbuf);
    exit(1)

  let new_line lexbuf =
    let open Lexing in
    let cpos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { cpos with
      pos_lnum = cpos.pos_lnum + 1;
      pos_bol = cpos.pos_cnum
    }

  let bitvector_of_string s =
    let len = String.length s in
    Bitvector_bool.init len (fun i -> s.[len-1-i] <> '0')
}


let binary = ['0' '1']
let whitespace = [' ' '\t']
let newline = ('\n' | '\r' | "\r\n")

rule token = parse
  | newline         { new_line lexbuf; token lexbuf }
  | whitespace+     { token lexbuf }
  | binary+ as b    { NUMBER (bitvector_of_string b) }
  | _               { lex_error lexbuf }
  | eof             { EOF }
