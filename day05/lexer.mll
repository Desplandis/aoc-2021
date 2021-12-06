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
}


let digit = ['0'-'9']
let uint = (digit | ['1'-'9'] digit*)
let blank = [' ' '\t']
let newline = ('\n' | '\r' | "\r\n")

rule token = parse
  | newline         { new_line lexbuf; token lexbuf }
  | blank+          { token lexbuf }
  | "->"            { ARROW }
  | ","             { COMMA }
  | uint as i       { INT(int_of_string i) }
  | eof             { EOF }
  | _               { lex_error lexbuf }
