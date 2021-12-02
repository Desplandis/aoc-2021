{
open Lexing
open Parser

exception SyntaxError of string * position * position

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_bol = pos.pos_cnum;
    pos_lnum = pos.pos_lnum + 1
  }
}

let digit = ['0'-'9']
let uint = (digit | ['1'-'9'] digit*)
let whitespace = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"

rule token = parse
  | whitespace { token lexbuf }
  | newline { next_line lexbuf; token lexbuf }
  | uint as i { UINT (int_of_string i) }
  | "forward" { FORWARD }
  | "down" { DOWN }
  | "up" { UP }
  | _ {
    let start = Lexing.lexeme_start_p lexbuf in
    let curr = Lexing.lexeme_end_p lexbuf in
    raise (SyntaxError (Lexing.lexeme lexbuf, start, curr))
  }
  | eof { EOF }
