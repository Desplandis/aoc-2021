%{
  module IntSet = Set.Make(Int)

  let syntax_error spos cpos =
    let open Lexing in
    Format.eprintf "Line %d, characters %d-%d: Syntax Error\n"
      spos.pos_lnum (spos.pos_cnum - spos.pos_bol)
      (cpos.pos_cnum - cpos.pos_bol);
    exit(1)
%}


%token<int> INT
%token COMMA
%token EOL
%token EOF
%token SEP

%start<int list * (int list list) array> parse


%%

parse: d=draw EOL bs=separated_nonempty_list(EOL, board) EOF {
  (d, Array.of_list bs)
}
| error {
  (* Can't reach *)
  syntax_error $startpos $endpos
}

draw: xs=separated_nonempty_list(COMMA, INT) EOL { xs }

board: rs=nonempty_list(row) { rs }
row: option(SEP) xs=separated_nonempty_list(SEP, INT) EOL { xs }
