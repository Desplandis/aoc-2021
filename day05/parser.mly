%{

  let syntax_error spos cpos =
    let open Lexing in
    Format.eprintf "Line %d, characters %d-%d: Syntax Error\n"
      spos.pos_lnum (spos.pos_cnum - spos.pos_bol)
      (cpos.pos_cnum - cpos.pos_bol);
    exit(1)
%}


%token<int> INT
%token COMMA
%token ARROW
%token EOF

%start<Types.Line.t list> parse


%%

parse: ls=list(line) EOF { ls }
| error {
  (* Can't reach *)
  syntax_error $startpos $endpos
}

line: pstart=pos ARROW pend=pos { (pstart, pend) }

pos: x=INT COMMA y=INT { (x, y) }
