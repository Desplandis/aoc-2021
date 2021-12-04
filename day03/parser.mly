%{

  let syntax_error spos cpos =
    let open Lexing in
    Format.eprintf "Line %d, characters %d-%d: Syntax Error\n"
      spos.pos_lnum (spos.pos_cnum - spos.pos_bol)
      (cpos.pos_cnum - cpos.pos_bol);
    exit(1)
%}


%token<Bitvector_bool.t> NUMBER
%token EOF

%start<Bitvector_bool.t list> parse


%%

parse: nbs = number* EOF {
  nbs
}
| error {
  (* Can't reach *)
  syntax_error $startpos $endpos
}

number: nb = NUMBER { nb }
