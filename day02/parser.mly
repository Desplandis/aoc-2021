%token FORWARD
%token DOWN
%token UP
%token<int> UINT
%token EOF

%start<Types.instr list> instrs

%%

instrs : instrs = instr* EOF { instrs }

instr :
  | FORWARD i = UINT { Forward i }
  | DOWN i = UINT { Down i }
  | UP i = UINT { Up i }
