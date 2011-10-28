{ open Parser }

let digit = ['0'-'9']
let exp = 'e' ['-' '+']? digit+
let opt1 = digit+ '.' digit* exp?
let opt2 = digit+ exp
let opt3 = '.' digit+ exp?

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "###"    { comment lexbuf }           (* Comments *)
| "#"      { line_comment lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { EQSYM }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "if"     { IF }
| "else"   { ELSE }
| "fun"    { FUN }
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "true"   { BOOL_LITERAL(true) }
| "false"  { BOOL_LITERAL(false) }
| (opt1 | opt2 | opt3) as lxm { FLOAT_LITERAL(float_of_string lxm) }
| digit+ as lxm { INT_LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "###" { token lexbuf }
| _     { comment lexbuf }

and line_comment = parse
  ['\n' '\r'] { token lexbuf }
| _           { line_comment lexbuf }
