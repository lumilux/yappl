%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK 
%token COMMA COLON CONCAT ATTACH COND TILDE
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT AND OR IN LET
%token EQSYM NEQ LT LEQ GT GEQ MEMOEQ
%token IF ELSE INT FLOAT BOOL FUN USCORE COND_VAR
%token <bool> BOOL_LITERAL
%token <float> FLOAT_LITERAL
%token <int> INT_LITERAL
%token <string> ID
%token FUN_LITERAL
%token EOF

%nonassoc NOCOND
%nonassoc COND
%nonassoc NOELSE
%nonassoc ELSE LET
%right COLON EQSYM 
%right NOT AND 
%left EQ NEQ IN
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left CONCAT ATTACH
%left SEMI 

%start program
%type <Ast.program> program

%%

program:
   /* nothing  { None }*/
  expr          { $1 }

expr:
    BOOL_LITERAL     { BoolLiteral($1) }
  | INT_LITERAL      { IntLiteral($1) }
  | FLOAT_LITERAL    { FloatLiteral($1) }
  | LPAREN expr RPAREN { $2 }
  | ID               { Id($1) }
  | NOT expr         { Unop(Not, $2) }
  | MINUS expr         { Unop(Neg, $2) }
  | expr SEMI expr   { ExprSeq($1, $3) }
  | expr PLUS   expr { Binop($1, Add,    $3) }
  | expr MINUS  expr { Binop($1, Sub,    $3) }
  | expr TIMES  expr { Binop($1, Mult,   $3) }
  | expr DIVIDE expr { Binop($1, Div,    $3) }
  | expr EQSYM  expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,    $3) }
  | expr LT     expr { Binop($1, Less,   $3) }
  | expr LEQ    expr { Binop($1, Leq,    $3) }
  | expr GT     expr { Binop($1, Greater,$3) }
  | expr GEQ    expr { Binop($1, Geq,    $3) }
  | expr CONCAT expr { Binop($1, ListConcat, $3) }
  | expr ATTACH expr { Binop($1, ListBuild, $3) }
  /*| FUN func_bind expr  { FuncBind($2, $3) }
  | TILDE ID expr_seq_opt cond_opt { Eval($2, $3, $4) }
  | IF LPAREN expr RPAREN expr %prec NOELSE { If($3, $5, Noexpr) }
  | IF LPAREN expr RPAREN expr ELSE expr    { If($3, $5, $7) } */
  | LBRACK expr_list_opt RBRACK { ListBuilder($2) }  
  | val_bind_list IN expr {ValBind($1,$3) } 
  /*| ID EQSYM val_bind_list_opt   { ValBind($1, $3) } 
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
*/

//func_bind: func_bind { $1 }

/*func_binding:
   FUN_LITERAL val_decl LPAREN formals_list RPAREN EQ expr_list 
     { { vdecl = $2;
	 formals = $4;
	 body = List.rev $7 } }

formals_list:
               { Noexpr }
  | val_decl formals_list   { $1 :: $2 }
*/

val_decl: decl  { $1 }

decl: dtype COLON dname { {dtype = $1; dname = $3} }

dtype: dtype {$1}

dname: dname {$1} 

expr_list_opt:
    /* nothing */ { [] }
  |expr_list          {$1}

expr_list:
    expr                          {[$1]}
  | expr_list COMMA expr          { $3 :: $1 }

val_bind_list:
  LET val_bind {[$2]}
  | val_bind_list AND val_bind { $3 :: $1 } 

val_bind: 
  | LET val_decl EQSYM expr {{vdecl = $2; vexpr = $4}}
