%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK 
%token COMMA COLON CONCAT ATTACH
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT
%token EQ NEQ LT LEQ GT GEQ
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
%nonassoc ELSE 
%right EQSYM
%right NOT
%left EQ NEQ
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
 | expr          { $1 }

expr:
    BOOL_LITERAL     { BoolLiteral($1) }
  | INT_LITERAL      { IntLiteral($1) }
  | FLOAT_LITERAL    { FloatLiteral($1) }
  | LPAREN expr RPAREN { $2 }
  | ID               { Id($1) }
  | NOT expr         { Unop(Not, $2) }
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
/*  | FUN func_bind expr  { FuncBind($2, $3) }
  | TILDE ID expr_seq_opt cond_opt { Eval($2, $3, $4) }
  | IF LPAREN expr RPAREN expr %prec NOELSE { If($3, $5, Noexpr) }
  | IF LPAREN expr RPAREN expr ELSE expr    { If($3, $5, $7) }
  | RBRACK expr_list_opt LBRACK 
  | ID EQSYM expr   { ValBind($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
*/

//func_bind: func_bind { $1 }

/*func_binding:
   FUN_LITERAL val_decl LPAREN formals_list RPAREN EQ expr_list 
     { { vdecl = $2;
	 formals = $4;
	 body = List.rev $7 } }

val_decl: val_decl { $1 }

formals_list:
               { Noexpr }
  | val_decl formals_list   { $1 :: $2 }

expr_list:
               { Noexpr }
  | expr expr_list          { $1 :: $2 }
*/
