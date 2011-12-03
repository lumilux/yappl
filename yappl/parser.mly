%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN
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
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { None }
 | expr          { $1 }

expr:
    BOOL_LITERAL     { BoolLit($1) }
  | INT_LITERAL      { IntLit($1) }
  | FLOAT_LITERAL    { FloatLit($1) }
  | LPAREN expr RPAREN { $2 }
  | ID               { Id($1) }
  | expr SEMI expr   { Sequence($1, $3) }
  | expr PLUS   expr { Binop($1, Add,    $3) }
  | expr MINUS  expr { Binop($1, Sub,    $3) }
  | expr TIMES  expr { Binop($1, Mult,   $3) }
  | expr DIVIDE expr { Binop($1, Div,    $3) }
  | expr %right EQSYM  expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,    $3) }
  | expr LT     expr { Binop($1, Less,   $3) }
  | expr LEQ    expr { Binop($1, Leq,    $3) }
  | expr GT     expr { Binop($1, Greater,$3) }
  | expr GEQ    expr { Binop($1, Geq,    $3) }
  | expr CONCAT expr { Binop($1, Concat, $3) }
  | expr ATTACH expr { Binop($1, Attach, $3) }
  | TILDE ID expr_seq_opt cond_opt { Eval($2, $3, $4) }
  | IF LPAREN expr RPAREN expr %prec NOELSE { If($3, $5, Noexpr) }
  | IF LPAREN expr RPAREN expr ELSE expr    { If($3, $5, $7) }
  | RBRACK expr_list_opt LBRACK 

  | ID EQSYM expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }


fdecl:
   FUN_LITERAL type_decl COLON ID LPAREN formals_opt RPAREN EQ stmt_list 
     { { fname = $4;
	 formals = $6;
	 return = $2;
	 body = List.rev $9 } }
 

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr_seq_opt:
    expr expr_seq_opt { $1 :: $2 }
  | expr_opt          { $1 }

cond_opt:
    /* nothing */ { Noexpr }
  | COND expr     { Cond($2) }

expr_list_opt:
    /* nothing */ { [] }
  | expr_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | expr_list COMMA expr { $3 :: $1 }
