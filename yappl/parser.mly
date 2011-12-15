%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK 
%token COMMA COLON CONCAT ATTACH COND TILDE
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT AND OR 
%token EQSYM NEQ LT LEQ GT GEQ MEMOEQ
%token IF ELSE INT FLOAT BOOL FUN USCORE COND_VAR IN
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
  expr          { $1 }

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
  | FUN func_bind IN expr { FuncBind($2, $4) }
/*  | TILDE ID expr_seq_opt cond_opt { Eval($2, $3, $4) }
  | IF LPAREN expr RPAREN expr %prec NOELSE { If($3, $5, Noexpr) }
  | IF LPAREN expr RPAREN expr ELSE expr    { If($3, $5, $7) }
  | RBRACK expr_list_opt LBRACK 
  | ID EQSYM expr   { ValBind($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
*/

//func_bind: func_bind { $1 }

/* Function binding */

  /* Question: Why is func_bind a list? */

func_bind:    
  function_dec assn_op expr 
     { [{ fdecl = $1;
	 op = $2;
	 body = $3}] }

function_dec:
  t COLON name args
    { { freturn = $1;
        fname = $3;
        fargs = List.rev $4} }

assn_op: assn_op { $1 }

t: 
   t { $1 }

name: 
   ID { $1 }

args:
  /* nothing */ {[]}
  | decl args { $1 :: $2 }

decl: 
  t COLON name 
    { { dtype = $1;
      dname = $3 } }

/*val_decl: val_decl { $1 }

formals_list:
               { Noexpr }
  | val_decl formals_list   { $1 :: $2 }

expr_list:
               { Noexpr }
  | expr expr_list          { $1 :: $2 }
*/
