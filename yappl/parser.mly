%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK 
%token COMMA COLON CONCAT ATTACH COND TILDE
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT AND OR IN LET
%token EQSYM NEQ LT LEQ GT GEQ MEMOEQ
%token IF ELSE INT FLOAT BOOL FUN USCORE COND_VAR IN
%token <bool> BOOL_LITERAL
%token <float> FLOAT_LITERAL
%token <int> INT_LITERAL
%token <string> ID
%token FUN_LITERAL
%token EOF

%right COLON EQSYM 
%right NOT SEMI
%left EQ NEQ IN
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left CONCAT ATTACH
%left  AND
%left LBRACK RBRACK
%nonassoc NOCOND
%nonassoc COND
%nonassoc NOELSE
%nonassoc ELSE LET
%nonassoc TILDE
%nonassoc ID

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
  | expr EQSYM  expr { Binop($1, Equal,  $3) }
  | expr NEQ    expr { Binop($1, Neq,    $3) }
  | expr LT     expr { Binop($1, Less,   $3) }
  | expr LEQ    expr { Binop($1, Leq,    $3) }
  | expr GT     expr { Binop($1, Greater,$3) }
  | expr GEQ    expr { Binop($1, Geq,    $3) }
  | expr CONCAT expr { Binop($1, ListConcat, $3) }
  | expr ATTACH expr { Binop($1, ListBuild, $3) }
  | func_bind IN expr { FuncBind($1, $3) }
  | TILDE ID expr_seq_opt cond_opt { Eval($2, $3, $4) }
/*  | IF LPAREN expr RPAREN expr %prec NOELSE { If($3, $5, Noexpr) }
  | IF LPAREN expr RPAREN expr ELSE expr    { If($3, $5, $7) } */
  | LBRACK expr_list_opt RBRACK { ListBuilder($2) }  
  | LET val_bind_list IN expr {ValBind($2,$4) } 

//func_bind: func_bind { $1 }

/* Function binding */

  /* Question: Why is func_bind a list? */

func_bind:    
  function_decl assn_op expr 
      { [{ fdecl = $1;
	 op = $2;
	 body = $3}] }

function_decl:
  FUN t COLON ID args
    { { freturn = ValType $2;
        fname = $4;
        fargs = List.rev $5} }

assn_op: 
    EQSYM { Assign } 
  | MEMOEQ { MemoAssign }

t: 
    INT { Int }
  | FLOAT { Float }
  | BOOL { Bool }
  | t LBRACK RBRACK { List $1 }

args:
  /* nothing */ {[]}
  | args decl { $2 :: $1 }

decl: 
  t COLON ID 
    { { dtype = ValType $1;
      dname = $3 } }

/* Function evaluation */

expr_seq_opt:
  /* nothing */ { [] }
  | expr_seq    { List.rev $1 }

expr_seq:
  expr          { [$1] }
  | expr_seq expr   { $2 :: $1 }

cond_opt:
  /* nothing */ { Noexpr }
  | COND expr { $2 }

      /*val_decl: val_decl { $1 }

formals_list:
               { Noexpr }
  | val_decl formals_list   { $1 :: $2 }
*/

val_decl: decl  { $1 }

expr_list_opt:
    /* nothing */ { [] }
  |expr_list          {$1}

expr_list:
    expr                          {[$1]}
  | expr_list COMMA expr          { $3 :: $1 }

val_bind_list:
   val_bind {[$1]}
  | val_bind_list AND val_bind { $3 :: $1 } 

val_bind: 
  | val_decl EQSYM expr {{vdecl = $1; vexpr = $3}}
