%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK 
%token COMMA COLON CONCAT ATTACH COND TILDE
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT AND OR IN LET
%token EQSYM NEQ LT LEQ GT GEQ MEMOEQ
%token IF ELSE THEN INT FLOAT BOOL FUN USCORE COND_VAR IN
%token MATCH WITH ARROW WILDCARD
%token <bool> BOOL_LITERAL
%token <float> FLOAT_LITERAL
%token <int> INT_LITERAL
%token <string> ID
%token FUN_LITERAL
%token EOF

%nonassoc IN
%nonassoc SEMI
%nonassoc LET
%nonassoc MATCH WITH
%nonassoc NOCOND
%nonassoc COND
%nonassoc NOELSE
%nonassoc THEN
%nonassoc ELSE
%right COLON EQSYM 
%left ATTACH
%left CONCAT 
%left AND OR
%left EQ NEQ 
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc NOT
%nonassoc TILDE
%nonassoc ID LBRACK RBRACK 

%start program
%type <Ast.program> program

%%

program:
   /* nothing  { None }*/
  expr          { $1 }

expr:
    expr_core { $1 }
  | binop { $1 }
  | eval { $1 }

expr_no_eval:
    expr_core { $1 }
  | binop_no_eval { $1 }  

eval:
  TILDE ID expr_seq_opt /*cond_opt  */ { Noexpr } /* { Eval($2, $3, $4) }*/

binop_no_eval:
    expr_no_eval SEMI   expr_no_eval { ExprSeq($1, $3) }
  | expr_no_eval PLUS   expr_no_eval { Binop($1, Add,    $3) }
  | expr_no_eval MINUS  expr_no_eval { Binop($1, Sub,    $3) }
  | expr_no_eval TIMES  expr_no_eval { Binop($1, Mult,   $3) }
  | expr_no_eval DIVIDE expr_no_eval { Binop($1, Div,    $3) }
  | expr_no_eval EQSYM  expr_no_eval { Binop($1, Equal,  $3) }
  | expr_no_eval NEQ    expr_no_eval { Binop($1, Neq,    $3) }
  | expr_no_eval LT     expr_no_eval { Binop($1, Less,   $3) }
  | expr_no_eval LEQ    expr_no_eval { Binop($1, Leq,    $3) }
  | expr_no_eval GT     expr_no_eval { Binop($1, Greater,$3) }
  | expr_no_eval GEQ    expr_no_eval { Binop($1, Geq,    $3) }
  | expr_no_eval CONCAT expr_no_eval { Binop($1, ListConcat, $3) }
  | expr_no_eval ATTACH expr_no_eval { Binop($1, ListBuild, $3) }

binop:
  | expr SEMI   expr { ExprSeq($1, $3) }
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

expr_core:
/*  | LPAREN expr RPAREN { $2 } */
  | BOOL_LITERAL     { BoolLiteral($1) }
  | INT_LITERAL      { IntLiteral($1) }
  | FLOAT_LITERAL    { FloatLiteral($1) }
  | LPAREN expr RPAREN { $2 }
  | ID               { Id($1) }
  | NOT expr         { Unop(Not, $2) }
  | MINUS expr       { Unop(Neg, $2) }
  | func_bind IN expr { FuncBind($1, $3) }
/*  | IF LPAREN expr RPAREN THEN expr %prec NOELSE { If($3, $6, Noexpr) }
  | IF LPAREN expr RPAREN THEN expr ELSE expr    { If($3, $6, $8) } */
  | IF  expr  THEN expr { If($2, $4, Noexpr) }  
  | IF  expr THEN expr ELSE expr { If($2, $4, $6) }
/*  | LBRACK expr_list_opt RBRACK { ListBuilder($2) }  
  | LET val_bind_list IN expr {ValBind($2,$4) } 
  | MATCH expr WITH pattern_match  { Match($2, $4) }*/

/* Function binding */

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
    /* nothing */   { [] }
  | expr_seq        { List.rev $1 }

expr_seq:
    expr_no_eval          { [$1] }
  | expr_seq expr_no_eval  { $2 :: $1 }

cond_opt:
  /* nothing */ { Noexpr }
  | COND expr { $2 }


/* Lists */

expr_list_opt:
    /* nothing */ { [] }
  |expr_list          {$1}

expr_list:
    expr                          {[$1]}
  | expr_list COMMA expr          { $3 :: $1 }


/* Value binding */

val_decl: decl  { $1 }

val_bind_list:
   val_bind {[$1]}
  | val_bind_list AND val_bind { $3 :: $1 } 

val_bind: 
  | val_decl EQSYM expr {{vdecl = $1; vexpr = $3}}


/* Pattern matching */

pattern_match:
  pattern ARROW expr pattern_match_cont { Pattern($1, $3, NoPattern) }
  | COND pattern ARROW expr pattern_match_cont { Pattern($2, $4, $5) }

pattern_match_cont:
  /* nothing */ %prec MATCH { NoPattern }
  | COND pattern ARROW expr pattern_match_cont { Pattern($2, $4, $5) }

pattern:
  ID { Ident($1) }
  | WILDCARD { Wildcard }
  | pattern ATTACH pattern { Concat($1, $3) }
