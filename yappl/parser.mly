%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK 
%token COMMA COLON CONCAT ATTACH BAR GIVEN TILDE
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token NOT AND OR IN LET BIND_SEP
%token EQSYM NEQ LT LEQ GT GEQ MEMOEQ
%token IF ELSE THEN INT FLOAT BOOL FUN COND_VAR IN
%token MATCH WITH ARROW WILDCARD
%token <bool> BOOL_LITERAL
%token <float> FLOAT_LITERAL
%token <int> INT_LITERAL
%token <string> ID
%token EOF

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc above_SEMI
%nonassoc LET FUN
%nonassoc WITH
%nonassoc BIND_SEP
%nonassoc THEN
%nonassoc ELSE
%nonassoc IF MATCH
%right COLON EQSYM 
%nonassoc below_BAR
%nonassoc BAR
%nonassoc GIVEN
%left ATTACH
%left CONCAT 
%left AND OR
%left EQ NEQ 
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc NOT
%nonassoc TILDE
%nonassoc ARROW
%nonassoc LPAREN RPAREN
%nonassoc ID COND_VAR 
%left LBRACK
%nonassoc RBRACK BOOL_LITERAL FLOAT_LITERAL INT_LITERAL LBRACE COMMA USCORE

%start program
%type <Ast.program> program

%%

program:
   /* nothing  { None }*/
  seq_expr          { $1 }

seq_expr:
| expr        %prec below_SEMI { $1 }
| expr SEMI              { $1 }
| expr SEMI seq_expr     { ExprSeq($1,$3) }

expr:
    expr_core { $1 }
  | binop { $1 }


binop:
/* | expr SEMI   expr { ExprSeq($1, $3) } */
  | expr PLUS   expr { Binop($1, Add,    $3) }
  | expr MINUS  expr { Binop($1, Sub,    $3) }
  | expr TIMES  expr { Binop($1, Mult,   $3) }
  | expr DIVIDE expr { Binop($1, Div,    $3) }
  | expr MOD    expr { Binop($1, Mod,    $3) }
  | expr EQSYM  expr { Binop($1, Equal,  $3) }
  | expr NEQ    expr { Binop($1, Neq,    $3) }
  | expr LT     expr { Binop($1, Less,   $3) }
  | expr LEQ    expr { Binop($1, Leq,    $3) }
  | expr GT     expr { Binop($1, Greater,$3) }
  | expr GEQ    expr { Binop($1, Geq,    $3) }
  | expr CONCAT expr { Binop($1, ListConcat, $3) }
  | expr ATTACH expr { Binop($1, ListBuild, $3) }
  | expr OR     expr { Binop($1, Or, $3) } 
  | expr AND    expr { Binop($1, And, $3) }

expr_core:
  | BOOL_LITERAL     { BoolLiteral($1) }
  | INT_LITERAL      { IntLiteral($1) }
  | FLOAT_LITERAL    { FloatLiteral($1) }
  | LPAREN seq_expr RPAREN { $2 }
  | ID               { Id($1) }
  | COND_VAR         { CondVar } 
  | NOT expr         { Unop(Not, $2) }
  | MINUS expr       %prec TIMES { Unop(Neg, $2) } 
  | func_bind IN seq_expr { FuncBind($1, $3) } 
  | TILDE ID expr_seq_opt cond_opt  { Eval($2, $3, $4) }  
/*  | TILDE ID expr_seq_opt   { Eval($2, $3, Noexpr) }     */
  | IF seq_expr THEN expr ELSE expr { If($2, $4, $6) }
/*  | IF seq_expr THEN expr { If($2, $4, Noexpr) }  */
  | LBRACK expr_list_opt RBRACK { ListBuilder($2) }   
  | LET val_bind_list IN seq_expr {ValBind($2,$4) } 
  | MATCH seq_expr WITH pattern_match  { Match($2, $4) } 
  | ID LBRACK expr RBRACK { GetIndex($1,$3) }

/* Function binding */

func_bind:    
  function_decl assn_op seq_expr 
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
    /* nothing */   %prec above_SEMI { [] }
  | expr_seq       %prec above_SEMI { List.rev $1 }

expr_seq:
     expr          %prec above_SEMI  { [$1] }
   | expr_seq expr %prec above_SEMI { $2 :: $1 }


/*expr_seq_opt:
  | expr  %prec ID { $1 }*/

cond_opt:
  /* nothing*/ %prec below_BAR { Noexpr }
  | GIVEN expr  { $2 } 


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
  | val_bind_list BIND_SEP val_bind { $3 :: $1 } 

val_bind: 
   val_decl EQSYM seq_expr {{vdecl = $1; vexpr = $3}}


/* Pattern matching */

pattern_match:
  | bar_opt pattern ARROW seq_expr { Pattern($2, $4, NoPattern) }
  | pattern_match BAR pattern ARROW seq_expr   { Pattern($3, $5, $1) }
/*  pattern ARROW expr pattern_match_cont { Pattern($1, $3, $4) }
  | BAR pattern ARROW expr pattern_match_cont { Pattern($2, $4, $5) }
  | LPAREN pattern RPAREN ARROW expr pattern_match_cont { Pattern($2,$5,$6) } */

bar_opt:
  | /* nothing */ {} 
  | BAR  {}

/*pattern_match_cont:
  nothing  %prec MATCH { NoPattern }
  | BAR pattern ARROW expr pattern_match_cont { Pattern($2, $4, $5) }
  | BAR LPAREN pattern RPAREN ARROW expr pattern_match_cont { Pattern($3,$6,$7) }*/

pattern:
  | LPAREN pattern RPAREN { $2 }
  | ID { Ident($1) }
  | WILDCARD { Wildcard }
  | pattern ATTACH pattern { Concat($1, $3) }
