%{
    open AST
%}

%token EOF
%token <char> BOOL
%token <int> INT
%token <float> FLOAT
%token <int array> INTVEC
%token <float array> FLOATVEC
%token <int array array> INTMAT
%token <float array array> FLOATMAT
%token CREATEINTVEC CREATEFLOATVEC CREATEINTMAT CREATEFLOATMAT
%token LPAREN RPAREN LBRACE RBRACE LSQ RSQ DELIM SENTEND
%token RAISE PRINT INPUT IF THEN ELSE WHILE FOR
%token <string> PRINTVAR
%token <string> INPUTFILE
%token <string> VAR
%token TYPEBOOL TYPEINT TYPEFLOAT TYPEVEC TYPEMAT
%token ASSIGN PLUS MINUS TIMES DIV MOD SQRT MAGHALF ANGLE DIM TRANSPOSE MINOR INVERSE ADJOINT AND OR NOT DOT EQ NE LT GT LTE GTE

%nonassoc THEN ELSE
%nonassoc IF
%right ASSIGN
%left OR
%left AND
%right NOT
%nonassoc EQ NE LT GT LTE GTE
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc DOT
%right NEG POS 
%nonassoc LSQ RSQ
%right MAGHALF SQRT TRANSPOSE INVERSE ADJOINT MINOR DIM ANGLE

%start program
%type <AST.stmt> program

%%

program:
  | stmts EOF { Block($1) }
  ;

stmts:
    | stmt stmts { [$1] @ $2 }
    | stmt { [$1] }
    ;

decl_var:
  | VAR { Var($1) }
  ;
lhs_expr:
  | decl_var { $1 }
  | lhs_expr LSQ expr RSQ { Index($1, $3) }
  ;
atomic_expr:
  | BOOL { ConstBool $1 }
  | INT { ConstSInt $1 }
  | FLOAT { ConstS $1 }
  | INTVEC { ConstVInt $1 }
  | FLOATVEC { ConstV $1 }
  | INTMAT { ConstMInt $1 }
  | FLOATMAT { ConstM $1 }
  | LPAREN expr RPAREN { $2 }
  ;
expr:
  | atomic_expr { $1 }
  | lhs_expr { $1 }
  | MINUS expr %prec NEG { Neg($2) }
  | PLUS expr %prec POS { Pos($2) }
  | NOT expr { Not($2) }
  | expr TIMES expr { Prod($1, $3) }
  | expr DIV expr { Div($1, $3) }
  | expr MOD expr { Rem($1, $3) }
  | expr PLUS expr { Add($1, $3) }
  | expr MINUS expr { Sub($1, $3) }
  | MAGHALF expr MAGHALF { Mag($2) }
  | SQRT LPAREN expr RPAREN { Sqrt($3) }
  | expr DOT expr { DotProd($1, $3) }
  | ANGLE LPAREN expr DELIM expr RPAREN { Angle($3, $5) }
  | DIM LPAREN expr RPAREN { Dimension($3) }
  | TRANSPOSE LPAREN expr RPAREN { Transpose($3) }
  | INVERSE LPAREN expr RPAREN { Inverse($3) }
  | ADJOINT LPAREN expr RPAREN { Adjoint($3) }
  | MINOR LPAREN expr DELIM expr DELIM expr RPAREN { Minor($3, $5, $7) }
  | CREATEINTVEC LPAREN expr DELIM expr RPAREN { CreateVecInt($3, $5) }
  | CREATEFLOATVEC LPAREN expr DELIM expr RPAREN { CreateVecFloat($3, $5) }
  | CREATEINTMAT LPAREN expr DELIM expr DELIM expr RPAREN { CreateMatInt($3, $5, $7) }
  | CREATEFLOATMAT LPAREN expr DELIM expr DELIM expr RPAREN { CreateMatFloat($3, $5, $7) }
  | expr AND expr { And($1, $3) }
  | expr OR expr { Or($1, $3) }
  | expr EQ expr { Comp(Eq, $1, $3) }
  | expr NE expr { Comp(Ne, $1, $3) }
  | expr LT expr { Comp(Lt, $1, $3) }
  | expr GT expr { Comp(Gt, $1, $3) }
  | expr LTE expr { Comp(Lte, $1, $3) }
  | expr GTE expr { Comp(Gte, $1, $3) }
  | IF expr THEN expr ELSE expr { Cond($2, $4, $6) }
  ;


type_expr:
  | TYPEBOOL  { Bool }
  | TYPEINT { ScalarInt }
  | TYPEFLOAT { ScalarFloat }
  | TYPEINT TYPEVEC  { VectorInt None }
  | TYPEINT TYPEVEC LPAREN INT RPAREN  { VectorInt (Some $4) }
  | TYPEFLOAT TYPEVEC { VectorFloat None }
  | TYPEFLOAT TYPEVEC LPAREN INT RPAREN  { VectorFloat (Some $4) }
  | TYPEINT TYPEMAT  { MatrixInt None }
  | TYPEINT TYPEMAT LPAREN INT DELIM INT RPAREN  { MatrixInt (Some ($4, $6)) }
  | TYPEFLOAT TYPEMAT  { MatrixFloat None }
  | TYPEFLOAT TYPEMAT LPAREN INT DELIM INT RPAREN  { MatrixFloat (Some ($4, $6)) }
  ;

stmt:
  | type_expr decl_var ASSIGN expr SENTEND { Define($1, $2, $4) }
  | lhs_expr ASSIGN expr SENTEND { Reassign($1, $3) }
//  | expr LSQ expr RSQ ASSIGN expr SENTEND { Reassign(Index($1, $3), $6) }
  | PRINTVAR SENTEND { Print(Var($1)) }
  | type_expr decl_var ASSIGN INPUT SENTEND { Input(Some $1, $2, None) }
  | lhs_expr ASSIGN INPUT SENTEND { Input(None, $1, None) }
//  | expr LSQ expr RSQ ASSIGN INPUT SENTEND { Input(None, Index($1, $3), None) }
  | type_expr decl_var ASSIGN INPUTFILE SENTEND { Input(Some $1, $2 , Some $4) }
  | lhs_expr ASSIGN INPUTFILE SENTEND { Input(None, $1, Some $3) }
//  | expr LSQ expr RSQ ASSIGN INPUTFILE SENTEND { Input(None, Index($1, $3), Some $6) }
  | RAISE decl_var SENTEND { Raise($2) }
  | LBRACE stmts RBRACE { Block($2) }
  | IF expr THEN stmt ELSE stmt { IfTE($2, $4, $6) }
  | FOR LPAREN decl_var DELIM expr DELIM expr RPAREN stmt { For($3, $5, $7, $9) }
  | WHILE expr stmt { While($2, $3) }
  ;

%%