{
    open Parser
    open Utils
}

let whitespace = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let small = ['a'-'z']
let large = ['A'-'Z']
let letter = small | large
let alphanum = letter | digit
let ident = letter (alphanum | '_' | ''')*
let int = digit+
let sign = ['+' '-']?
let floatNormal = digit* '.' digit+
let filename = ['a'-'z' 'A'-'Z' '0'-'9' '.' '_' '-' '/' '\\' '*' '?' '<' '>' '"' '|']+ (*file name*)
let singlelinecomment = "//" [^ '\n']* ('\n'|eof) (*single line comments*)
let floatScientific = (digit '.' digit+ | digit) ('e' | 'E') sign digit+
let float = floatNormal | floatScientific
let comma = ','
let matrixDimType =  int (whitespace)* comma (whitespace)* int
let intElem = sign int
let floatElem = sign float
let intVec = '[' intElem (whitespace)* (comma (whitespace)* intElem (whitespace)*)* ']'
let floatVec ='[' floatElem (whitespace)* (comma (whitespace)* floatElem (whitespace)*)* ']'
let fullIntVec = int (whitespace)* intVec
let fullFloatVec = int (whitespace)* floatVec
let intMat = matrixDimType (whitespace)* '[' intVec (whitespace)* (comma (whitespace)* intVec (whitespace)*)* ']'
let floatMat =  matrixDimType (whitespace)* '[' floatVec (whitespace)* (comma (whitespace)* floatVec (whitespace)*)* ']'


(*TO DO WHEN BUILDING PARSER AND FURTHER FUNCTIONALITY
+3+3 case sort out -> parser will have to manage this
right now we accept vectors of different sizes within a matrix, but will need to sort this out in the parser 
right now identifiers cant make up matrix dim, or cant be used in vectors, which needs to be fixed in parser
more over vector constants need to be parsed from a string to something better, and similarly for matdimtype
*)

rule token = parse

    (*whitespace*)
    |whitespace {token lexbuf} (*eats whitespace*)

    (*EOF*)
    |eof {EOF}

    (*comments*)
    |singlelinecomment {token lexbuf} (*eats comments*)
    |"```" {multiLineComment lexbuf}
    

    (*constants*)
    |"T"|"F" as retBool {BOOL (retBool)}
    |int as retInt {INT (int_of_string (retInt))}
    |float as retFloat {FLOAT (float_of_string (retFloat))}
    |fullIntVec as retIntVec {INTVEC (vec_of_string int_of_string retIntVec)}
    |fullFloatVec as retFloatVec {FLOATVEC (vec_of_string float_of_string retFloatVec)}
    |intMat as retIntMat {INTMAT (mat_of_string int_of_string retIntMat)}
    |floatMat as retFloatMat {FLOATMAT (mat_of_string float_of_string retFloatMat)}
    (* |matrixDimType as retMatDimType {MATDIMTYPE ((matDimType_of_string retMatDimType))} *)

    (*strings*)

    |"createIntVec" {CREATEINTVEC}
    |"createFloatVec" {CREATEFLOATVEC}
    |"createIntMat" {CREATEINTMAT}
    |"createFloatMat" {CREATEFLOATMAT}

    (*punctuators*)
    |"(" {LPAREN}
    |")" {RPAREN}
    |"{" {LBRACE}
    |"}" {RBRACE}
    |"[" {LSQ}
    |"]" {RSQ}
    |"," {DELIM}
    |";" {SENTEND}

    (*reserved keywords*)
    | "raise"| "RAISE" | "Raise" {RAISE}
    | "print("| "PRINT(" | "Print(" {printfile lexbuf}
    | "input("| "INPUT(" | "Input(" {inputfile lexbuf}
    | "if" | "IF" | "If" {IF}
    | "then" | "THEN" | "Then" {THEN}
    | "else" | "ELSE" | "Else" {ELSE}
    | "while" | "WHILE" | "While" {WHILE}
    | "for" | "FOR" | "For" {FOR}

    (*Types names*)
    | "bool" | "BOOL" | "Bool" | "boolean" | "BOOLEAN" | "Boolean" {TYPEBOOL}
    | "int" | "INT" | "Int" | "integer" | "INTEGER" | "Integer" {TYPEINT}
    | "float" | "FLOAT" | "Float" {TYPEFLOAT}
    | "vec" | "VEC"| "Vec" | "Vector" | "vector" | "VECTOR" {TYPEVEC}
    | "mat" | "MAT" | "Mat" | "matrix" | "MATRIX" | "Matrix" {TYPEMAT}

    (*Operators*)
    | ":=" {ASSIGN}

    (*arithmetic*)
    | "+" {PLUS}  
    | "-" {MINUS}
    | "*" {TIMES}
    | "/" {DIV}
    | "MOD" {MOD}
    | "sqrt" | "SQRT" | "Sqrt" {SQRT}

    (*vector and matrix operations*)
    | "|" {MAGHALF}
    | "angle" | "Angle" | "ANGLE" {ANGLE}
    | "dim" | "Dim" | "DIM" | "dimension" | "Dimension" | "DIMENSION" {DIM}
    | "transpose" | "Transpose" | "TRANSPOSE" {TRANSPOSE}
    | "minor" | "Minor" | "MINOR" {MINOR}
    | "Adj" | "adj" | "ADJ" | "Adjoint" | "ADJOINT" | "adjoint" {ADJOINT}
    | "Inv" | "inv" | "INV" | "Inverse" | "INVERSE" | "inverse" {INVERSE}
    
    (*alternate operators for some operations*)
    | "and" | "AND" | "And" | "&&" {AND}
    | "or" | "OR" | "Or" | "||" {OR}
    | "not" | "NOT" | "Not" | "!" {NOT}
    | "." {DOT}

    (*logical comparision*)
    | "=" {EQ}
    | "<" {LT}
    | ">" {GT}
    | "<=" {LTE}
    | ">=" {GTE}
    | "<>" | "!=" | "=/=" {NE}

    

    (*Variable name*)
    |ident as retVar {VAR (retVar)}

    

    (*error handling*)
    | _ {raise (Failure ("Lexing error: unexpected character" ^ (Lexing.lexeme lexbuf)))}

and multiLineComment = parse
    |"```" {token lexbuf}
    |eof {raise (Failure ("Lexing error: unterminated multiline comment"))}
    |_ {multiLineComment lexbuf}

and inputfile = parse
    |filename as retFilename ")" {INPUTFILE (retFilename)}
    |(whitespace)* ")" {INPUT}
    |eof {raise (Failure ("Lexing error: unterminated input file"))}
    |_ {raise (Failure ("Lexing error: unexpected character"))}

and printfile = parse
    |ident as retVar ")" {PRINTVAR (retVar)}
    |(whitespace)* ")" {PRINT}
    |eof {raise (Failure ("Lexing error: unterminated print file"))}
    |_ {raise (Failure ("Lexing error: unexpected character"))}