(* test_parser.ml *)
open Lexer
open Parser
open Lexing
open AST

let array_to_string to_string arr =
  "[|" ^ (String.concat "; " (Array.to_list (Array.map to_string arr))) ^ "|]"
  ;; 
  
let array2d_to_string to_string arr =
  "[|" ^ (String.concat "; " (Array.to_list (Array.map (fun x -> array_to_string to_string x) arr))) ^ "|]"
  ;;

let rec string_of_type = function
  | Bool -> "Bool"
  | ScalarInt -> "ScalarInt"
  | ScalarFloat -> "ScalarFloat"
  | VectorInt None -> "VectorInt(None)"
  | VectorInt (Some n) -> Printf.sprintf "VectorInt(%d)" n
  | VectorFloat None -> "VectorFloat(None)"
  | VectorFloat (Some n) -> Printf.sprintf "VectorFloat(%d)" n
  | MatrixInt None -> "MatrixInt(None)"
  | MatrixInt (Some (r, c)) -> Printf.sprintf "MatrixInt(%d, %d)" r c
  | MatrixFloat None -> "MatrixFloat(None)"
  | MatrixFloat (Some (r, c)) -> Printf.sprintf "MatrixFloat(%d, %d)" r c
  ;;

let string_of_compOp = function
| Eq -> "Eq"
| Ne -> "Ne"
| Lt -> "Lt"
| Gt -> "Gt"
| Lte -> "Lte"
| Gte -> "Gte"
;;

let rec string_of_expr = function
| ConstBool b -> Printf.sprintf "ConstBool(%c)" b
| ConstSInt i -> Printf.sprintf "ConstSInt(%d)" i
| ConstS f -> Printf.sprintf "ConstS(%f)" f
| ConstV v -> Printf.sprintf "ConstV(%s)" (array_to_string string_of_float v)
| ConstVInt v -> Printf.sprintf "ConstVInt(%s)" (array_to_string string_of_int v)
| ConstM m -> Printf.sprintf "ConstM(%s)" (array2d_to_string string_of_float m)
| ConstMInt m -> Printf.sprintf "ConstMInt(%s)" (array2d_to_string string_of_int m)
| Var v -> Printf.sprintf "Var(%s)" v
| Add (a, b) -> Printf.sprintf "Add(%s, %s)" (string_of_expr a) (string_of_expr b)
| Sub (a, b) -> Printf.sprintf "Sub(%s, %s)" (string_of_expr a) (string_of_expr b)
| Neg a -> Printf.sprintf "Neg(%s)" (string_of_expr a)
| Pos a -> Printf.sprintf "Pos(%s)" (string_of_expr a)
| Prod (a, b) -> Printf.sprintf "Prod(%s, %s)" (string_of_expr a) (string_of_expr b)
| Div (a, b) -> Printf.sprintf "Div(%s, %s)" (string_of_expr a) (string_of_expr b)
| Rem (a, b) -> Printf.sprintf "Rem(%s, %s)" (string_of_expr a) (string_of_expr b)
| Mag a -> Printf.sprintf "Mag(%s)" (string_of_expr a)
| Sqrt a -> Printf.sprintf "Sqrt(%s)" (string_of_expr a)
| DotProd (a, b) -> Printf.sprintf "DotProd(%s, %s)" (string_of_expr a) (string_of_expr b)
| Index (v, i) -> Printf.sprintf "Index(%s, %s)" (string_of_expr v) (string_of_expr i)
| Angle (a, b) -> Printf.sprintf "Angle(%s, %s)" (string_of_expr a) (string_of_expr b)
| Dimension a -> Printf.sprintf "Dimension(%s)" (string_of_expr a)
| Transpose a -> Printf.sprintf "Transpose(%s)" (string_of_expr a)
| Inverse a -> Printf.sprintf "Inverse(%s)" (string_of_expr a)
| Adjoint a -> Printf.sprintf "Adjoint(%s)" (string_of_expr a)
| Minor (a, b, c) -> Printf.sprintf "Minor(%s, %s, %s)" (string_of_expr a) (string_of_expr b) (string_of_expr c)
| And (a, b) -> Printf.sprintf "And(%s, %s)" (string_of_expr a) (string_of_expr b)
| Or (a, b) -> Printf.sprintf "Or(%s, %s)" (string_of_expr a) (string_of_expr b)
| Not a -> Printf.sprintf "Not(%s)" (string_of_expr a)
| Comp (op, a, b) -> Printf.sprintf "Comp(%s, %s, %s)" (string_of_compOp op) (string_of_expr a) (string_of_expr b)
| CreateVecInt (s, v) -> Printf.sprintf "CreateVecInt(%s, %s)" (string_of_expr s) (string_of_expr v)
| CreateVecFloat (s, v) -> Printf.sprintf "CreateVecFloat(%s, %s)" (string_of_expr s) (string_of_expr v)
| CreateMatInt (r, c, v) -> Printf.sprintf "CreateMatInt(%s, %s, %s)" (string_of_expr r) (string_of_expr c) (string_of_expr v)
| CreateMatFloat (r, c, v) -> Printf.sprintf "CreateMatFloat(%s, %s, %s)" (string_of_expr r) (string_of_expr c) (string_of_expr v)
| Cond (c, t, f) -> Printf.sprintf "Cond(%s, %s, %s)" (string_of_expr c) (string_of_expr t) (string_of_expr f)
;;

let rec string_of_stmt = function
  | Define (t, v, e) -> Printf.sprintf "Define(%s, %s, %s)" (string_of_type t) (string_of_expr v) (string_of_expr e)
  | Reassign (v, e) -> Printf.sprintf "Reassign(%s, %s)" (string_of_expr v) (string_of_expr e)
  | Print v -> Printf.sprintf "Print(%s)" (string_of_expr v)
  | Input (t, v, f) -> 
      let t_str = match t with None -> "None" | Some t -> string_of_type t in
      let f_str = match f with None -> "None" | Some f -> Printf.sprintf "\"%s\"" f in
      Printf.sprintf "Input(%s, %s, %s)" t_str (string_of_expr v) f_str
  | Raise v -> Printf.sprintf "Raise(%s)" (string_of_expr v)
  | Block stmts -> Printf.sprintf "Block([\n%s\n])" (String.concat ";\n" (List.map string_of_stmt stmts))
  | IfTE (c, t, e) -> Printf.sprintf "IfTE(%s, %s, %s)" (string_of_expr c) (string_of_stmt t) (string_of_stmt e)
  | For (v, start, stop, body) -> Printf.sprintf "For(%s, %s, %s, %s)" (string_of_expr v) (string_of_expr start) (string_of_expr stop) (string_of_stmt body)
  | While (c, body) -> Printf.sprintf "While(%s, %s)" (string_of_expr c) (string_of_stmt body)
  ;;


let print_ast stmt =
print_endline (string_of_stmt stmt)
;;



let parse_with_error lexbuf =
  try
    let ast = Parser.program Lexer.token lexbuf in
      Printf.printf "Parsed AST:\n";
      print_ast ast;
      Some ast
  with
  | Parsing.Parse_error ->
      Printf.printf "Syntax error at offset %d\n" (lexeme_start lexbuf);
      None


  let parse_file filename =
    let chan =
      if filename = "stdin" then stdin  (* Use standard input correctly *)
      else open_in filename
    in
    let lexbuf = Lexing.from_channel chan in
    let ast = parse_with_error lexbuf in
    if ast = None then
      Printf.printf "Parsing failed\n"
    else 
      try
        let _ = AST.inpoint (Option.get ast) in ();
        Printf.printf "Type checking passed\n"
      with
      | AST.Wrong(e) -> Printf.printf "Type Error: %s\n" (string_of_expr e)
      | AST.TypeError(s) -> Printf.printf "Type Error: %s\n" (string_of_stmt s);
  
    if filename <> "stdin" then close_in chan (* Don't close stdin *)
    ;;

let () =
  if Array.length Sys.argv > 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else if Array.length Sys.argv = 2 then
    parse_file Sys.argv.(1)
  else 
    parse_file "stdin"
  ;;



  