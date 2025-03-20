

open Array;;



type compOp = Eq | Ne | Lt | Gt | Lte | Gte;;
type types =  Bool    (* boolean *)
            | ScalarFloat   (* a scalar — any float value *)
            | ScalarInt  (* a scalar — any int value *)
            | VectorFloat of int option   (* n-dimensional with elements of type float*)
            | VectorInt of int option  (* n-dimensional with elements of type int*)
            | MatrixFloat of (int * int) option  (* n x m matrix with elements of type float *)
            | MatrixInt of (int * int) option   (* n x m matrix with elements of type int *)
;;

module Env = Map.Make(String);;
type type_env = types Env.t;;


type expr =  
(*constants*)
| ConstBool of char   (* Boolean constants *)
| ConstSInt of int  (* Scalar constants *)
| ConstS of float    (* Scalar constants *)
| ConstV of float array    (* Vector constants *)
| ConstVInt of int array   (* Vector constants *)
| ConstM of float array array   (* Matrix constants *)
| ConstMInt of int array array  (* Matrix constants *)
| Var of string   (* variable *)


(*basic int operations*)
| Add of expr * expr   (* overloaded —sum of  two scalars or sum of two vectors of the same dimension, or even the sum of matrices *)
| Sub of expr * expr   (* difference of two scalars or difference of two vectors of the same dimension or matrices of the same dimensions *)
| Neg of expr     (* overloaded — additive inverse of  a scalar or additive inverse of a vector, or additive inverse of a matrix *)
| Pos of expr (* overloaded — identity of a scalar or vector or matrix *)
| Prod of expr * expr   (* overloaded — product of a scalar with another scalar or product of a scalar and a vector or product of a scalar with a matrix or multiplication of matrix with a matrix*)
| Div of expr * expr   (* division of two scalars *)  
| Rem of expr * expr   (* remainder of two scalars *)
| Mag of expr   (* overloaded: absolute value of a scalar or magnitude of a vector or determinant of a matrix*)
| Sqrt of expr   (* square root of a scalar *)

(*remaining vector operations*)
| DotProd of expr * expr  (*dot product of two vectors of the same dimension *) 
| Index of expr*expr (*overloaded vector/matrix and index*)
| Angle of expr * expr  (* in radians, the angle between two vectors *)
| Dimension of expr (* overloaded: dimension of a vector or number of rows and columns of a matrix *)

(*remaining matrix operations*)
| Transpose of expr  (* transpose of a matrix *)
| Inverse of expr  (* inverse of a square matrix *)
| Adjoint of expr (* adjoint of a square matrix *)
| Minor of expr * expr * expr (* minor of a square matrix *)

(*bool operations*)
| And of expr * expr   (* conjunction of two booleans *)
| Or of expr * expr   (* disjunction of two booleans *)
| Not of expr   (* negation of a boolean *)

(*comparison operations*)
| Comp of compOp * expr * expr  (* comparison of two scalars *)

(*create vector/ matrix operations*)
| CreateVecInt of expr*expr (*size and value*)
| CreateVecFloat of expr*expr (*size and value*)
| CreateMatInt of expr*expr*expr (*size tuple and value*)
| CreateMatFloat of expr*expr*expr (*size tuple and value*)

(*conditional expressions*)
| Cond of expr * expr * expr  (* "if_then_else" --  if the first expr evaluates to T then evaluate the second expr, else the third expr *)
;;

type stmt =
  | Define of types* expr * expr
  | Reassign of expr * expr
  | Print of expr
  | Input of types option * expr * string option  (* filename or None for stdin *)
  | Raise of expr
  | Block of stmt list
  | IfTE of expr * stmt * stmt
  | For of expr * expr * expr * stmt  (* var, start, end, body *)
  | While of expr * stmt
;;

exception Wrong of expr;;
exception TypeError of stmt;;

(*VectorInt none is added to support Input() operations at compile time
That could lead to propagation of this type throughout the program, for which I will add support in later parts of the assignment
Right now I assume that these types are invalid types for expressions*)

let is_equal_types t1 t2 = match t1, t2 with
| ScalarFloat, ScalarFloat -> true
| ScalarInt, ScalarInt -> true
| VectorFloat (Some n1), VectorFloat (Some n2) -> n1 = n2
| VectorInt (Some n1), VectorInt (Some n2) -> n1 = n2
| MatrixFloat (Some (n1, m1)), MatrixFloat (Some (n2, m2)) -> n1 = n2 && m1 = m2
| MatrixInt (Some (n1, m1)), MatrixInt (Some (n2, m2)) -> n1 = n2 && m1 = m2
| _ -> false
;;

let invalid_type t = match t with
| VectorFloat None -> true
| VectorInt None -> true
| MatrixFloat None -> true
| MatrixInt None -> true
| _ -> false
;;

let rec type_of_expr exp variable_types = match exp with
| ConstBool _ -> Bool
| ConstSInt _ -> ScalarInt
| ConstS _ -> ScalarFloat
| ConstV arr -> VectorFloat (Some (Array.length arr))
| ConstVInt arr -> VectorInt (Some (Array.length arr))
| ConstM arr -> MatrixFloat (Some (Array.length arr, Array.length arr.(0)))
| ConstMInt arr -> MatrixInt (Some (Array.length arr, Array.length arr.(0)))
| Var v -> 
  begin
    try Env.find v variable_types with
    | Not_found -> raise (Wrong exp)
  end

| Add (a, b) ->
    begin
      let typ1 = type_of_expr a variable_types in
      let typ2 = type_of_expr b variable_types in
      if is_equal_types typ1 typ2 then typ1 else 
        match typ1, typ2 with
        | ScalarInt, ScalarFloat -> ScalarFloat
        | ScalarFloat, ScalarInt -> ScalarFloat
        | VectorInt Some(n), VectorFloat Some(m) -> if n = m then VectorFloat (Some n) else raise (Wrong exp)
        | VectorFloat Some(n), VectorInt Some(m) -> if n = m then VectorFloat (Some n) else raise (Wrong exp)
        | MatrixInt Some(x, y), MatrixFloat Some(c, d) -> if x = c && y = d then MatrixFloat (Some (c, d)) else raise (Wrong exp)
        | MatrixFloat Some(x, y), MatrixInt Some(c, d) -> if x = c && y = d then MatrixFloat (Some (c, d)) else raise (Wrong exp)
        | _ -> raise (Wrong exp)
    end

| Sub (a, b) ->
  begin
    let typ1 = type_of_expr a variable_types in
    let typ2 = type_of_expr b variable_types in
    if is_equal_types typ1 typ2 then typ1 else 
      match typ1, typ2 with
      | ScalarInt, ScalarFloat -> ScalarFloat
      | ScalarFloat, ScalarInt -> ScalarFloat
      | VectorInt Some(n), VectorFloat Some(m) -> if n = m then VectorFloat (Some n) else raise (Wrong exp)
      | VectorFloat Some(n), VectorInt Some(m) -> if n = m then VectorFloat (Some n) else raise (Wrong exp)
      | MatrixInt Some(x, y), MatrixFloat Some(c, d) -> if x = c && y = d then MatrixFloat (Some (c, d)) else raise (Wrong exp)
      | MatrixFloat Some(x, y), MatrixInt Some(c, d) -> if x = c && y = d then MatrixFloat (Some (c, d)) else raise (Wrong exp)
      | _ -> raise (Wrong exp)
  end

| Neg a ->
    begin
      let typ = type_of_expr a variable_types in
      if invalid_type typ then raise (Wrong exp) else typ
    end

| Pos a ->
    begin
      let typ = type_of_expr a variable_types in
      if invalid_type typ then raise (Wrong exp) else typ
    end

| Prod (x, y) -> 
    begin
      let typ1 = type_of_expr x variable_types in
      let typ2 = type_of_expr y variable_types in 
      match typ1, typ2 with
      | ScalarInt, ScalarInt -> ScalarInt
      | ScalarFloat, ScalarFloat -> ScalarFloat
      | ScalarInt, ScalarFloat -> ScalarFloat
      | ScalarFloat, ScalarInt -> ScalarFloat
      | VectorInt Some(n), ScalarInt -> VectorInt (Some n)
      | VectorInt Some(n), ScalarFloat -> VectorFloat (Some n)
      | VectorFloat Some(n), ScalarInt -> VectorFloat (Some n)
      | VectorFloat Some(n), ScalarFloat -> VectorFloat (Some n)
      | MatrixInt Some(a, b), ScalarInt -> MatrixInt (Some (a, b))
      | MatrixInt Some(a, b), ScalarFloat -> MatrixFloat (Some (a, b))
      | MatrixFloat Some(a, b), ScalarInt -> MatrixFloat (Some (a, b))
      | MatrixFloat Some(a, b), ScalarFloat -> MatrixFloat (Some (a, b))
      | ScalarInt, VectorInt Some(n) -> VectorInt (Some n)
      | ScalarFloat, VectorInt Some(n) -> VectorFloat (Some n)
      | ScalarInt, VectorFloat Some(n) -> VectorFloat (Some n)
      | ScalarFloat, VectorFloat Some(n) -> VectorFloat (Some n)
      | ScalarInt, MatrixInt Some(a, b) -> MatrixInt (Some (a, b))
      | ScalarFloat, MatrixInt Some(a, b) -> MatrixFloat (Some (a, b))
      | ScalarInt, MatrixFloat Some(a, b) -> MatrixFloat (Some (a, b))
      | ScalarFloat, MatrixFloat Some(a, b) -> MatrixFloat (Some (a, b))
      | MatrixInt Some(a, b), VectorInt Some n -> if n = b then VectorInt (Some a) else raise (Wrong exp)
      | MatrixInt Some(a, b), VectorFloat Some n -> if n = b then VectorFloat (Some a) else raise (Wrong exp)
      | MatrixFloat Some(a, b), VectorInt Some n -> if n = b then VectorFloat (Some a) else raise (Wrong exp)
      | MatrixFloat Some(a, b), VectorFloat Some n -> if n = b then VectorFloat (Some a) else raise (Wrong exp)
      | MatrixInt Some(a, b), MatrixInt Some(c, d) -> if b = c then MatrixInt (Some (a, d)) else raise (Wrong exp)
      | MatrixInt Some(a, b), MatrixFloat Some(c, d) -> if b = c then MatrixFloat (Some (a, d)) else raise (Wrong exp)
      | MatrixFloat Some(a, b), MatrixInt Some(c, d) -> if b = c then MatrixFloat (Some (a, d)) else raise (Wrong exp)
      | MatrixFloat Some(a, b), MatrixFloat Some(c, d) -> if b = c then MatrixFloat (Some (a, d)) else raise (Wrong exp)
      | _ -> raise (Wrong exp)
    end

| Div (a, b) ->
    begin
      let typ1 = type_of_expr a variable_types in
      let typ2 = type_of_expr b variable_types in
      if is_equal_types typ1 typ2 then
        match typ1 with
        | ScalarInt -> ScalarInt
        | ScalarFloat -> ScalarFloat
        | _ -> raise (Wrong exp)
      else 
        match typ1, typ2 with
        | ScalarInt, ScalarFloat -> ScalarFloat
        | ScalarFloat, ScalarInt -> ScalarFloat
        | _ -> raise (Wrong exp)
    end

| Rem (a, b) ->
    begin
      let typ1 = type_of_expr a variable_types in
      let typ2 = type_of_expr b variable_types in
      if is_equal_types typ1 typ2 then
        match typ1 with
        | ScalarInt -> ScalarInt
        | ScalarFloat -> ScalarFloat
        | _ -> raise (Wrong exp)
      else
        match typ1, typ2 with
        | ScalarInt, ScalarFloat -> ScalarFloat
        | ScalarFloat, ScalarInt -> ScalarFloat
        | _ -> raise (Wrong exp)
    end

| Mag a ->
    begin
      let typ = type_of_expr a variable_types in
      match typ with
      | ScalarInt -> ScalarInt
      | ScalarFloat -> ScalarFloat
      | VectorInt Some(_) -> ScalarFloat
      | VectorFloat Some(_) -> ScalarFloat
      | MatrixInt Some(a, b) -> if a = b then ScalarFloat else raise (Wrong exp)
      | MatrixFloat Some(a, b) -> if a = b then ScalarFloat else raise (Wrong exp)
      | _ -> raise (Wrong exp)
    end

| Sqrt a ->
    begin
      let typ = type_of_expr a variable_types in
      match typ with
      | ScalarInt -> ScalarFloat
      | ScalarFloat -> ScalarFloat
      | _ -> raise (Wrong exp)
    end

| DotProd (a, b) ->
    begin
      let typ1 = type_of_expr a variable_types in
      let typ2 = type_of_expr b variable_types in
      if is_equal_types typ1 typ2 then
        match typ1 with
        | VectorInt Some(_) -> ScalarInt
        | VectorFloat Some(_) -> ScalarFloat
        | _ -> raise (Wrong exp)
      else
        match typ1, typ2 with
        | VectorInt Some(n), VectorFloat Some(m) -> if n = m then ScalarFloat else raise (Wrong exp)
        | VectorFloat Some(n), VectorInt Some(m) -> if n = m then ScalarFloat else raise (Wrong exp)
        | _ -> raise (Wrong exp)
    end

| Index (v, i) ->
    begin
      let typ1 = type_of_expr v variable_types in
      let typ2 = type_of_expr i variable_types in
      if typ2 != ScalarInt then raise (Wrong exp)
      else match typ1 with
      | VectorInt Some(_) -> ScalarInt
      | VectorFloat Some(_) -> ScalarFloat
      | MatrixInt Some(_, b) -> VectorInt (Some b)
      | MatrixFloat Some(_, b) -> VectorFloat (Some b)
      | _ -> raise (Wrong exp)
    end

| Angle (a, b) ->
  begin
    let typ1 = type_of_expr a variable_types in
    let typ2 = type_of_expr b variable_types in
    if is_equal_types typ1 typ2 then
      match typ1 with
      | VectorInt Some(_) -> ScalarInt
      | VectorFloat Some(_) -> ScalarFloat
      | _ -> raise (Wrong exp)
    else
      match typ1, typ2 with
      | VectorInt Some(n), VectorFloat Some(m) -> if n = m then ScalarFloat else raise (Wrong exp)
      | VectorFloat Some(n), VectorInt Some(m) -> if n = m then ScalarFloat else raise (Wrong exp)
      | _ -> raise (Wrong exp)
  end

| Dimension a ->
    begin
      let typ = type_of_expr a variable_types in
      match typ with
      | VectorInt Some(_) -> ScalarInt
      | VectorFloat Some(_) -> ScalarInt
      | MatrixInt Some(_) -> VectorInt (Some 2)
      | MatrixFloat Some(_) -> VectorInt (Some 2)
      | _ -> raise (Wrong exp)
    end

| Transpose a ->
    begin
      let typ = type_of_expr a variable_types in
      match typ with
      | MatrixInt Some(a, b) -> MatrixInt (Some (b, a))
      | MatrixFloat Some(a, b) -> MatrixFloat (Some (b, a))
      | _ -> raise (Wrong exp)
    end

| Inverse a ->
    begin
      let typ = type_of_expr a variable_types in
      match typ with
      | MatrixInt Some(a, b) -> if a = b then MatrixInt (Some (a, b)) else raise (Wrong exp)
      | MatrixFloat Some(a, b) -> if a = b then MatrixFloat (Some (a, b)) else raise (Wrong exp)
      | _ -> raise (Wrong exp)
    end

| Adjoint a ->
    begin
      let typ = type_of_expr a variable_types in
      match typ with
      | MatrixInt Some(a, b) -> if a = b then MatrixInt (Some (a, b)) else raise (Wrong exp)
      | MatrixFloat Some(a, b) -> if a = b then MatrixFloat (Some (a, b)) else raise (Wrong exp)
      | _ -> raise (Wrong exp)
    end

| Minor (a, b, c) ->
    begin
      let typ1 = type_of_expr a variable_types in
      let typ2 = type_of_expr b variable_types in
      let typ3 = type_of_expr c variable_types in
      if typ2 != ScalarInt || typ3 != ScalarInt then raise (Wrong exp)
      else match typ1 with
      | MatrixInt Some(a, b) -> if a = b then ScalarInt else raise (Wrong exp)
      | MatrixFloat Some(a, b) -> if a = b then ScalarFloat else raise (Wrong exp)
      | _ -> raise (Wrong exp)
    end

| And (a, b) ->
    begin
      let typ1 = type_of_expr a variable_types in
      let typ2 = type_of_expr b variable_types in
      if is_equal_types typ1 typ2 then
        match typ1 with
        | Bool -> Bool
        | _ -> raise (Wrong exp)
      else raise (Wrong exp)
    end

| Or (a, b) ->
    begin
      let typ1 = type_of_expr a variable_types in
      let typ2 = type_of_expr b variable_types in
      if is_equal_types typ1 typ2 then
        match typ1 with
        | Bool -> Bool
        | _ -> raise (Wrong exp)
      else raise (Wrong exp)
    end

| Not a ->
    begin
      let typ = type_of_expr a variable_types in
      if typ = Bool then Bool else raise (Wrong exp)
    end

| Comp (op, a, b) ->
    begin
      let typ1 = type_of_expr a variable_types in
      let typ2 = type_of_expr b variable_types in
      if (typ1=typ2 && (not (invalid_type typ1))) then
        match op with
        |Eq -> Bool
        |Ne -> Bool
        |_ ->
          begin
          match typ1 with
          | ScalarInt -> Bool
          | ScalarFloat -> Bool
          | _ -> raise (Wrong exp)
          end
      else raise (Wrong exp)
    end

| CreateVecInt (s, v) ->
    begin
      let typ1 = type_of_expr s variable_types in
      let typ2 = type_of_expr v variable_types in
      if typ1 != ScalarInt then raise (Wrong exp)
      else match typ2 with
      | ScalarInt ->
          (match s with
          | ConstSInt a -> VectorInt (Some a)
          | _ -> raise (Wrong exp))
      | _ -> raise (Wrong exp)
    end

| CreateVecFloat (s, v) ->
    begin
      let typ1 = type_of_expr s variable_types in
      let typ2 = type_of_expr v variable_types in
      if typ1 != ScalarInt then raise (Wrong exp)
      else match typ2 with
      | ScalarFloat ->
          (match s with
          | ConstSInt a -> VectorFloat (Some a)
          | _ -> raise (Wrong exp))
      |ScalarInt ->
          (match s with
          | ConstSInt a -> VectorFloat (Some a)
          | _ -> raise (Wrong exp))
      | _ -> raise (Wrong exp)
    end

| CreateMatInt (r, c, v) ->
    begin
      let typ1 = type_of_expr r variable_types in
      let typ2 = type_of_expr c variable_types in
      let typ3 = type_of_expr v variable_types in
      if typ1 != ScalarInt || typ2 != ScalarInt then raise (Wrong exp)
      else match typ3 with
      | ScalarInt ->
          (match (r, c) with
          | (ConstSInt a, ConstSInt b) -> MatrixInt (Some (a, b))
          | _ -> raise (Wrong exp))
      | _ -> raise (Wrong exp)
    end

| CreateMatFloat (r, c, v) ->
    begin
      let typ1 = type_of_expr r variable_types in
      let typ2 = type_of_expr c variable_types in
      let typ3 = type_of_expr v variable_types in
      if typ1 != ScalarInt || typ2 != ScalarInt then raise (Wrong exp)
      else match typ3 with
      | ScalarFloat ->
          (match (r, c) with
          | (ConstSInt a, ConstSInt b) -> MatrixFloat (Some (a, b))
          | _ -> raise (Wrong exp))
      | ScalarInt ->
          (match (r, c) with
          | (ConstSInt a, ConstSInt b) -> MatrixFloat (Some (a, b))
          | _ -> raise (Wrong exp))
      | _ -> raise (Wrong exp)
    end

| Cond (c, t, f) ->
    begin
      let typ1 = type_of_expr c variable_types in
      let typ2 = type_of_expr t variable_types in
      let typ3 = type_of_expr f variable_types in
      if typ1 != Bool then raise (Wrong exp)
      else if (is_equal_types typ2 typ3 ||(typ2 = Bool && typ3 = Bool)) then typ2 else raise (Wrong exp)
    end
;;

let compatible t1 t2 = match t1, t2 with
| ScalarInt, ScalarFloat -> true
| VectorInt Some(n), VectorFloat Some(m) -> n = m
| MatrixInt Some(a, b), MatrixFloat Some(c, d) -> a = c && b = d
| VectorInt None, VectorInt Some(_) -> true
| VectorFloat None, VectorFloat Some(_) -> true
| VectorInt None, VectorFloat Some(_) -> true
| MatrixInt None, MatrixInt Some(_) -> true
| MatrixFloat None, MatrixFloat Some(_) -> true
| MatrixInt None, MatrixFloat Some(_) -> true
| _ -> t1 = t2
;;

let rec type_cheker_of_stmt stmt variable_types = match stmt with
| Define (t, v, e) ->
    begin
      let typ = type_of_expr e variable_types in
      if compatible t typ then match v with
        |Var(s) -> Env.add s typ variable_types
        |_ -> raise (TypeError stmt)
      else raise (TypeError stmt)
    end
| Reassign (v, e) ->
    begin
      let typ1 = type_of_expr v variable_types in
      let typ2 = type_of_expr e variable_types in
      if compatible typ1 typ2 then (if typ1 = typ2 then variable_types else match v with 
      |Var(s) -> Env.add s typ2 variable_types 
      |_ -> raise (TypeError stmt)
      ) else raise (TypeError stmt)
    end
| Print e -> 
  begin
    try let _ = type_of_expr e variable_types in variable_types
    with Wrong _ -> raise (TypeError stmt)
  end
| Raise e -> variable_types
| Input (t, v, _) -> 
  begin
    if t = None then variable_types
    else match v with | Var(s) -> Env.add s (Option.get t) variable_types | _ -> raise (TypeError stmt)
  end 

| Block stmts -> 
    begin
      let rec check_stmts stmts variable_types = match stmts with
      | [] -> variable_types
      | hd::tl -> check_stmts tl (type_cheker_of_stmt hd variable_types)
      in check_stmts stmts variable_types
    end
|While (e, s) -> 
  begin
    let cond_type = type_of_expr e variable_types in
    if cond_type != Bool then
      raise (TypeError stmt)
    else
      let _ = type_cheker_of_stmt s variable_types in
      variable_types
    end
|IfTE(cond, ifTrue, ifFalse) ->
  begin
    let cond_type = type_of_expr cond variable_types in
    if cond_type != Bool then
      raise (TypeError stmt)
    else
      let _ = type_cheker_of_stmt ifTrue variable_types in
      let _ = type_cheker_of_stmt ifFalse variable_types in
      variable_types
    end
|For(i, start, stop, body) ->
  begin
    let start_type = type_of_expr start variable_types in
    let stop_type = type_of_expr stop variable_types in
    if (start_type = ScalarInt && stop_type = ScalarInt) then
      match i with
      | Var(s) -> let _ = type_cheker_of_stmt body (Env.add s start_type variable_types) in
                  variable_types
      | _ -> raise (TypeError stmt)
  else
      raise (TypeError stmt)
  end
;;

let inpoint ast = let _ = type_cheker_of_stmt ast Env.empty in ast;;

                












