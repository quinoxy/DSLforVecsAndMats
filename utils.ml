
let vec_of_string_helper f s =
  try
    let s = String.trim s in
    let s = if String.length s > 0 && s.[0] = ',' then String.sub s 1 ((String.length s) - 1) else s in
    let s = if String.length s > 0 && s.[0] = '[' then String.sub s 1 ((String.length s) - 1) else s in
    let s = if String.length s > 0 && s.[String.length s - 1] = ']' then String.sub s 0 ((String.length s) - 1) else s in
    List.map f (List.map String.trim (String.split_on_char ',' s))
  with
  | _ -> failwith "Invalid vector format"

let vec_of_string f s =
  try
    let parts = String.split_on_char '[' s in
    match parts with
    | [size_str; elements_str] ->
      let size = int_of_string(String.trim size_str) in
      let elements = vec_of_string_helper f elements_str in
      if ((List.length elements) = size && size>=1) then
        Array.of_list elements
      else
        failwith "Vector size mismatch"
    | _ -> failwith "Invalid vector format"
  with
  | _ -> failwith "Invalid vector format"

let split_at_first c s =
  try
    let index = String.index s c in
    let left = String.sub s 0 index in
    let right = String.sub s (index + 1) ((String.length s) - index - 1) in
    [left; right]
  with Not_found -> [s]
  
let mat_of_string f s =
  try 
    let parts = split_at_first '[' s in
    match parts with
    | [size_tuple; elements_str] ->
      let size_tuple = (String.trim size_tuple) in
      let size_parts = String.split_on_char ',' size_tuple in
      if List.length size_parts <> 2 then failwith "Invalid matrix size format";
      let rows = int_of_string (String.trim (List.nth size_parts 0)) in
      let cols = int_of_string (String.trim (List.nth size_parts 1)) in
      let elements = 
        elements_str
        |> String.trim
        |> fun s -> String.sub s 0 ((String.length s) - 1)
        |> String.split_on_char ']'
        |> List.filter (fun s -> String.length s > 0)
        |> List.map String.trim
        |> List.map (fun s -> if String.length s > 0 && s.[0] = ',' then String.sub s 1 ((String.length s) - 1) else s)
        |> List.map (fun s-> (string_of_int cols) ^ " " ^ s)
        |> List.map (vec_of_string f)
      in
      if ((List.length elements) = rows && rows>=1) then
        Array.of_list elements
      else
        failwith "Matrix size mismatch"
    | _ -> failwith "Invalid matrix format"
  with
  | _ -> failwith "Invalid matrix format"

let matDimType_of_string s =
  try
    let parts = String.split_on_char ',' s in
    if List.length parts <> 2 then failwith "Invalid matrix dimension format";
    let rows = int_of_string (String.trim (List.nth parts 0)) in
    let cols = int_of_string (String.trim (List.nth parts 1)) in
    [rows; cols]
  with
  | _ -> failwith "Invalid matrix dimension format"