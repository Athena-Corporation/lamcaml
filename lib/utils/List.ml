(*Finsih task, output type should be string; should be generic;  hint use fold

[ x1 ; x2 ; ... ; xn ]

[ x1 , x2 , ... , xn ]

*)

let list_to_string (f : 'a -> string) (l: 'a list) : string =
  let rec body_to_string f l =
    match l with
    | [] -> String.empty
    | hd :: tl ->
      (f hd) ^ " ; " ^ (body_to_string f tl)
  in
  match l with
  | [] -> String.empty
  | hd :: [] -> "[" ^ (f hd) ^ "]"
  | hd :: tl -> "[" ^ (body_to_string f l) ^ "]"
