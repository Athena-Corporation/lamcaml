(**
[list_to_string (f: a') (l)] where the function takes any type of list, and turns it into a', 
then it takes the body of the function and apennds it into the list,
finally it adds the brackets and the first memebr of the list and follows it by teh body of the list
*)

let list_to_string (f : 'a -> string) (l: 'a list) : string =
  let rec body_to_string f l =
    match l with
    | [] -> String.empty
    | hd :: [] -> (f hd) 
    | hd :: tl ->(f hd) ^ " ; " ^ (body_to_string f tl)
  in
  match l with
  | [] -> String.empty
  | hd :: [] -> "[" ^ (f hd) ^ "]"
  | hd :: tl -> "[" ^ (f hd) ^ " ; " ^ (body_to_string f tl) ^ "]"

  