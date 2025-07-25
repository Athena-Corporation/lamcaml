(*

Language Semantics and Implementation

- syntax: (the way we write stuff)
- semantics: (the meaning of programs)
  1. small step semantics
      => GIVE MEANING TO VERY SMALL ATOMIC CONSTRUCTS FOR THE LANGUAGE'S
      SYNTAX AND LET THE PROGRAMMER DEFINE THE WHOLE MEANING OF A PROGRAM
  2. PROCESS MEANING
      a). interpreter (Python, Java and so on )
            Reads the sentences and itself will "run" the semantics
      b). compiler (OCaml, C/C++ and so on)
            Reads the sentences and transform it into binary code (machine code).

Program P =
  for(int i ; i < 10 ; i++) {
    print "I am at : " + i
  }
MEANS..=> "in a for loop we count i from 0 to 9, and print the number"

- values in a language are the the very basic building blocks of data
      e.g., int, booleans, lists, user defined datatypes... are values

I : Expression -> Value -> Value

*)

type expr = Error | Num of int | Add of expr * expr

(* 0 + 0 *)
let example = Add (Num 9, Num 0)

let rec interpreter (e : expr) : expr =
  match e with
  | Error -> Error
  | Num x -> Num x
  | Add (x, y) -> 
    match (interpreter x, interpreter y) with 
    | (Num a, Num b) -> Num (a + b)
    | _ -> Error

let rec int_of_expr (e: expr) : int = 
  match e with 
  | Num x -> x
  | _ -> 000

let () = 
  let result = interpreter example in
  print_endline (string_of_int (int_of_expr result))
