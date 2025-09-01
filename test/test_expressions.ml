(*
α-equivalence

λ-terms: x | s t | λ x. t


1. Goal: "model/implement" functional application

f(x) = x + y        => λ x . x + y

f(5) = 5 + y        => (λ x. x + y) 5 →? 5 + y

What is needed for this implementation?

Let us apply f(5), step-by-step:
1. pen-and-paper: Notice that to apply f(5) we substitute in the body of the function
f wich is "x + y" all occurrences of x by 5.
2. The result is 5 + y.

λ x . x + y => λ x. M where M = x + y

A substitution as a function from variables to terms.
[x := t]

Definitin of substitution:

- x [ x := N] = N ,
- y [ x := N] = y
- (M N) [ x := L] = (M [x := L]) (N [ x := L])
- lambda case? NEXT MEETING AS IT IS MORE COMPLICATED

- example: (x + y) [x := 5] = (x [x := 5]) + (y [x := 5])
                              = (5) + y


f(x) = x + 1 | f(y) = y + 1 | f(z) = z + 1



*)












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

let rec equal e e' =
  match (e, e') with
  | (Error, Error) -> true
  | (Num n, Num m) -> Int.equal n m
  | (Add (e1, e2), Add(g1, g2)) ->
    equal e1 g1 && equal e2 g2
  | _ -> false

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
