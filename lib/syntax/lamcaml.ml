type expr  =
  | Var of string
  | App of expr * expr
  | Lam of string * expr
  | If of expr * expr * expr  
  | Op of native_op
  | Num of int
  | Boul of bool

and native_op =
  | Add
  | Sub

let eval_Op op v1 v2 = 
  match op, v1, v2 with 
  | Add, Num x, Num y -> Num (x + y) 
  | Sub, Num x, Num y -> Num (x - y)
  | _ -> failwith "error"

let rec string_of_expr e =
  match e with
  | Var x -> x
  | App (e1, e2) -> "(" ^ string_of_expr e1 ^ "•" ^ string_of_expr e2 ^ ")"
  | Lam (x, body) -> "λ" ^ x ^ "." ^ "(" ^ string_of_expr body ^ ")"
  | Num x -> string_of_int x
  | Boul x -> if x = true then "True" else "False"
  | If (x, y, z) -> "if" ^ string_of_expr x ^ "then" ^ string_of_expr y ^ "else" ^ string_of_expr z 
  | _ -> failwith "error"


(*
Primitive types:

Booleans: true/false
Integers: ... -1, 0, 1, ...
Functions: λ x. p

Syntax of the language (expressions)

Values  v, v' ::== true | false | λ x. p | Number
Expressions p,q,r ::== v | p q | if p then q else r | NativeOp

where NativeOp can be:
It is usually useful to have some operators to each primitive type, e.g.,
    - booleans: /\ and \/
    - integers: +  and -

An Example of a program that should be able to define:

function is_even x = {
  if div_rest (x, 2) = 0
    then true
  else
    false
  }

  function f z = {
    if (is_even 2)
      then X
  else
    Y
  }

*)
