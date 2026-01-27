
(*
Values  v, v' ::== true | false | λ x. p | Number
Expressions p,q,r ::== v | p q | if p then q else r | NativeOp
*)
type expr =
  | Var of string
  | App of expr * expr
  | Lam of string * expr (* you have to implemente renaming and everything *)
(*   | LamShallow of (string -> expr) (* ocaml does the renaming, Higher-Order Abstract Syntax *) *)
  | If of expr * expr * expr
  | Op of native_op
  | Num of int (* shallow embeding *)
  | Bool of bool (* shallow embedding *)
and native_op =
  | Add
  | Sub

(* and values =

  (Add(ex1, ex2)) σ = Add(exp1σ , exp2σ)
  (true σ) = true

*)

let is_value (e : expr) =
  match e with
  | Bool _ -> true
  | Lam _ -> true
  | Num _ -> true
  | _ -> false

(*
  In call-by-value semantics, (eager evaluation) means that we evaluate the arguments given to a function firs, only then we evaluate the whole function.
      (λ x. M) (arg : expr)

      SEMANTICS : HOW THE PROGRAMMING LANGUAGE COMPUTES

      if (exp1) then exp2 else exp3

*)

let b = if (3 * 0 > 3) then false else true

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
  | Bool x -> if x = true then "True" else "False"
  | If (x, y, z) -> "if" ^ string_of_expr x ^ "then" ^ string_of_expr y ^ "else" ^ string_of_expr z
  | _ -> failwith "error"


(*
Primitive types:

Booleans: true/false
Integers: ... -1, 0, 1, ...
Functions: λ x. p

Syntax of the language (expressions)



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

(* TASK 1. Implemente substitutions for this new language. *)
(* TASK 2. Define the structural operational semantics of this language.
    - small-step semantics.
*)
