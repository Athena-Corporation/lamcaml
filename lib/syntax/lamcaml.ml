open Name_gen

(* 1. Syntax ---------------------------------------------------------------- *)
(* Human-readable syntax of the programs *)
(*
  p,q,r ::= x | p ∙ q | λ x . p | if p then q else r | k | true | false | BinOp p q | UnaOp p
*)

(* AST of our language, i.e., the internal representation of a program *)
type expr =
  (* λ-calculus *)
  | Var of string
  | App of expr * expr
  | Lam of string * expr
  (* λ-calculus + if construction *)
  | If of expr * expr * expr
  (* Base types, ints and bools *)
  | Num of int
  | Bool of bool
  (* Operators over the base types *)
  | BinOp of bin_ops * expr * expr
  | UnaOp of una_ops * expr
and bin_ops =
  | Add
  | Leq
  | Geq
and una_ops =
  | Inv
  | Not


(* 2. Small-Step Semantics *)
let is_value e =
  match e with
  | Bool _ -> true
  | Lam _ -> true
  | Num _ -> true
  | _ -> false

(* TODO: Renaming before substitution *)
let rec gen_list (e:expr) : string list =
  match e with
  | Var x -> x :: []
  | App (l, r) -> gen_list l @ gen_list r @ []
  | Lam(x, body) -> x :: gen_list body @ []
  | If(f, s, t) -> gen_list f @ gen_list s @ gen_list t
  | Num n -> string_of_int(n) :: []
  | Bool v -> string_of_bool(v) :: []
  | BinOp (_,e1,e2) -> failwith "not implemented"
  | UnaOp (_,e) -> failwith "not implemented"

(*
  if (x + y > 0) ...

*)

(* TODO: fix *)
let rename (e: expr) (oldn: string) : expr =
  let rec aux_rename (expr: expr) (newn: string) : expr =
    if is_value e = false then
      match expr with
      | Var x as v -> (if x = oldn then Var newn else v)
      | App (l, r) -> App (aux_rename l newn, aux_rename r newn)    (*Have to make it so that it sees if the expr is a value or not*)
      | If (l, m , r) -> If( aux_rename l newn, aux_rename m newn, aux_rename r newn)
      | _ -> failwith "Type cannot be renamed"
    else
      match expr with
      | Lam (x, body) -> (if x = oldn then expr else Lam (x, aux_rename body newn))
      | _ -> failwith "Type cannot be renamed"

  in
    let u = "u" in
    if check_var u (gen_list(e)) then
      aux_rename e u
  else
    failwith "fatal error"

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


(* TASK 1. Implemente substitutions for this new language. *)
(* TASK 2. Define the structural operational semantics of this language.
    - small-step semantics.
*)
