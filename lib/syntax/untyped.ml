
(*
  1. Our datatype for expressions/terms
*)

(* Expressions.

To define terms, we need:
  - a type for variables
With that, an expression is:
  - a variable
  - if s and t are expressions, (s t) is an expression
  - if s is an expression and x is a variable, then λ x. s is an expression

Implement this.
*)

(* Expressions, this is def. 1.3.2 from the book. *)
type expr =
  | Var of string
  | App of expr * expr
  | Lam of string * expr

(* Exercise 0. Implement syntactical identity is a predicate. *)
(* Notation 1.3.4 *)

(* Exercise 1. Implement subterms. *)
(* Definition 1.3.5 *)

(* Ex. 2. Implement a predicate (function that the output type is boolean) for proper subterms.*)
(* Definition 1.3.8 *)

(* Section 1.4 - Free and Bound Variables. -----------------------*)

(* Ex. 3. Implement Definition 1.4.1 *)

(* Ex. 4. Implement a predicate for closed terms. *)
(* Definition 1.4.3 *)







(* Example, the identity on lambda calculus:
  -
  M  = λ x. (x (λ y . y)) => "the semantics": take the first argument 'x' and apply the identity to x.
  M' = λ z. (z (λ y. y))  => "the semantics" : the same as M.

   "The name of bound variables should not affect its semantics."
   Intuition:
   f(x) = x + 2 => "the semantics": "take an input, add 2 to this input"
   f(z) = z + 2 => the same...
   f(w) = ... and so on...

   f(x) = x + 2 + y => "the semantics" :
    "take the input and add it to 2 and an unknown constant y"
   f'(x) = x + 2 + z => "take the input add it to 2 and z"

*)

(* Exercises for this type: *)
(*
1. Pretty printing.
2. Notions of equality.
    - Syntactic equality ≡
    - Alpha equality =α
*)
