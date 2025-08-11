
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


let e1 = Lam ("x", App (Var "x", Var "x"))
let e2 = Lam ("x", App (Var "x", Var "x"))
let e3 = Lam ("y", App (Var "y", Var "u"))
let e4 = Lam ("x", App (Var "x", Var "y"))
let e5 = App (Var "x", Var "x")
let e6 = App (Var "x", Var "y")

(* Exercise 0. Implement syntactical identity is a predicate. *)
(* Notation 1.3.4 *)
let rec equal_expr e1 e2 : bool =
  match e1, e2 with
  | Var x, Var y -> x = y
  | App (l1, r1), App (l2, r2) -> equal_expr l1 l2 && equal_expr r1 r2
  | Lam (x1, b1), Lam (x2, b2) -> x1 = x2 && equal_expr b1 b2
  | _ -> false


(* Pretty printer for expr *)
let rec string_of_expr e =
  match e with
  | Var x -> x
  | App (e1, e2) -> "(" ^ string_of_expr e1 ^ "•" ^ string_of_expr e2 ^ ")"
  | Lam (x, body) -> "λ" ^ x ^ "." ^ "(" ^ string_of_expr body ^ ")"


(* Exercise 1. Implement subterms. *)
(* Definition 1.3.5 *)
let rec sub_term e : expr list =
  match e with
  | Var _ -> [e]
  | App (e1, e2) -> e :: (sub_term e1 @ sub_term e2)
  | Lam (_, body) -> e :: sub_term body


let rec print_list lst =
  match lst with
  | [] -> ()
  | h :: t ->
      print_endline (string_of_expr h);
      print_list t

let () =
  print_endline (string_of_bool (equal_expr e3 e2));  
  print_endline (string_of_bool (equal_expr e2 e1));  
  print_endline (string_of_bool (equal_expr e4 e1));;

let () =
  print_list (sub_term e1);
  print_list (sub_term e2);
  print_list (sub_term e3)


(* Ex. 2. Implement a predicate (function that the output type is boolean) for proper subterms.*)
(* Definition 1.3.8 *)

let rec is_subterm x y : bool = 
  equal_expr x y || 
  match y with 
  | Var _ -> false 
  | App (l, r) -> is_subterm x l || is_subterm x r 
  | Lam (_, body) -> is_subterm x body


let is_proper_subterm e1 e2 = 
  is_subterm e2 e1 && not (equal_expr e1 e2)

let () = 
  print_endline (string_of_bool (is_subterm e5 e1));
  print_endline (string_of_bool (is_subterm e6 e1));
  print_endline (string_of_bool (is_proper_subterm e1 e2));
  print_endline (string_of_bool (is_proper_subterm e1 e5))


 
(* Section 1.4 - Free and Bound Variables. -----------------------*)

(* Ex. 3. Implement Definition 1.4.1 *)

(* Ex. 4. Implement a predicate for closed terms. *)
(* Definition 1.4.3 *)
