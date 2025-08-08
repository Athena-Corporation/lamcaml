
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
let rec equal_expr e1 e2: bool = 
  match e1, e2 with 
  | Var x, Var y -> x = y
  | App(e1, r1), App(e2, r2) -> equal_expr e1 e2 && equal_expr r1 r2
  | Lam(x1, body1), Lam(x2, body2) -> x1 = x2 && equal_expr body1 body2 
  | _, _ -> false 

let () =
  let e1 = Lam ("x", App (Var "x", Var "x")) in
  let e2 = Lam ("x", App (Var "x", Var "x")) in
  let e3 = Lam ("y", App (Var "y", Var "u")) in
  let e4 = Lam ("x", App (Var "x", Var "y")) in   

  let result1 = equal_expr e1 e2 in
  print_endline (string_of_bool result1);   

  let result2 = equal_expr e3 e2 in
  print_endline (string_of_bool result2);    

  let result3 = equal_expr e4 e2 in
  print_endline (string_of_bool result3)     


(* Exercise 1. Implement subterms. *)
(* Definition 1.3.5 *)
let rec sub_term e : expr list  = 
  match e with 
  | Var _  ->  [e]
  | App(e1, e2) -> e :: sub_term e1 @ sub_term e2 
  | Lam(_, body) -> e :: sub_term body 
 Create string of expr !!!!!!!!!!!!!!!! 
let rec print_list lst = 
  match lst with 
  | [] -> () 
  | h :: t -> print_endline( string_of_expr h); print_list t 
let () =
  let e1 = Lam ("x", App (Var "x", Var "x")) in
  let result1 = sub_term e1 in 
  print_list result1



(* Ex. 2. Implement a predicate (function that the output type is boolean) for proper subterms.*)
(* Definition 1.3.8 *)

(* Section 1.4 - Free and Bound Variables. -----------------------*)

(* Ex. 3. Implement Definition 1.4.1 *)

(* Ex. 4. Implement a predicate for closed terms. *)
(* Definition 1.4.3 *)

