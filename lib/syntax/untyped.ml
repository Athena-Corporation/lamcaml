
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

(* Exercise 0. Implement syntactical identity is a predicate. *)
(* Notation 1.3.4 *)
let rec equal_expr e1 e2 : bool =
  let open String in (* locally open strings *)
  match e1, e2 with
  | Var x, Var y -> equal x y
  | App (l1, r1), App (l2, r2) -> equal_expr l1 l2 && equal_expr r1 r2
  | Lam (x1, b1), Lam (x2, b2) -> equal x1 x2 && equal_expr b1 b2
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


(* Section 1.4 - Free and Bound Variables. -----------------------*)

(* Ex. 3. Implement Definition 1.4.1 *)
let rec free_vars exp =
  match exp with
  | Var x -> [x]
  | App (x1, x2) -> free_vars x1 @ free_vars x2
  | Lam (x, body) -> List.filter (fun y -> y <> x) (free_vars body)

let rec bound_vars exp =
  match exp with
  | Var _ -> []
  | App (x1, x2) -> bound_vars x1 @ bound_vars x2
  | Lam (x, body) -> x :: bound_vars body

(* Ex. 4. Implement a predicate for closed terms. *)
(** Definition 1.4.3 *)
let combinator exp =
  match free_vars exp with
  | [] -> true
  | _ -> false

(* Variables and renaming *)

(*

Ex. 5 - Implement a predicate, i.e., a function from a type to booleans,
that given two terms s and t as input returns true if s is α-equivalent to t.
(In the book this is basically Def. 1.5.2).
Hint: you need to also implement renaming (Def. 1.5.1)

 *)


let rec rename_check (e:expr) (oldn: string) (nwn:string) : bool = 
  match e with 
  |Var x -> if x = oldn then x = nwn else false
  |App (x1, x2) -> rename_check x1 oldn nwn || rename_check x2 oldn nwn
  |Lam (x1, body) -> if x1 = oldn then x1 = nwn else false || rename_check body oldn nwn

let rec rename (e: expr) (oldn: string) (nwn : string) : expr = 
  if not (rename_check e oldn nwn) then e else 
    match e with 
    | Var x -> if x = oldn then Var nwn else Var x 
    | App (x1, x2) -> App (rename x1 oldn nwn, rename x2 oldn nwn)
    | Lam (x1, body) -> if x1 = oldn then Lam (nwn, rename body oldn nwn) else Lam (x1, rename body oldn nwn)  
(* Has some wring thigns int eh function, the newname shoudl be fresh and should not appear in teh aplhaequivalent functions*)

let rec is_alpha (e1: expr) (e2: expr) : bool = 
  match (e1,e2) with 
  | Var x, Var y -> x = y
  | App (x1, x2), App (y1,y2) -> is_alpha x1 y1 && is_alpha x2 y2
  | Lam (x1, x2), Lam (y1, y2) -> if x1 = y1 then is_alpha x2 y2 else is_alpha x2 (rename y2 y1 x1)
  | _ -> false

(*
Ex. 6 - Implement Substitution: Def. 1.6.1.
  Hint Question: what is a good data structure for substitutions?
*)
