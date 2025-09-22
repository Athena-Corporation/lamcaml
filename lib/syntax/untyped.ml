
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
(* Definition 1.4.3 *)
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

(* This function checks whether the new name nwn is valid
(fresh = doesn't occur anywhere in in the expression).
*)
let rec rename_check (e:expr) (oldn: string) (nwn:string) : bool =
  match e with
  |Var x -> if x = oldn then x = nwn else false
  |App (x1, x2) -> rename_check x1 oldn nwn || rename_check x2 oldn nwn
  |Lam (x1, body) -> if x1 = oldn then x1 = nwn else false || rename_check body oldn nwn

(* This function satisfy the following:
    - it only returns true if the "name" argument is completely new.
*)
let rec is_fresh (e : expr) (name : string) : bool =
  match e with
  | Var x ->
    if x = name then false else true
  | App(l, r) ->
    is_fresh l name && is_fresh r name
  | Lam (x, e') ->
    if x = name then false else is_fresh e' name

(**
[rename e oldn nwn] is the expression [e'] resulting from replacing {b}every{b}
free occurrence of the name [oldn] with the argument name [nwm].

@Error the function exists with error if the new name is not fresh.
*)
let rec rename (e: expr) (oldn: string) (nwn : string) : expr =
  if is_fresh e nwn then
    match e with
    | (Var x) as v ->
      if x = oldn then Var nwn else v
    | App (l, r) -> App (rename l oldn nwn, rename r oldn nwn)
    | Lam (x, body) ->
      Lam (x, rename body oldn nwn)
  else
  failwith (
    "Renaming impossible as the new name " ^
    nwn ^
    " is not fresh in " ^ (string_of_expr e)
  )
(* Has some wring thigns int eh function, the newname shoudl be fresh and should not appear in teh aplhaequivalent functions*)

(* A function [gen_new_name e] of an expression e, should return a string such that it is a fresh name for e. *)
(* Hint:
  1. Compute the list of all names that appear in e, let's call it l.
    This will give us a list of names that for sure are not fresh for e.
  2. Compute the length of l, let's call it n.
    Why? Because this will give us the number of names that exists.
  3. Let the CANDIDATE for new name be : "x" ^ (string_of_int n) (which is exactly xn), let's call this x_n.
  4. Test if x_n is in l.
      - if x_n is in l then
          let's try a new name with now, "x" ^ (string_of_int (n + 1))
          and go back to 4 to test the new name
      - else
          return x_(n + ...)
Main idea:
  - since we increase the index by one each time we fail and there are finitely many variables in l,
    what will happen is that at some point we will create a name, let's say, x_m for some m
    that doesn't occur in l.

You can test that for any expression e, is_fresh e (gen_new_name e) will be true.
*)
let gen_new_name (e : expr) : string =
  (* let l = <compute the list of all names that appear in e> in *)
  failwith "not implemented"




let rec is_alpha (e1: expr) (e2: expr) : bool =
  match (e1,e2) with
  | Var x, Var y -> x = y
  | App (x1, x2), App (y1,y2) -> is_alpha x1 y1 && is_alpha x2 y2
  | Lam (x, e1), Lam (y, e2) ->
    if x = y then
      is_alpha e1 e2
    else
      (* Challange: how to generate a new name such that is_fresh of the new name that we generate will give true. *)
      is_alpha e1 (rename e2 y x)
  | _ -> false

(*  *)

(*  *)




(*
Ex. 6 - Implement Substitution: Def. 1.6.1.
  Hint Question: what is a good data structure for substitutions?
*)
