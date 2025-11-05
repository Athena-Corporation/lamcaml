
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

type expr =
  | Var of string
  | App of expr * expr
  | Lam of string * expr

let rec equal_expr e1 e2 : bool =
  let open String in (* locally open strings *)
  match e1, e2 with
  | Var x, Var y -> equal x y
  | App (l1, r1), App (l2, r2) -> equal_expr l1 l2 && equal_expr r1 r2
  | Lam (x1, b1), Lam (x2, b2) -> equal x1 x2 && equal_expr b1 b2
  | _ -> false

let rec string_of_expr e =
  match e with
  | Var x -> x
  | App (e1, e2) -> "(" ^ string_of_expr e1 ^ "•" ^ string_of_expr e2 ^ ")"
  | Lam (x, body) -> "λ" ^ x ^ "." ^ "(" ^ string_of_expr body ^ ")"


let rec sub_term e : expr list =
  match e with
  | Var _ -> [e]
  | App (e1, e2) -> e :: (sub_term e1 @ sub_term e2)
  | Lam (_, body) -> e :: sub_term body

let rec is_subterm x y : bool =
  equal_expr x y ||
  match y with
  | Var _ -> false
  | App (l, r) -> is_subterm x l || is_subterm x r
  | Lam (_, body) -> is_subterm x body


let is_proper_subterm e1 e2 =
  is_subterm e2 e1 && not (equal_expr e1 e2)

let rec free_vars exp =
  match exp with
  | Var x -> [x]
  | App (x1, x2) -> free_vars x1 @ free_vars x2
  | Lam (x, body) -> List.filter (fun y -> y <> x) (free_vars body)

let rec is_free (v: string) (e: expr) : bool  = 
  match e with 
  | Var x -> v = x 
  | App (l, r) -> is_free v l || is_free v r
  | Lam(x, body) -> if v = x then false else is_free v body

let rec bound_vars exp =
  match exp with
  | Var _ -> []
  | App (x1, x2) -> bound_vars x1 @ bound_vars x2
  | Lam (x, body) -> x :: bound_vars body

let combinator exp =
  match free_vars exp with
  | [] -> true
  | _ -> false



(* Variables and renaming *)

(** This function checks whether the new name nwn is valid
(fresh = doesn't occur anywhere in in the expression).This function satisfy the following:
  - it only returns true if the "name" argument is completely new.
*)

let rec gen_list (e: expr) : string list =
  match e with
  | Var x -> x :: []
  | App (l ,r) -> gen_list l @ gen_list r @ []
  | Lam (x, body) -> x :: gen_list body @ []


let rec check_var opfst (lst: string list) : bool =
  match lst with
  | [] -> false
  | h :: t -> h = opfst || check_var opfst t

(**
[gen_new_name opfst lst] 
*)

let rec gen_new_name (opfst: string) (lst: string list) : string =
  if check_var opfst lst then
    let rec unique n =
      let nwn = opfst ^ string_of_int n in
      if check_var nwn lst then unique (n + 1)
      else nwn
    in
    unique 1
  else
    opfst

(**
[is_fresh e name] checks if the expression given already has the string in the lambda expression 
  uses patter mathhing to rec check if the name is in the lambda expr
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
[rename e oldn] is the expression [e'] resulting from replacing {b}every{b}
free occurrence of the name [oldn] with the argument name.

@Error Have to change the thing, bc lambda cannot be renamed, if the lambda is equal
 to the nwn then renaming cannot happen; also the oldn shoulnd need to be compared to x, aux_rname shoudl only be considred about renaming
*)
let rec rename (e: expr) (oldn: string) : expr =
  let u = "u" in
  let rec aux_rename (expr: expr) (newn: string) : expr =
    match expr with
    | Var x -> if x = oldn then Var newn else Var x
    | App (l, r) -> App (aux_rename l newn, aux_rename r newn)
    | Lam (x1, body) ->
        if x1 = oldn then 
          e     (*Change so that it is an exception message leaving it as x gives errors when doing nested lambda functions*)
      else 
        Lam (x1, aux_rename body newn)
  in
  if is_fresh e u then
    aux_rename e u                      
  else
    let u' = gen_new_name u (gen_list e) in
    aux_rename e u'



    
(* Not wokring fix, infinite recursion bug happenign
let rec is_alpha (e1: expr) (e2: expr) : bool =
  match (e1,e2) with
  | Var x, Var y -> x = y
  | App (x1, x2), App (y1,y2) -> is_alpha x1 y1 && is_alpha x2 y2
  | Lam (x, e1) as v1 , Lam (y, e2) ->
    if x = y then is_alpha e1 e2
    else is_alpha v1 (Lam (y, rename e2 y))    
  | _ -> false
*)


(* 
A function [gen_new_name e] of an expression e, should return a string such that it is a fresh name for e. 
 Hint:
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

let gen_new_name (e : expr) : string =
  (* let l = <compute the list of all names that appear in e> in *)
  failwith "not implemented"

*)


(*
Ex. 6 - Implement Substitution: Def. 1.6.1.
  Hint Question: what is a good data structure for substitutions?
In subsutituion we hvae to somehow chnage the vairbales into numbers, so that we can actually get a value 
(λx. x + 1) where x = 5, we have to be able to type this into ocaml and get a value of 6, the one has to chnage from varibale to number

let rec substitute (e; expr) (v: string) : expr = 
  match e with 

*)