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

type subst = string * expr

(* 2. Small-Step Semantics *)
let is_value e =
  match e with
  | Bool _ -> true
  | Lam _ -> true
  | Num _ -> true
  | _ -> false

let rec string_of_expr e =
  match e with
  | Var x -> x
  | App (e1, e2) -> "(" ^ string_of_expr e1 ^ "•" ^ string_of_expr e2 ^ ")"
  | Lam (x, body) -> "λ" ^ x ^ "." ^ "(" ^ string_of_expr body ^ ")"
  | If(f, s, t) -> "If " ^ string_of_expr f ^ " then " ^ string_of_expr s ^ " else " ^ string_of_expr t
  | Bool(true) -> "True"
  | Bool(false) ->"False"
  | Num x -> string_of_int x 
  | _ -> failwith "Error"


let rec gen_list (e:expr) : string list =
  match e with
  | Var x -> x :: []
  | App (l, r) -> gen_list l @ gen_list r @ []
  | Lam(x, body) -> x :: gen_list body @ []
  | If(f, s, t) -> gen_list f @ gen_list s @ gen_list t
  | Num n -> string_of_int(n) :: []
  | Bool v -> string_of_bool(v) :: []
  | BinOp (_,e1,e2) -> gen_list e1 @ gen_list e2 @ []
  | UnaOp (_,e) -> gen_list e @ []

let rec is_fresh (e : expr) (name : string) : bool =
  match e with
  | Var x -> if x = name then false else true
  | App(l, r) -> is_fresh l name && is_fresh r name
  | Lam (x, e') -> if x = name then false else is_fresh e' name
  | If (f ,s, t) -> is_fresh f name && is_fresh s name && is_fresh t name
  | BinOp(_, f, s) -> is_fresh f name && is_fresh s name
  | UnaOp(_, f) -> is_fresh f name
  | _ -> false      


let rename (e: expr) (oldn: string) (nwn: string) : expr =
  let rec aux_rename (expr: expr) (newn: string) : expr =
    match expr with
      | Var x as v -> (if x = oldn then Var newn else v)
      | App (l, r) -> App (aux_rename l newn, aux_rename r newn)    
      | If (l, m , r) -> If( aux_rename l newn, aux_rename m newn, aux_rename r newn)
      | Lam (x, body) -> (if x = oldn then expr else Lam (x, aux_rename body newn))
      | _ -> failwith "Type cannot be renamed"
  in
    if is_fresh  e nwn  then
      aux_rename e nwn
  else
    let u' = gen_new_name nwn (gen_list e) in
    if is_fresh e u' then aux_rename e u'
    else 
      failwith "Fatal Error: Implementation of the function gen_new_name in module untyped.ml is not correct."

(*               Substituion         Do Some Tests to See if it works *)

let rec app_sub (s : subst) (t: expr) : expr = 
  let lst_sub = List.rev_append (gen_list t) (gen_list (snd s)) in
  match t with 
  | Var x as v -> if x = (fst s) then (snd s) else v
  | App(l, r) -> App(app_sub s l, app_sub s r) 
  | If(f, m, t) -> If(app_sub s f, app_sub s m, app_sub s t)
  | Lam(x, body) -> 
      if x = (fst s) then t 
      else 
        let u = gen_new_name x ([fst s] @ lst_sub) in
        let new_body = rename body x u in 
        assert (is_fresh body u);
        Lam(u, app_sub s new_body)
  | BinOp(x, f, e) -> BinOp(x, app_sub s f, app_sub s e)
  | UnaOp(x, f) -> UnaOp(x, app_sub s f)
  | Bool _ as b -> b
  | Num _ as n -> n                


let rec beta (e : expr) : expr = 
  match e with
  | Var _ as v -> v
  | Num _ as n -> n
  | Bool _ as b -> b
  | Lam (x, body) -> Lam (x, beta body)

  | App (Lam (x, body), arg) -> 
      let subst = (x, arg) in 
      app_sub subst (beta body)

  | BinOp (typ, l, r) -> 
      (match (typ, beta l, beta r) with 
      | (Add, Num x, Num y) -> Num (x + y)
      | (Leq, Num x, Num y) -> Bool (x <= y)
      | (Geq, Num x, Num y) -> Bool (x >= y)
      | _ -> failwith "Type error")
  
  | UnaOp (typ, e) -> 
    (match (typ, beta e) with
    | (Inv, Num x) -> Num (-x)
    | (Not, Bool x) -> if x = true then (Bool false) else (Bool true)
    | _ -> failwith "Type error"  
    )

  | If(f, s, t) -> 
    (match (beta f) with
    | Bool true -> beta s
    | Bool false -> beta t 
    | _ -> failwith "Type error")

  | App (l, r) -> App (beta l, beta r)



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
