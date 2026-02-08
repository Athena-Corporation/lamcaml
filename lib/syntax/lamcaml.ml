type expr  =
  | Var of string
  | App of expr * expr
  | Lam of string * expr
  | If of expr * expr * expr  
  | Op of native_op
  | Num of int
  | Bool of bool
and native_op =
  | Add
  | Sub


let rec string_of_expr e =
  match e with
  | Var x -> x
  | App (e1, e2) -> "(" ^ string_of_expr e1 ^ "•" ^ string_of_expr e2 ^ ")"
  | Lam (x, body) -> "λ" ^ x ^ "." ^ "(" ^ string_of_expr body ^ ")"
  | Num x -> string_of_int x
  | Bool x -> if x = true then "True" else "False"
  | If (x, y, z) -> "if" ^ string_of_expr x ^ "then" ^ string_of_expr y ^ "else" ^ string_of_expr z 
  | _ -> failwith "error" 

let is_value e  = 
  match e with 
  | Bool _ -> true
  | Lam _ -> true
  | Num _ -> true
  | _ -> false

let eval_Op op v1 v2 = 
  match op, v1, v2 with 
  | Add, Num x, Num y -> Num (x + y) 
  | Sub, Num x, Num y -> Num (x - y)
  | _ -> failwith "error"


let rec gen_list (e:expr) : string list = 
  match e with 
  | Var x -> x :: []
  | App (l, r) -> gen_list l @ gen_list r @ []
  | Lam(x, body) -> x ::gen_list body @ []
  | If(f, s, t) -> gen_list f @ gen_list s @ gen_list t
  | Num n -> string_of_int(n) :: []
  | Bool v -> string_of_bool(v) :: []
  | Op _ -> "Op" :: []

let rec check_var (v: string) (lst: string list) : bool = 
  match lst with 
  | [] -> false
  | h :: t -> h = v || check_var v t 


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