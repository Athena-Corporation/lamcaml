(** Lambda functions*)
type expr =
  | Var of string
  | App of expr * expr
  | Lam of string * expr

(** Given 2 [expr] the [equal_expr] will state if the expression are equal or not.
  Recursively checks if the varibales are equal to each or not using Library [String] and [equal]
*)
let rec equal_expr e1 e2 : bool =
  let open String in (* locally open strings *)
  match e1, e2 with
  | Var x, Var y -> equal x y
  | App (l1, r1), App (l2, r2) -> equal_expr l1 l2 && equal_expr r1 r2
  | Lam (x1, b1), Lam (x2, b2) -> equal x1 x2 && equal_expr b1 b2
  | _ -> false

(** [string_of_expr] gives out any [expr] as a [string] for pretty printing in terminal.
 Adds Lambda symbols and parenthesis for client to see
 *)
let rec string_of_expr e =
  match e with
  | Var x -> x
  | App (e1, e2) -> "(" ^ string_of_expr e1 ^ "•" ^ string_of_expr e2 ^ ")"
  | Lam (x, body) -> "λ" ^ x ^ "." ^ "(" ^ string_of_expr body ^ ")"

(** [sub_term] will break an [expr] down into its Subterms and put in in a list to be printed out.
[Subterms] in Lambda Calculus are the pieces of a lambda expression
{b}Error: the subterms are not actually right it cannot give out the binding var as a subterm{b}
- Ex. [λx.(xx) --> [(xx), (x), (x)]]
*)
let rec sub_term e : expr list =
  match e with
  | Var _ -> [e]
  | App (e1, e2) -> e :: (sub_term e1 @ sub_term e2)
  | Lam (_, body) -> e :: sub_term body

(** [is_subterm] states whether 2 [expr] are Subterms. *)
let rec is_subterm x y : bool =
  equal_expr x y ||
  match y with
  | Var _ -> false
  | App (l, r) -> is_subterm x l || is_subterm x r
  | Lam (_, body) -> is_subterm x body

(** [is_poprsubterm] checks if 2 [expr] are are proper subterms or not.
A [Proper Subterm] is when a Subterm does not equal the orginal expression
*)
let is_propsubterm e1 e2 =
  is_subterm e2 e1 && not (equal_expr e1 e2)

(** [free_vars] checks whether a variable inside an [expr] is free; adds all Free varibales to a list
[Free Variables] in Lambda Calculus is when a varibale is not bound by a binding variable.
- Ex. [λx.(xy) --> (y)]
*)
let rec free_vars exp =
  match exp with
  | Var x -> [x]
  | App (x1, x2) -> free_vars x1 @ free_vars x2
  | Lam (x, body) -> List.filter (fun y -> y <> x) (free_vars body)

(** [is_free] states whether a varible is Free or not in an [expr]
[Free Variables] in Lambda Calculus is when a variable is not bound by a binding variable.
- Ex. [λx.(xy) --> (y)]
*)
let rec is_free (v: string) (e: expr) : bool  =
  match e with
  | Var x -> v = x
  | App (l, r) -> is_free v l || is_free v r
  | Lam(x, body) -> if v = x then false else is_free v body

(** [bound_vars] checks whether a variable inside an [expr] is Bound; adds all Bound varibales to a list
[Bound Variables] in Lambda Calculus is when a variable is bound by a binding variable.
- Ex. [λx.(xy) --> (x)]
*)
let rec bound_vars exp =
  match exp with
  | Var _ -> []
  | App (x1, x2) -> bound_vars x1 @ bound_vars x2
  | Lam (x, body) -> x :: bound_vars body

(** [combinator] states whether an expression is a closed λ term.
[Closed λ-Term/Combinator] is when a λ does not have any Free variables
*)
let combinator exp =
  match free_vars exp with
  | [] -> true
  | _ -> false

(* ----------------------------------------Renaming & helping Functions----------------------------- *)

(** [gen_list] generates a list of all the variables that are present in a given [expr].*)
let rec gen_list (e: expr) : string list =
  match e with
  | Var x -> x :: []
  | App (l ,r) -> gen_list l @ gen_list r @ []
  | Lam (x, body) -> x :: gen_list body @ []

(** [check_var] checks and states if a variable is inside a list. Used to check if a variable is present in the variables
found in a give [expr]
*)
let rec check_var var (lst: string list) : bool =
  match lst with
  | [] -> false
  | h :: t -> h = var || check_var var t

(**
[gen_new_name] will generate a new variable to add into an [expr]; occurs when a variable is not
[Fresh] in an [expr].
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

(** [is_fresh] checks if the the [name: string] is already present in a given [expr] *)
let rec is_fresh (e : expr) (name : string) : bool =
  match e with
  | Var x ->
    if x = name then false else true
  | App(l, r) ->
    is_fresh l name && is_fresh r name
  | Lam (x, e') ->
    if x = name then false else is_fresh e' name

(**
[rename] is the [expr] [e'] resulting from replacing {b}every{b}
free occurrence of the name [oldn] with the argument name.
*)
let rename (e: expr) (oldn: string) : expr =
  let rec aux_rename (expr: expr) (newn: string) : expr =
    match expr with
    | Var x as v -> (
      if x = oldn then
        Var newn
      else
        v
    )
    | App (l, r) -> App (aux_rename l newn, aux_rename r newn)
    | Lam (x, body) -> (
      if x = oldn then
        expr
      else
        Lam (x, aux_rename body newn)
    ) in
  let u = "u" in
  if is_fresh e u then
    aux_rename e u
  else
    let u' = gen_new_name u (gen_list e) in
    if is_fresh e u' then
      aux_rename e u'
    else
      failwith "Fatal Error: Implementation of the function gen_new_name in module untyped.ml is not correct."


(* Renaming: *)
(*
It needs
alpha_variant :
*)

let rename' (e: expr) (oldn: string) (nwn: string): expr =
  let rec aux_rename (expr: expr) (newn: string) : expr =
    match expr with
    | Var x as v -> (
      if x = oldn then
        Var newn
      else
        v
    )
    | App (l, r) -> App (aux_rename l newn, aux_rename r newn)
    | Lam (x, body) -> (
      if x = oldn then
        expr
      else
        Lam (x, aux_rename body newn)
    ) in
  if is_fresh e nwn then
    aux_rename e nwn
  else
    let u' = gen_new_name nwn (gen_list e) in
    if is_fresh e u' then
      aux_rename e u'
    else
      failwith "Fatal Error: Implementation of the function gen_new_name in module untyped.ml is not correct."





(*
  λ x . (x (λ x . x))" an alpha-variant"
  λ z. ([x][x := z])
*)

(*
Ex. 6 - Implement Substitution: Def. 1.6.1.
  Hint Question: what is a good data structure for substitutions?

*)

(*
Def. Subst. σ : X -> T

[x := e]


fst : A * B -> A
snd : A * B -> B

*)

type subst = string * expr
(*
String = The old variable the is being replaces --> "x"
Expr = Is the new expression that will be implemented after the subst --> Var "y"
*)


let string_of_subst (s : subst) : string =
  match s with
  | (x, e) -> "[ " ^ x ^ " := " ^ (string_of_expr e) ^ " ]"

let fun_of_subst (s : subst) : string -> expr =
  failwith "TODO: Not implemented Yet."

let rec app_sub (s : subst) (t : expr) : expr =
  match t with
  | Var x as v -> if x = (fst s) then snd s else v
  | App(l,r) -> App (app_sub s l, app_sub s r)
  | Lam(x,body) ->
      if x = (fst s) then t
    else
      let z = gen_new_name x ([fst s] @ (List.rev_append (gen_list t) (gen_list (snd s)))) in
      let new_body = rename' body x z in
      assert (is_fresh body z) ;
      Lam (z, app_sub s new_body)



(* Beta reduction (Unrestrected)

Computation means this:
(λ x. s) t ->β s [x := t]

Closure/Compatibility...
if s ->β s' then s t ->β s' t (Comp. Left)

if t ->β t' then s t ->β s t' (Comp. Right)

if s ->β s' then λ x. s ->β λ x. s' (Comp. Under a Lambda)

*)

(* Interpreter... *)
let rec beta (t : expr) : expr =
  match t with
  | Var _ -> t
  | Lam (x, body) -> Lam (x, (beta body))
  | App(Lam(x, body), arg) ->
    let beta_subst = (x, arg) in (* [x := arg] *)
    app_sub beta_subst (beta body)
  | App (lhs, rhs) -> App (beta lhs, beta rhs)


(*
Primitive types:
Booleans: true/false
Integers: ... -1, 0, 1, ...
Functions: λ x. p
Pairs:     (s, t)

Final Product

Values   v, v' ::== x | true | false | λ x. p | Number
Expressions p,q,r ::== v | p q | if p then q else r | NativeOp | f | (s,t)
Functions f, g, h :: function f [x₁,..., xₙ] = { p }
where NativeOp can be:
It is usually useful to have some operators to each primitive type, e.g.,
    - and, or, xor, for booleans
    - addition, subtraction, for integers
    - recursion for functions (optional for now)

An Example of a program that should be able to define:

function is_even x = {
  if div_rest (x, 2) = 0
    then true
  else
    false
  }

*)
