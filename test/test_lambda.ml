open Syntax.Untyped
open Utils.Lsps

let e1 = Lam ("x", App (Var "x", Var "y"))
let e2 = Lam ("x", App (Var "x", Var "x"))
let e7 = Lam ("z", App (Var "z", Var "y"))

(*
let e2 = Lam ("x", App (Var "x", Var "x"))
let e3 = Lam ("y", App (Var "y", Var "u"))
let e4 = Lam ("x", App (Var "x", Var "y"))
let e5 = App (Var "x", Var "x")
let e6 = App (Var "x", Var "y")
let id1 = Lam ("x", (Var "x"))
let id2 = Lam ("y", (Var "y"))
*)


let () =
  print_endline (string_of_bool (is_alpha e1 e2));
  print_endline (string_of_bool (is_alpha e1 e7))



(*
let () =
  print_endline (string_of_bool (combinator e1));
  print_endline (string_of_bool (combinator e2))
*)

(*
let() = 
  print_endline( "Free: " ^ String.concat "," (free_vars e1));
  print_endline( "Bound: " ^ String.concat "," (bound_vars e1))
*)

(*
let () =
  string_of_expr e1  |> print_endline ;
  string_of_expr id2 |> print_endline
string_of_bool (equal_expr id1 id2) |> print_endline *)



(* let () =
  print_endline (string_of_bool (equal_expr e3 e2));
  print_endline (string_of_bool (equal_expr e2 e1));
  print_endline (string_of_bool (equal_expr e4 e1));;

let () =
  print_list (sub_term e1);
  print_list (sub_term e2);
  print_list (sub_term e3)


let () =
  print_endline (string_of_bool (is_subterm e5 e1));
  print_endline (string_of_bool (is_subterm e6 e1));
  print_endline (string_of_bool (is_proper_subterm e1 e2));
  print_endline (string_of_bool (is_proper_subterm e1 e5)) *)
