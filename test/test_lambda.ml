open Syntax.Untyped
open Utils.Tools


(*
let e1 = Lam ("x", App (Var "x", Var "y"))
let e2 = Lam ("x", App (Var "x", Var "x"))
let e7 = Lam ("z", App (Var "z", Var "y"))
let e2 = Lam ("x", App (Var "x", Var "x"))
let e3 = Lam ("y", App (Var "y", Var "u"))
let e4 = Lam ("x", App (Var "x", Var "y"))
let e5 = App (Var "x", Var "x")
let e6 = App (Var "x", Var "y")
let id1 = Lam ("x", (Var "x"))
let id2 = Lam ("y", (Var "y"))
*)

(* λ x. x y let us rename y to z

let e1 = Lam ("x", App (Var "x", Var "y"))
let e2 = Lam ("x", App(Var "x", Var "y"))


let () =
  print_endline(string_of_expr(rename e1 "y" "z"));
   print_endline(string_of_expr(rename e1 "x" "z"))

*)

(* Ex. 5.(A). Compute the examples in Example 1.5.3.


special case
print_endline (string_of_bool (is_alpha f1 f3));

*)


let f3 = App (Lam ("x", App (Var "x", Lam ("z", App (Var "x", Var "y")))), Var "z")
let f2 = Lam ("x", (Lam("y", Var "x")))
let e3 = Lam ("x", App (Var "x", Var "y"))
let e3' = App (Var "x", Lam ("x", App (Var "x", Var "y")))

(*
e3 = (λ x. x y)[x := z] = e3
e3' = x (λ x . x y) [x := z] = z (λ x. x y)
*)

(* Renaming test *)
let() =
  print_string (string_of_expr (rename e3' "x"))



(*
let() =
  print_endline (list_to_string (fun x -> x) (gen_list e3));
  print_endline (list_to_string (fun x -> x) (gen_list f1));
  print_endline (string_of_bool (check_var "x1" (gen_list f1)));
  print_endline (gen_new_name "x7" (gen_list f1))

*)

let f1 = App (Lam ("x", App (Var "x", Lam ("z", App (Var "x", Var "y")))), Var "z")
let f2 = App (Lam ("u", App (Var "u", Lam ("z", App (Var "u", Var "y")))), Var "z")
let f3 = App (Lam ("z", App (Var "z", Lam ("x", App (Var "z", Var "y")))), Var "z")
let f4 = App (Lam ("y", App (Var "y", Lam ("z", App (Var "y", Var "y")))), Var "z")
let f5 = App (Lam ("z", App (Var "z", Lam ("z", App (Var "z", Var "y")))), Var "z")
let f6 = App (Lam ("u", App (Var "u", Lam ("z", App (Var "u", Var "y")))), Var "v")




  (*
  Does not run for some reason terminal freezes, and computer starts to fan out for all the things below
  print_endline (string_of_bool (is_alpha f1 f2));
  print_endline (string_of_bool (is_alpha f1 f4));
  print_endline (string_of_bool (is_alpha f1 f5));   (* should be false but is true*)
  print_endline (string_of_bool (is_alpha f1 f6));
  *)










(*
let () =
  print_endline (string_of_bool (is_alpha e1 e2));
  print_endline (string_of_bool (is_alpha e1 e7))
*)


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
