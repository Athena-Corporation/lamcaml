open Syntax.Lamcaml
open Utils.Tools

(* let f x y =
  let z = 10 + 10 in
  (* side-effect *)
  let () = print_endline "I got computed" in
  x + y *)

(* let t1 = BinOp(Geq, BinOp(Add, Num 2, Num 3), Num 4)  (* True *)
let t2 = If(BinOp(Geq, Num 1, Num 2), Num 10, Num 20) (* 20 *)
let t3 = App(Lam("x", Var "x"), Num(6)) (* 6 *)
let t4 = App(Lam("x", BinOp(Add, Var "x", Num 1)), Var "x") (* 16 *)
let t5 = App(Lam("x", Lam("y", Var "x")), Var "y") (* λu.y *)
let t6 = UnaOp(Inv, App(Lam("x", Var "x"), Num(-9))) 9 *)

let e = App(Lam("y", BinOp(Add, Var "y", Num 1)), Var "x" )
let t5 = App(Lam("x", Lam("y", Var "x")), Var "y") (* λu.y *)
let t2 = If(BinOp(Geq, Num 6, Num 2), Num 10, Num 20) (* 20 *)

let()  =
  print_endline (string_of_expr  (e));
  print_endline (string_of_expr (t5));
  print_endline (string_of_expr (beta(t2)));
