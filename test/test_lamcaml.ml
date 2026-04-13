open Syntax.Lamcaml
open Utils.Tools


let t1 = BinOp(Geq, BinOp(Add, Num 2, Num 3), Num 4)  (* True *)
let t2 = If(BinOp(Geq, Num 1, Num 2), Num 10, Num 20) (* 20 *)
let t3 = App(Lam("x", Var "x"), Num(6)) (* 6 *)
let t4 = App(Lam("x", BinOp(Add, Var "x", Num 1)), BinOp(Add, Num 10, Num 5))  (* 16 *)
let t5 = App(Lam("x", Lam("y", Var "x")), Var "y") (* λu.y *)
let t6 = UnaOp(Inv, App(Lam("x", Var "x"), Num(-9))) (* 9 *)

let()  = 
  print_endline(string_of_expr(beta t1));
  print_endline(string_of_expr(beta t2));
  print_endline(string_of_expr(beta t3));
  print_endline(string_of_expr(beta t4));
  print_endline(string_of_expr(beta t5));
  print_endline(string_of_expr(beta t6))
  
  



