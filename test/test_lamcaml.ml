open Syntax.Lamcaml
open Utils.Tools


let f1 = Bool(true)
let f2 = App(Var"u", Var "y")

let f4 = App(Lam("x", Var "x"), Var "y")
let e = If(BinOp(Geq, Num 2, Num 1), Var "x", Var "y")
let u = If(UnaOp(Not,(BinOp(Geq, Var "x", Num 1))), App(Var "x", Lam("x", Var "x")), Var "y")
let f = BinOp(Add, Num 6, Num 7)
let y = BinOp(Add, UnaOp(Inv,Num 6), Num 7)

(* True [x := y] = True *)

let()  =
  print_endline ((rename u "x" "z") |> string_of_expr)
  (* print_endline(is_fresh f1 "x" |> string_of_bool) *)
  (* print_endline(string_of_expr(beta e));
  print_endline(string_of_expr(beta u));
  print_endline(string_of_expr(beta f));
  print_endline(string_of_expr(beta y)) *)


