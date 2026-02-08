open Syntax.Lamcaml

let f = Lam("y", Var "u")
let f1 = Lam("z", App(Var "u", Var "u1"))
let f2 = App(Lam("x", Var "x"), Var "y")
let f3 = Lam ("y", App (Lam("x", Var "x"), Var "y"))

let f4 = Lam( "y", App((Lam( "x", Var "x"), Var "y")))
let e = Var "x"
let r = App(Var "x", Var "y")
let g = Lam("x", App(Var "x", Var "y"))


let() =  print_endline((string_of_expr (rename f "u")));