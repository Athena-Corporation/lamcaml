# LamCaml
This project is a high school research project conducted by ***Siddharth Sriganesh***, with the guidance of ***[Dr. Deivid Vale](https://deividrvale.github.io)***. Our aim is to implement a small, fully functional programming language in OCaml using the fundamental definitions of Lambda Calculus.

## Under the hood: A Powerful but Simple Model of Computation
Every programming language is built on a model of computation. The most common is the Turing Machine model which serves as a basis for languages such as Python, Java and C++. In this project, we have chosen to use λ-calculus as base for computation. λ-calculus is especially well versed for this project due to it being very simple to define and use, with only 3 things needed to use it 
  - Variables (x)
  - Abstractions(λx.M)
  - Application(MN)

Furthermore the lambda calculus is Turing Complete, allowing the user to simulate any algorithm. With the help of beta-reduction lambda expressions can go through step-by-step evaluation to execute an expression. Making Lambda Calculus itself a very small programming language.

## Implementation: Coupled with a Powerful Functional Programming Language
λ-calculus is functional because of its ability to use functions as first-class citizens (meaning a function can be assigned as an argument or variable, whilst also being able to be returned from a function). We have decided to use **[OCaml](https://ocaml.org)** in this project because it is a functional language, which employs Algebraic Data Types, Pattern Matching and Recursion which can be used to implement and define the Lambda Functions.

```ocaml
type expr =
  | Var of string
  | App of expr * expr
  | Lam of string * expr

let e = Var "x"
let r = App(Var "x", Var "y")
let g = Lam("x", App(Var "x", Var "y"))
```

## First Class Functions: The Mechanics of LamCaml
The past months have been used for creating functions that are able to transform lambda functions. We have implemented functions that allow the user to check for syntactical equality, subterms, free and bound variables.

**Equality function**
```ocaml
let rec equal_expr e1 e2 : bool =
  let open String in 
  match e1, e2 with
  | Var x, Var y -> equal x y
  | App (l1, r1), App (l2, r2) -> equal_expr l1 l2 && equal_expr r1 r2
  | Lam (x1, b1), Lam (x2, b2) -> equal x1 x2 && equal_expr b1 b2
  | _ -> false

```
To successfully show and print these through the terminal, we have encoded pretty printing functions to help print our expressions in the terminal. To show exactly where application or abstraction is occurring in the function. 

Terminal:
> x
>
>(x•y)
>
>λx.((x•y))

Renaming has also been implemented, as the user is able to rename any free variable in a lambda expression. Our function only allows the user to rename the free variable into the variable "u".The function generates new variables to avoid capturing an old variable by adding an integer to the end of the old variable.
```ocaml
let f = Lam("y", Var "x")
let f1 = Lam("z", App(Var "u", Var "u1"))

let() = 
  print_endline(string_of_expr(rename f "x"));
  print_endline(string_of_expr(rename f1 "u1"));
```
Terminal:
> λy.(u)
>
> λz.((u•u2))

Finally, the implementation of substitution can be seen in the project, as we have defined substitution as an Algebraic Data Type (which can be pretty printed) that shows the variable that will be substituted, followed by the expression that will replace it.

```ocaml
type subst = string * expr

let() = 
  print_endline(string_of_subst("x", Var "y"));
  print_endline(string_of_subst("x", App(Var"x", Var "y")));
```
Terminal:
> [ x := y ]
>
>[ x := (x•y) ]

The substitution function (app_sub) will take in 2 inputs (**subst** and an **expr**), and then recursively go through the expression to apply substitution. Of course, it also employs capture-avoiding substitution by implementing renaming, to rename the variables of functions that would allow the expression to change its meaning. (The type of renaming implemented is a variant of the former function, as it doesn't change the name to "u" but rather renames the variable in the given context).

```ocaml
let e = Var "x"
let r = App(Var "x", Var "y")
let g = Lam("x", App(Var "x", Var "y"))

let() = 
  print_endline(string_of_expr(app_sub ("y", g) (r)));
  print_endline(string_of_expr(app_sub ("y", e) (g)));;
```
Terminal:
> (x•λx.((x•y)))
>
> λx1.((x1•x))

The project is not fully completed as we have only been able to implement the above functions, renaming, substituion and computation. For the future of this project we hope to implement a very basic language. A vision for said langauge is shown below. 
```ocaml
function is_even x = {
  if div_rest (x, 2) = 0 then
    true
  else
    false
}
```
## Build Instructions

We use [opam](https://opam.ocaml.org/doc/Install.html) to build ONijn.
Make sure it is installed on your system before proceeding.

Here's the **dependency** list,
which can be installed using opam.

- **opam** v2.1.3 or higher.
  - See [opam](https://opam.ocaml.org/doc/Install.html) for installation instructions.
- **ocaml** Latest version
- **dune** Latest Version

#### Managing opam switches

If your current ``opam switch``
doesn't have OCaml v4.14.0 or higher,
we recommend creating a fresh ``opam switch``:

Run

```bash
dune build
```

#### Installing binaries locally

Run

```ocaml
dune install
```

## Bibliography
- **[OCaml Programming: Correct + Efficient + Beautiful,  Michael Ryan Clarkson](https://cs3110.github.io/textbook/cover.html)** 
- **[Type Theory and Formal Proof: An Intorduction,  Rob Nederpelt and Hermen Geuvers](https://anggtwu.net/tmp/nederpelt_geuvers__type_theory_and_formal_proof_an_introduction.pdf)** 
