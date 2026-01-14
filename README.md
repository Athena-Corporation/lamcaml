# LamCaml
This project is a high school research project conducted by ***Siddharth Sriganesh***, with the guidance of ***[Dr. Deivid Vale](https://deividrvale.github.io)***. Our aim is to implement a small, fully functional programming language in OCaml using the fundamental definitions of Lambda Calculus.

## Under the hood: A Powerful but Simple Model of Computation

  - ⭕ emphasize that every programming language has a model of computation as a basis
  - ⭕ tell that in this project: we choose to use the λ-calculus
  - ⭕ why: (1. because it is very simple to define and to reason about, 2. it is Turing complete (what it means))

Lambda Calculus is essential for demonstrating how the software logic of a computer evaluates a program.
By learning and applying Lambda Calculus, one can build an entire universe of logic from just two or three basic expressions.

### Implementing: Coupled with a Powerful Functional Programming Language
  - ⭕ since the λ-calculus by itself is very functional (because it only has functions as first-class citizens) it makes sense to implement it in a functional language
  - ⭕ in this project we haven chosen ocaml
  - ⭕ why?

Using OCaml as the implementation language is ideal because OCaml incorporates the core definitions of Lambda Calculus into its own syntax. You can see the use of Abstraction and Application at every level of the language. {remove}

Furthermore, OCaml is a functional programming language that allows the user to define Algebraic Data Types, utilize powerful pattern matching, and employ recursion, making it the perfect choice for building a compiler or interpreter.

## X

- ⭕ "What we have"
  - ⭕ describe the state of the project: broad overview of the "features" of the language that has been implemented up to now
      - proper deal with renaming and alpha equivalence
      - we can actually comptue with our notion of λ-calculus
  - ⭕ give examples of how programs are described in our setting

    - For example: HINT (implement booleans)
    ```ocaml
    let e = App(Lam("x", Var "x"), Var "y")
    ```



  - ⭕ show how to **compute** with our language
- ⭕ justify adding more features to the language because since it is in a "pure" state, actually programming with this language is akin to programming in a low-level language like assembly, so the next step is to make it more high-level...
what we will have by the end

### Build Instructions

  - ⭕ if one wants to build this project locally...

Each Lambda Expression is to be defined with our internal OCaml.


To execute and test the logic, utilize the core functions defined in the untyped.ml file. These tools allow you to evalute, rename, subsitute and pretty print functions.

To use the project, simply install Dune and execute your functions by running dune test in the terminal. This will automatically compile your code and run any predefined test cases to verify our Lambda Calculus implementation.

### Running example programs

  - ⭕ describe how one can run the predefined tests
  - ⭕ describe how one can run their own programs in our language

## Bibliography
- Type Theory and Formal Proof: An Intorduction,  Rob Nederpelt and Hermen Geuvers
- OCaml Programming: Correct + Efficient + Beautiful,  Michael Ryan Clarkson: https://cs3110.github.io/textbook/cover.html
