Tasks for meeting on 12/1

- Readt Beta Reduction
- Analyze Subst functions to see how it works

(& opam env) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ }




  - ⭕ emphasize that every programming language has a model of computation as a basis
  - ⭕ tell that in this project: we choose to use the λ-calculus
  - ⭕ why: (1. because it is very simple to define and to reason about, 2. it is Turing complete (what it means))

Lambda Calculus is essential for demonstrating how the software logic of a computer evaluates a program.
By learning and applying Lambda Calculus, one can build an entire universe of logic from just two or three basic expressions.









  - ⭕ since the λ-calculus by itself is very functional (because it only has functions as first-class citizens) it makes sense to implement it in a functional language
  - ⭕ in this project we haven chosen ocaml
  - ⭕ why?

Using OCaml as the implementation language is ideal because OCaml incorporates the core definitions of Lambda Calculus into its own syntax. You can see the use of Abstraction and Application at every level of the language. {remove}

Furthermore, OCaml is a functional programming language that allows the user to define Algebraic Data Types, utilize powerful pattern matching, and employ recursion, making it the perfect choice for building a compiler or interpreter.



- ⭕ "What we have"
  - ⭕ describe the state of the project: broad overview of the "features" of the language that has been implemented up to now
      - proper deal with renaming and alpha equivalence
      - we can actually comptue with our notion of λ-calculus
  - ⭕ give examples of how programs are described in our setting



  - ⭕ show how to **compute** with our language
- ⭕ justify adding more features to the language because since it is in a "pure" state, actually programming with this language is akin to programming in a low-level language like assembly, so the next step is to make it more high-level...
what we will have by the end    