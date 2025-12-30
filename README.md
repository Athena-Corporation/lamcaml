# LamCaml
This project is a high school research project conducted by ***Siddharth Sriganesh***, with the guidance of ***Dr. Deivid Vale***. Our aim is to implement a small, fully functional programming language in OCaml using the fundamental definitions of Lambda Calculus.

# Why Lambda Calculus
Lambda Calculus is essential for demonstrating how the software logic of a computer evaluates a program. By learning and applying Lambda Calculus, one can build an entire universe of logic from just two or three basic expressions.

Using OCaml as the implementation language is ideal because OCaml incorporates the core definitions of Lambda Calculus into its own syntax. You can see the use of Abstraction and Application at every level of the language. Furthermore, OCaml is a functional programming language that allows the user to define Algebraic Data Types, utilize powerful pattern matching, and employ recursion, making it the perfect choice for building a compiler or interpreter.

# How to Use 
Each Lambda Expression is to be defined with our internal OCaml logic. 
>For example: 
>
>> let e = App(Lam("x", Var "x"), Var "y")
To execute and test the logic, utilize the core functions defined in the untyped.ml file. These tools allow you to evalute, rename, subsitute and pretty print functions. 

To use the project, simply install Dune and execute your functions by running dune test in the terminal. This will automatically compile your code and run any predefined test cases to verify your Lambda Calculus implementation.

# Resources 
Type Theory and Formal Proof: An Intorduction,  Rob Nederpelt and Hermen Geuvers
OCaml Programming: Correct + Efficient + Beautiful,  Michael Ryan Clarkson: https://cs3110.github.io/textbook/cover.html
