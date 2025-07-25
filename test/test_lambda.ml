(*Create lambda calculas function/implement it here
  What it should do: 
   - Have Boolean logic, true or false
   - Have if statements 
*)

let ltrue x y = x 
let lfalse x y = y 
let lif b z w = b z w 

let () = print_endline (lif ltrue "Yes" "No")
let () = print_endline (lif lfalse "Yes" "No")
