(** [list_in] checks and states if a variable is inside a list. Used to check if a variable is present in the  variables found in a give [expr] *)
let rec list_in var (blocked_names: string list) : bool =
  match (List.find_opt (String.equal var) blocked_names) with
  | None -> false
  | Some _ -> true

(**
[gen_new_name] will generate a new variable to add into an [expr]; occurs when a variable is not
[Fresh] in an [expr].
*)
let rec gen_new_name (base_name: string) (blocked_names: string list) : string =
  if list_in base_name blocked_names then
    let rec gen_new_name_aux n =
      let new_name = base_name ^ string_of_int n in
      if list_in new_name blocked_names then gen_new_name_aux (n + 1)
      else new_name
    in
    gen_new_name_aux 1
  else
    base_name
