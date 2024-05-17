(** Question 1 *)

(* TODO: Add test cases. *)
let collect_variables_tests : (formula * Variable_set.t) list = [
  ((Disjunction (Disjunction (Variable "a1", Variable "a2"), Variable "a3")),
   Variable_set.singleton "a1"
   |> Variable_set.add "a2"
   |> Variable_set.add "a3" );
  
  (Negation (Negation (Variable "a")),Variable_set.singleton "a");
  (Conjunction (Disjunction (Variable "a", Negation (Variable "b")),
                Variable "c"),
   Variable_set.singleton "a"
   |> Variable_set.add "b"
   |> Variable_set.add "c" ); 
]

(* TODO: Implement the function. *)
let collect_variables (formula : formula) : Variable_set.t =
  let rec collect_helper (formula:formula) (acc:Variable_set.t) : Variable_set.t=
    match formula with 
    | Variable v ->  Variable_set.add v acc
    | Disjunction (a,b) -> collect_helper a (collect_helper b acc)
    | Conjunction (c,d) -> collect_helper c (collect_helper d acc)
    | Negation (e) -> collect_helper e acc
  in collect_helper formula (Variable_set.empty)

    
    
(** Question 2 *)

(* TODO: Add test cases. *)
let eval_success_tests : ((truth_assignment * formula) * bool) list = [
  ((Variable_map.singleton "x" true
    |> Variable_map.add "y" false
    |> Variable_map.add "z" false
   ,Disjunction (Disjunction (Variable "x", Variable "y"), Variable "z"))
  ,true); 
  
  ((Variable_map.singleton "a" true
    |> Variable_map.add "b" false
    |> Variable_map.add "c" false
   ,Conjunction (Disjunction (Variable "a", Negation (Variable "b")),
                 Variable "c")) 
  ,false);
  
  ((Variable_map.singleton "x" false
    |> Variable_map.add "y" false
    |> Variable_map.add "z" false
   ,Disjunction (Disjunction (Variable "x", Variable "y"), Variable "z"))
  ,false); 
  
  ((Variable_map.singleton "a" false
   ,Negation(Negation (Variable "a")))
  ,false);
  
  ((Variable_map.singleton "a" true
    |> Variable_map.add "b" false 
   ,Disjunction(Variable "a",Variable "b")) 
  ,true);
  
  ((Variable_map.singleton "a" false
   ,Negation(Negation(Negation (Variable "a"))))
  ,true); 
  
  ((Variable_map.singleton "a" false, Disjunction(Variable "a", Variable "a"))
  ,false);
  
  ((Variable_map.singleton "a" false
    |> Variable_map.add "b" true
   ,Disjunction(Conjunction(Variable "a", Variable "b" ),Variable "b"))
    
  ,true);
  
]


(* TODO: Add test cases. *)
let eval_failure_tests : ((truth_assignment * formula) * exn) list = [ 
  
  ((Variable_map.singleton "a" true
    |> Variable_map.add "b" false 
   ,Conjunction (Disjunction (Variable "a", Negation (Variable "b")),
                 Variable "c")) 
  ,Unassigned_variable "c");
  
  ((Variable_map.singleton "b" true 
   ,Disjunction(Variable "a",Variable "b")) 
  ,Unassigned_variable "a");
  
  
]

(* TODO: Implement the function. *)
let rec eval (state : truth_assignment) (formula : formula)  =
  match formula with
  | Disjunction (a,b) -> (eval state a || eval state b) && (eval state b || eval state a) 
  | Conjunction (c,d) -> (eval state c && eval state d) || (eval state d && eval state c)
  | Negation (e) -> not (eval state e)
  | Variable v -> 
      match Variable_map.find_opt v state with
      |Some r -> r
      |None -> raise (Unassigned_variable v) 
  
(** Question 3 *)

(* TODO: Add test cases. *)
let find_satisfying_assignment_tests : (formula * truth_assignment option) list = [
  (Negation(Variable "a"),Some (Variable_map.singleton "a" false));
  
  (Conjunction(Conjunction(Negation(Variable "a"), Variable "b"),Variable "b")
  ,Some (Variable_map.singleton "a" false |> Variable_map.add "b" true));
  
  (Conjunction (Variable "x", Negation (Variable "x")),None)
  
]

  
(* TODO: Implement the function. *)
let find_satisfying_assignment (formula : formula) =
  
  let rec find_helper (vars_list) (new_a:truth_assignment):truth_assignment  = 
    
    match vars_list with 
    |[] when not (eval new_a formula)-> raise Unsatisfiable_formula 
    |[] -> new_a
                                          
    |x::xs -> try find_helper xs (Variable_map.add x true new_a) with
      |Unsatisfiable_formula -> try find_helper xs (Variable_map.add x false new_a) with
        |Unsatisfiable_formula -> find_helper xs (Variable_map.add x true new_a)
    
  in find_helper (Variable_set.elements(collect_variables formula)) (Variable_map.empty) 
  
