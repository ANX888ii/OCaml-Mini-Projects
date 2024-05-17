(* Question 1: Manhattan Distance *)
(* TODO: Write a good set of tests for distance. *)
let distance_tests = [
  (((1,1),(1,1)),0);
  (((0,0),(1,1)),2);
  (((1,1),(0,0)),2);
  (((-1,-1),(1,1)),4);
  (((-1,-1),(-2,-2)),2);
  (* Your test cases go here *)
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let distance (x1, y1) (x2, y2) = 
  abs(x2 - x1) + abs(y2 - y1) 



(* Question 2: Binomial *)
(* TODO: Write your own tests for the binomial function.
         See the provided test for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0,0),1);
  ((1,0),1);
  ((1,1),1);
  ((2,1),2);

]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec factorial i = 
  if i<=1 then 1 else i * factorial (i-1)
                              
let binomial n k =
  (factorial n) / ((factorial k) * (factorial (n-k)))



(* Question 3: Lucas Numbers *)

(* TODO: Write a good set of tests for lucas_tests. *)
let lucas_tests = [
  (0,2);
  (1,1);
  (2,3);
  (3,4);
  
]

(* TODO: Implement a tail-recursive helper lucas_helper. *) 
let rec lucas_helper (i:int) (partial_a:int) (partial_b:int) (n:int) :int  =
  if i = n then partial_a
  else lucas_helper (i+1) partial_b (partial_a+partial_b) n

(* TODO: Implement lucas that calls the previous function. *)
let lucas (n:int) :int = 
  if n=0 then 2 
  else if n=1 then 1
  else lucas_helper 0 2 1 (n-1) + lucas_helper 0 2 1 (n-2)
    
  


