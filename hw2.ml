(* Question 1 *)

(* TODO: Write a good set of tests for {!q1a_nat_of_int}. *)
let q1a_nat_of_int_tests : (int * nat) list = [
  (0,Z); (1, (S Z)); (2, S (S Z))]

(* TODO:  Implement {!q1a_nat_of_int} using a tail-recursive helper. *)
let rec q1a_helper (n : int) (acc : nat) : nat = 
  match n with | 0 -> acc | _ -> q1a_helper (n-1) (S acc) 
  
let q1a_nat_of_int (n : int) : nat = 
  match n with | 0 -> Z | _ -> S (q1a_helper (n-1) Z)

(* TODO: Write a good set of tests for {!q1b_int_of_nat}. *)
let q1b_int_of_nat_tests : (nat * int) list = [
  (Z,0);((S Z),1);(S (S Z),2);(S (S (S Z)),3)]

(* TODO:  Implement {!q1b_int_of_nat} using a tail-recursive helper. *)
let rec q1b_helper (n : nat) (acc : int) : int = 
  match n with | Z -> acc | S n -> q1b_helper n (acc+1)
  
let q1b_int_of_nat (n : nat) : int = 
  match n with | Z -> 0 | _ -> q1b_helper n 0

(* TODO: Write a good set of tests for {!q1c_add}. *)
let q1c_add_tests : ((nat * nat) * nat) list = [ 
  ((Z,Z),Z); (((S Z),Z),S Z); ]

(* TODO: Implement {!q1c_add}. *)
let rec q1c_add (n : nat) (m : nat) : nat = 
  match n with | Z -> m | S p -> S (q1c_add p m)


(* Question 2 *)

(* TODO: Implement {!q2a_neg}. *)
let q2a_neg (e : exp) : exp = 
  Times (Const(-1.0),e)

(* TODO: Implement {!q2b_minus}. *)
let q2b_minus (e1 : exp) (e2 : exp) : exp = Plus (e1,q2a_neg e2)

(* TODO: Implement {!q2c_pow}. *)
let rec q2c_helper (e : exp) (n : nat) (acc : exp) : exp =  
  match n with | Z -> acc | S l -> q2c_helper e l (Times(e,acc))
  

let q2c_pow (e1 : exp) (p : nat) : exp = 
  match p with | Z -> Const(1.0) | _ -> q2c_helper e1 p (Const(1.0))


(* Question 3 *)

(* TODO: Write a good set of tests for {!eval}. *)
let eval_tests : ((float * exp) * float) list = [
  ((1.0,Plus(Var,Var)),2.0); ((2.0,Times(Var,Var)),4.0);
  ((2.0,Div(Var,Const(1.0))),2.0); ((3.0,Plus(Const(1.0),Const(1.0))),2.0)
]

    (*let eval_helper (b : float) (e : exp) (acc : float) : float =*)
  
  
(* TODO: Implement {!eval}. *)
let rec eval (a : float) (e : exp) : float = 
  match e with | Var -> a
               | Const (num) -> num
               | Plus (e1,e2) -> (eval a e1) +. (eval a e2)
               | Times (e1,e2) -> (eval a e1) *. (eval a e2)
               | Div (e1,e2) -> (eval a e1) /. (eval a e2)
  
  
(* Question 4 *)

(* TODO: Write a good set of tests for {!diff_tests}. *)
let diff_tests : (exp * exp) list = [
  (Plus(Var,Const(1.0)), Plus(Const(1.0), Const(0.0)));
  (Times(Var,Const(2.0)),Plus (Times (Const(1.0), Const(2.0)), Times (Var, Const(0.0))));
  (Var,Const(1.0));
  (Const(1.0),Const(0.0));
  (Div(Var,Const(2.0)),Div
     (Plus (Times (Const 1., Const 2.),
            Times (Times (Var, Const 0.), Const (-1.))),
      Times (Const 2., Const 2.)))
  
]

(* TODO: Implement {!diff}. *)
let rec diff (e : exp) : exp = 
  match e with | Plus (e1,e2) -> Plus(diff e1,diff e2)
               | Times (e1,e2) -> Plus(Times(diff e1,e2),Times(e1,diff e2))
               | Const(num) -> Const(0.0)
               | Var -> Const(1.0) 
               | Div (e1,e2) -> Div(Plus(Times(diff e1,e2),Times(Times(e1,diff e2),Const(-1.0))),Times(e2,e2))
                                 
  
  
  
  
