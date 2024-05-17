(* Write some SUCCESS test cases for `infer` *)
let infer_tests : ((ctx * exp) * tp) list = [
  (([("x",Int);("y",Int)],Primop(Plus,[Var "x"; Var "y"])),
   Int);
  
  (([("x",Bool);("y",Bool)],Primop(Equals,[Var "x"; Var "y"])),
   Bool);

  (([],I 1), Int);
  
  (([("x",Int)],Primop (Plus, [Var "x"; I 5])), Int);
  
  (([("x",Bool);("y",Int)],If(Var "x", Var "y", Var "y")), Int);
  
  (([("z",Bool)],Fn([("z", Bool);], Var "z")), Arrow([Bool],Bool));
  
  (([],Fn([("z", Bool);], Var "z")), Arrow([Bool],Bool));
  
  (([],Fn([], B true)), Arrow([],Bool));
  
  (([],Fn([("z", Bool);("x",Bool)], Var "z")), Arrow([Bool;Bool],Bool));
  
  (([("z",Bool);("b",Int)],Fn([("z", Bool);], Fn([("b", Int);], Var("b")))), Arrow([Bool],Arrow([Int],Int)));
  
  (([],Primop(Negate,[I 5])), Int);
  
  (([],Rec(("a", Int, I(3)))),Int);
  
  (([],Rec(("a", Int, Primop(Plus,[Var "a";Var "a"])))),Int);
  
  (([],Rec(("a", Int, Primop(Plus,[Var "a";I 3])))),Int);
  
  (([],Apply(Fn([("x",Int)],Var "x"),[I 3])),Int);
  
  (([],Apply(Fn([("x",Int);("y",Int)],Var "x"),[I 3;I 3])),Int);
  
  (([],Apply(Fn([],B true),[])),Bool);
  
  (([], ex4), Bool);
  
  (([], ex6), Int);
  
  (([],Rec ("f", Arrow ([Int], Bool), Var "f")),Arrow ([Int], Bool))
  
  

]

(* Q1: `infer_op` - Type Inference in Primitive Operations *)

(* [infer_op] returns the type of a primitive operation *)
let infer_op (op : primop) (ts : tp list) : tp =
  match (op,ts) with 
  |(Plus,z) -> 
      (match z with
       |[x] when List.length z <> 2 -> raise ArityMismatch 
       |[x] -> Int
       |[x;y]-> if x<>y then raise TypeMismatch else Int)
        
  |(Minus,z) -> 
      if List.length z <> 2 then raise ArityMismatch 
      else (match z with
          |[x]-> raise ArityMismatch
          |[x;y]-> if x<>y then raise TypeMismatch else Int)
        
  |(Times,z) -> 
      if List.length z <> 2 then raise ArityMismatch 
      else (match z with
          |[x] -> raise ArityMismatch
          |[x;y]-> if x<>y then raise TypeMismatch else Int)
        
  |(Equals,z) -> 
      if List.length z <> 2 then raise ArityMismatch
      else (match z with
          |[x;y]-> if x<>y then raise TypeMismatch else Bool)
        
  |(LessThan,z) ->
      if List.length z <> 2 then raise ArityMismatch 
      else (match z with
          |[x;y]-> if x<>y then raise TypeMismatch else Bool)
        
  |(Negate,z)-> 
      if List.length z = 2 then raise ArityMismatch
      else (match z with 
          |[x] -> (match x with 
              |Int->Int
              |_ -> raise TypeMismatch ))
      
  
(* Q2: `infer` - General Type Inference *)

let rec comparelists lst1 lst2 = 
  if List.length lst1 <> List.length lst2 then false
  else
    match lst1,lst2 with
    |[],[]-> true
    |x::xs,y::ys -> if x <> y then false else comparelists xs ys

(* [infer] returns the type of an expression in a context *)
let rec infer (ctx : ctx) (e : exp) : tp =
  match e with
  | I _ -> Int
  | B _ -> Bool
  | Var x -> begin match List.assoc_opt x ctx with
      |None -> raise FreeVariable
      |Some t -> t
    end
  | Primop (op, es) -> 
      if op = Negate then infer_op op ([infer ctx (List.hd es)]) 
      else 
        begin match es with
          |[x;y]-> infer_op op ((infer ctx x)::[infer ctx y])
        end   
    
  | If (cond, e1, e2) -> 
      begin match infer ctx cond with
        |Bool->
            begin match infer ctx e1, infer ctx e2 with
              |t1,t2 when t1 = t2 -> t1
              |_-> raise TypeMismatch
            end
        |_-> raise TypeMismatch
      end
  | Let (x, e1, e2) -> let t = infer ctx e1 in infer ((x,t)::ctx) e2
        
  | Fn (xs, e') -> 
      let xstype = List.map snd xs in
      let e'type = infer (xs @ ctx) e' in 
      Arrow(xstype,e'type)
  
  | Apply (e', args) -> 
      if List.length args < 2 then 
        begin match infer ctx e' with
          |Arrow(t,t')-> 
              if List.length args = 1 then
                let t_args = infer ctx (List.hd args) in
                if t_args <> List.hd t then raise TypeMismatch 
                else t' 
              else t' 
          |_ -> raise TypeMismatch
        end 
        
      else (*more than one arguments*) 
        begin match infer ctx e' with 
          |Arrow(t,t')-> 
              if List.length t <> List.length args then raise ArityMismatch 
              else let argstype =
                     let rec convertargs acc newargs =
                       match newargs with
                       |[]->[]
                       |x::xs -> infer ctx x :: convertargs acc xs 
                     in convertargs [] args in
                if comparelists argstype t then t'
                else raise TypeMismatch 
        end
  
  | Rec (f, t, e') -> 
      if t <> infer [(f,t)] e' then raise TypeMismatch
      else begin match t with
        |Arrow(_) when t <> infer [(f,t)] e' -> raise TypeMismatch 
        |_-> infer [(f,t)] e'
      end
      
