(* SECTION 1 *)

(* Question 1.1 *)
let rec take (n : int) (s : 'a stream) : 'a list =
  if n = 0 then [] 
  else s.head :: take (n-1) (force s.tail)

(* Question 1.2 *)
let rec lucas1 = {
  head = 2;
  tail = Susp (fun() -> lucas2); 
}
and lucas2 = {
  head = 1;
  tail = Susp (fun() -> add_streams lucas1 lucas2) } 

(* Question 1.3 *)
let rec unfold (f : 'a -> ('b * 'a)) (seed : 'a) : 'b stream = 
  let (c,d) = f seed in 
  {head=c;
   tail = Susp (fun () -> str_map fst (iterate (fun (_,b)-> f b) (f d)))}
  
  
(* Question 1.4 *)
let lucas : int stream = unfold (fun(a,b)->(a,(b,a+b))) (2,1)

(* SECTION 2 *)

(* Question 2.1 *)
let rec scale (s1 : int stream) (n : int) : int stream = 
  {head = s1.head * n;
   tail = Susp (fun () -> scale (force s1.tail) n)} 

let rec merge (s1 : 'a stream) (s2 : 'a stream) : 'a stream = 
  {head = (if s1.head<s2.head then s1.head else s2.head);
   tail = (if s1.head<s2.head then Susp(fun()->merge (force s1.tail) s2) 
           else if s1.head>s2.head then Susp(fun()->merge s1 (force s2.tail))
           else Susp(fun()->merge (force s1.tail) (force s2.tail)))}
  
(* Question 2.2 *)
let rec s = { 
  head = 1;
  tail = Susp(fun() -> merge (merge (scale s 2) (scale s 3)) (scale s 5)) }