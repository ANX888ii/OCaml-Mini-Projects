
let ex8 : exp = Rec ("x", Bool, Var "x")  
let ex9 : exp = Rec ("x", Int,  Primop (Times, [Var "x"; Var "y"])) 
let ex10: exp = Apply (Var "x", [Primop (Times, [Var "y"; Var "y"]);
                                 Primop (Minus, [Var "y"; Var "y"])])
let ex11: exp = Apply (Var "y", [Primop (Times, [Var "y"; Var "y"]);
                                 Primop (Minus, [Var "y"; Var "y"])])
    
(**To DO: Write a good set of tests for free_variables **)
let free_variables_tests = [
  (* An example test case.
     Note that you are *only* required to write tests for Let, Rec, Fn, and Apply!
  *)
  (Let ("x", I 1, I 5), []); (ex1,[]); (ex2,[]); (ex3,[]);
  (ex8,[]);(ex9,["y"]); (ex10,["x";"y"]); (ex11,["y"]);

  (Fn(["x",Int],Var"c"),["c"])


]

(* TODO: Implement the missing cases of free_variables. *)
let rec free_variables : exp -> name list =
  (* Taking unions of lists.
     If the lists are in fact sets (all elements are unique),
     then the result will also be a set.
  *)
  let union l1 l2 = delete l2 l1 @ l2 in
  let union_fvs es =
    List.fold_left (fun acc exp -> union acc (free_variables exp)) [] es
  in
  function
  | Var y -> [y]
  | I _ | B _ -> []
  | If(e, e1, e2) -> union_fvs [e; e1; e2]
  | Primop (_, args) -> union_fvs args
  | Fn (xs, e) -> delete (List.map fst xs) (union_fvs [e])
  | Rec (x, _, e) -> delete [x] (union_fvs [e]) 
  | Let (x, e1, e2) -> delete [x] (union_fvs [e1; e2])
  | Apply (e, es) -> delete (union_fvs es) (union_fvs [e]) @ union_fvs es
      
  
(* TODO: Write a good set of tests for subst. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let subst_tests : (((exp * name) * exp) * exp) list = [
  (* An example test case. If you have trouble writing test cases of the
     proper form, you can try copying this one and modifying it.
     Note that you are *only* required to write tests for Rec, Fn, and Apply!
  *)
  (((I 1, "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
    Let ("y", I 2, Primop (Plus, [Var "y"; Var "x"]))),
   (* let y = 2 in y + 1 *)
   Let ("y", I 2, Primop (Plus, [Var "y"; I 1])));
  
  (((Var "y", "x"), (* [y/x] *)
    (* let y = x+3 in y + x *)
    Let ("y", Primop(Plus,[Var "x"; I 3]), Primop (Plus, [Var "y"; Var "x"]))),
   (* let z = y+3 in z+y *)
   Let ("z", Primop(Plus,[Var "y"; I 3]), Primop (Plus, [Var "z"; Var "y"])));
  
  (((I 1, "x"), (* [1/x] *)
    (* let y = 2 in y + x *)
    Let ("x", Primop(Plus, [Var "x"; I 1]), Primop (Plus, [Var "x"; I 2]))),
   (* let y = 2 in y + 1 *) 
   Let ("x", Primop(Plus, [I 1; I 1]), Primop (Plus, [Var "x"; I 2])));
  
  (((I 1, "x"), ex2), ex2);
  
  (((I 1, "x"), ex1), 
   
   Fn ([("x", Int); ("y", Int)],
       Primop (Plus,
               [Primop (Times, [Var "x"; Var "x"]);
                Primop (Times, [Var "y"; Var "y"])])));
  
  (((Var "y", "x"), ex1), 
   
   Fn ([("x", Int); ("y", Int)],
       Primop (Plus,
               [Primop (Times, [Var "x"; Var "x"]);
                Primop (Times, [Var "y"; Var "y"])])));
  
  (((Var "y", "x"), ex1), 
   
   Fn ([("x", Int); ("y", Int)],
       Primop (Plus,
               [Primop (Times, [Var "x"; Var "x"]);
                Primop (Times, [Var "y"; Var "y"])])));
  
  (((I 1, "x"), ex9), ex9);
  
  
  (((I 1, "x"), ex10), Apply (I 1, [Primop (Times, [Var "y"; Var "y"]);
                                    Primop (Minus, [Var "y"; Var "y"])]));
  
  (((I 1, "x"), ex11), ex11);
  
  (((Var "y", "x"), Rec("y",Int,Primop(Plus,[Var "y";Var"x"]))), 
   Rec("z",Int,Primop(Plus,[Var "z";Var"y"])));
  
  (((Var "y", "x"), Fn (["y",Int],Primop(Plus,[Var"y";Var"x"]))),
   Fn (["z",Int],Primop(Plus,[Var"z";Var"y"])));
  
  (((I 1, "x"), Apply(Var "z",[Primop (Plus, [Var "z"; Var "z"])])), 
   Apply(Var "z",[Primop (Plus, [Var "z"; Var "z"])]) );
  
  (((I 1, "x"), Apply(Var "x",[])), 
   Apply(I 1,[]));
  
  (((Var "y", "x"), Apply(Var "y",[Primop(Plus,[Var "x";Var"x"])])), 
   Apply(Var "y",[Primop(Plus,[Var "y";Var"y"])] ));
  
  (((Var "y", "x"), Apply(Var "y",[Primop(Plus,[Var "x";Var"x"]);Primop(Plus,[Var "y";Var"y"])])), 
   Apply(Var "y",[Primop(Plus,[Var "y";Var"y"]);Primop(Plus,[Var "y";Var"y"])]));

  (((I 1, "x"), Apply(Var "x",[])), 
   Apply(I 1,[]));
  
  (((Var "y", "x"), Apply(Var "x",[Primop(Plus,[Var "x";Var"x"]);Primop(Minus,[Var "x";Var"x"])])), 
   
   Apply(Var "y",[Primop(Plus,[Var "y";Var"y"]);Primop(Minus,[Var "y";Var"y"])])
  );

  (((B false, "g"), (Fn ([("r", Arrow ([Int; Int; Bool], Bool))],
                         Primop (Times, [Var "r"; Var "g"])))), 
   (Fn ([("r", Arrow ([Int; Int; Bool], Bool))],
        Primop (Times, [Var "r"; B false]))));
  
  
  
  (((Primop (Minus, [Var "u"; Var "c"]), "z"), 
    
    (Fn ([("c", Int); ("u", Bool); ("h", Bool)],
         Primop (Times, [Var "c"; Var "z"])))),
   
   (Fn ([("x", Int); ("y", Bool); ("z", Bool)],
        Primop (Times, [Var "x";Primop (Minus, [Var "u"; Var "c"]) ])))
   
  );
  
  (((I 1, "x"), Fn([("x",Int)],Var"x")), 
   Fn([("x",Int)],Var "x"));
  
  (((Var "y", "x"), Fn([("y",Int)],Primop(Plus,[Var "x"; Var "a"]))), 
   Fn([("z",Int)],Primop(Plus,[Var "y"; Var "a"]))
  );
]

(* TODO: Implement the missing cases of subst. *)
let rec subst ((e', x) as s) exp =
  match exp with
  | Var y ->
      if x = y then e'
      else Var y
  | I n -> I n
  | B b -> B b
  | Primop (po, args) -> Primop (po, List.map (subst s) args)
  | If (e, e1, e2) ->
      If (subst s e, subst s e1, subst s e2)
  | Let (y, e1, e2) ->
      let e1' = subst s e1 in
      if y = x then
        Let (y, e1', e2)
      else
        let (y, e2) =
          if List.mem y (free_variables e') then
            rename y e2
          else
            (y, e2)
        in
        Let (y, e1', subst s e2)

  | Rec (y, t, e) -> 
      if y = x then Rec (y,t,e) 
      else 
        let (y,ex1) = 
          if List.mem y (free_variables e') then rename y e 
          else (y, e)
        in Rec(y,t,subst s ex1) 

  | Fn (xs, e) -> (match xs with 
  
      | [] -> Fn([],e)
      |_ -> (match e' with
          |Primop(_)-> 
              let renamev = List.map fst xs in
              let typelist = List.map snd xs in 
              let (newnames,newexp) = rename_all renamev exp in 
              let newpara = List.map2 (fun x y -> (x,y)) newnames typelist in
              (match newexp with
               |Fn(l,r)-> Fn(newpara,subst s r))
              
          |_->
              let ftype =  List.hd (List.map snd xs) in
              let vars = (List.map fst xs) in 
              if List.length vars <> 1 then (*multiple parameters*)
                let replacev = (free_variables exp) in 
                if List.length replacev = 0 then Fn(xs,e)
                else Fn(xs,subst s e) (***2 cases**********)
              
              else (*one parameter*)
                let v = (List.hd vars) in
                let (xsi,ef) = rename v e in 
                Fn([xsi,ftype], subst s ef)))
  
  | Apply (e, es) -> Apply (subst s e, List.map (subst s) es)
          
        
and rename x e =
  let x' = freshVar x in
  (x', subst (Var x', x) e)

and rename_all names exp =
  List.fold_right
    (fun name (names, exp) ->
       let (name', exp') = rename name exp in
       (name' :: names, exp'))
    names
    ([], exp)

(* Applying a list of substitutions to an expression, leftmost first *)
let subst_list subs exp =
  List.fold_left (fun exp sub -> subst sub exp) exp subs
