(* Question 1: Tree Depth *)
(* TODO: Write a good set of tests for tree depth. *)
let tree_depth_cps_tests : (int tree * int) list =
  [ (* Your test cases go here *) 
    (Empty,0); (Tree(Empty,2,Tree(Empty,3,Empty)),2);
  ]

(* An example of Non-CPS function to find depth of a tree: *)
let rec tree_depth t =
  match t with
  | Empty -> 0
  | Tree (l, _, r) -> 1 + max (tree_depth l) (tree_depth r)

(* TODO: Implement a CPS style tree_depth_cps function.*)
let tree_depth_cps (t: 'a tree) = 
  let rec helper (t: 'a tree) (sc: (int -> int)) =
    match t with
    |Empty -> sc 0
    |Tree (l,_,r) -> helper l (fun hl-> helper r (fun hr -> sc (1 + max hl hr)))
  in
  helper t (fun h -> h)

(* Question 2(a): Tree Traversal *)
(* TODO: Write a good set of tests for testing your tree traversal function. *)
let traverse_tests : (int tree * int list) list = [
  (Tree (Tree (Empty, 2, Empty), 1, Tree (Empty, 3, Empty)),[2; 3; 1]);
  (Empty,[]);

]

(* TODO: Implement a CPS style postorder traversal function. *)
let traverse (tree : 'a tree) : 'a list = 
  let rec helper (tree : 'a tree) (sc: ('a list->'r)) =
    match tree with
    | Empty -> sc [];
    | Tree(l,c,r) -> helper l (fun hl-> helper r (fun hr -> sc (hl @ hr @ [c]))) 
  in
  helper tree (fun h->h)

(* Question 2(b): Distances from the Root *)
(* TODO: Write a good set of tests for testing the get_distances function. *)
let get_distances_tests : (int tree * int list) list = [
  (Tree (Tree (Empty, 3, Empty), 5, Tree (Empty, 6, Empty)),[8; 11; 5]);

]

(* TODO: Implement a CPS style get_distances function. *)
let get_distances (tree : int tree) : int list = 
  let rec helper tree sum sc =
    match tree with
    |Empty -> sc []
    |Tree(l,c,r) -> helper l (c+sum) (fun hl-> helper r (c+sum) (fun hr -> sc (hl @ hr @ [c+sum])))
  in
  helper tree 0 (fun h->h)

(* Question 3: Finding Subtrees *)
(* TODO: Write a good set of tests for finding subtrees. *)
let find_subtree_cps_tests : ((int list * int tree) * int tree option) list =
  [ (([],Empty),Some Empty); 
    (([1],Tree(Tree(Empty,1,Empty),3,Tree(Empty,2,Empty))),None);
    (([3],Tree(Tree(Empty,1,Empty),3,Tree(Empty,2,Empty))),Some (Tree (Empty,1,Empty)));
    (([3],Tree(Empty,3,Tree(Empty,2,Empty))),Some Empty);
    (([10],Empty),None);
    ( ( [5;7;1], Tree (
          Tree (
            Tree (Empty, 7, Empty), 
            7, 
            Tree (Empty, 1, Empty)), 
          5, 
          Tree (
            Tree (Empty, 7, Empty), 
            7, 
            Tree (Empty, 1, Empty)))) 
    , Some Empty );
  
  (* Your test cases go here *) ]

(* TODO: Implement a CPS style find_subtree_cont function.*)
let find_subtree_cps ls tree =
  let rec helper (ls:'a list) (tree:'a tree) (sc:'a tree->'a tree option) (fc:'a tree->'a tree option) = 
    match (ls,tree) with
    |([], i) -> sc i
    |(_, Empty)-> fc tree
    |(x::xs, Tree(l,c,r))-> match (l,c,r) with
      |(l,c,r) when c <> x -> fc l
      |(l,c,r) -> helper xs l (fun ll-> helper xs l sc fc) (fun rr-> helper xs r sc fc)
                
  in
  helper ls tree (fun h->(Some h)) (fun hf-> None)


