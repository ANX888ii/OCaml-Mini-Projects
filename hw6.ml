(*--------------------------------------------------------------*)
(* Q1 : String to Characters to String                  *)
(*--------------------------------------------------------------*)

(* 1.1 Turn a string into a list of characters. *)
let string_explode (s : string) : char list =
  tabulate (String.get s) (String.length s)
  

(* 1.2 Turn a list of characters into a string. *)
let string_implode (l : char list) : string =
  List.fold_right (fun x y -> x^y) (List.map (fun n -> Char.escaped n) l) ""
  
  

(* ------------------------------------------------------------------------*)
(* Q2 : Bank Account *)
(* ------------------------------------------------------------------------*)

let open_account (pass: password) : bank_account =
  
  let balance = ref 0 and password = ref pass and wrong = ref 0 in
  {
    
    update_pass = (fun old_pass new_pass -> 
        if !wrong = 3 then raise account_locked
        else if old_pass <> !password then (wrong:=!wrong +1; raise wrong_pass);
        wrong := 0;password := new_pass 
      );
      
      
    deposit = (fun input_pass amt ->
        if !wrong = 3 then raise account_locked 
        else if input_pass <> !password then (wrong:=!wrong +1; raise wrong_pass)
        else if amt < 0 then raise negative_amount; 
        wrong:= 0; balance:=!balance+amt 
      );
      
      
    retrieve = (fun input_pass amt-> 
        if (!wrong = 3 || input_pass <> !password) then 
          if !wrong = 3 then raise account_locked 
          else (wrong:=!wrong+1;raise wrong_pass)
              
        else if amt > !balance || amt < 0 then 
          if amt > !balance then raise not_enough_balance 
          else raise negative_amount;
          
        wrong:=0; balance := !balance - amt
            
    
      );
    
    show_balance = (fun input_pass -> 
        if !wrong = 3 then raise account_locked
        else if input_pass <> !password then (wrong:=!wrong +1; raise wrong_pass);
        wrong:=0;!balance)
  
  
  }
;;

