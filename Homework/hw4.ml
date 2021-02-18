(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account (p: passwd) : bank_account =
  
  let n = ref 0 and pass = ref p and count = ref 0 in 
  
  {
    update_passwd = (fun oldpass -> fun newpass -> if oldpass <> !pass then 
                        (count := !count + 1; 
                         raise wrong_pass) 
                      else (pass := newpass; count := 0))
  ;
    retrieve = (fun pwd -> fun amount -> if !count = 3 then 
                   raise too_many_attempts 
                 else if pwd <> !pass then (count := !count + 1; 
                                            raise wrong_pass)  
                 else 
                   (if amount > !n then raise no_money else n := !n - amount
                   ; count := 0));
    deposit = (fun pwd -> fun amount -> if !count = 3 then 
                  raise too_many_attempts 
                    
                else if pwd <> !pass then raise (count := !count + 1; 
                                                 raise wrong_pass)  
                                                                
                else n:= !n + amount; count := 0);
    
    print_balance = (fun pwd -> if !count = 3 then 
                        raise too_many_attempts 
                    
                      else if pwd <> !pass then raise (count := !count + 1; 
                                                       raise wrong_pass) 
                      else (count := 0; !n))
    
    
  }
  
  

;;


(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec fib_I (n: int) : fib_result =
  
  let x = ref 1 in
  
  let rec fib n = if n = 0 then 0
    else (if n = 1 then 1 else (x := !x + 2; fib (n-2) + fib (n-1)))
     
  in {
    num_rec = !x;
    result = fib n
  }
                       
;;


(* Q 2.2 : Memoization with a global store *)

let fib_memo (n: int) : int =
  let rec fib n =
    
    match Hashtbl.find_opt store n with 
    | Some(x) -> x
    | None -> 
        if n = 0 then (Hashtbl.add store 0 0; 0)
        else if n = 1 then (Hashtbl.add store 1 1; 1) else 
          let a = fib (n-1) + fib(n-2) in (Hashtbl.add store n a; a)
  
  in
  fib n
;;


(* Q 2.3 : General memoization function *)



let memo (f: (('a -> 'b) -> 'a -> 'b)) (stats: stats) : ('a -> 'b) =

  let hash = Hashtbl.create 1000;  in

  fun x ->

    (let rec h x = 

     

       match Hashtbl.find_opt hash x with 

       | Some(t) -> stats.lkp := !(stats.lkp) + 1; t

       | None -> let z = f h x in 

           (Hashtbl.add hash x z; stats.entries := !(stats.entries) + 1; z) 

           

     in h x) 

    

  

;;





(* Q 2.4 : Using memo to efficiently compute the Fibonacci number *)
(* We also accept let fibM = raise NotImplemented ;; *)


let fibM =
  
  let stats =  {
    entries = ref 0;
    lkp = ref 0
  
  }; in
  
 
  
  let memocall = ref (memo (fun g x -> if x = 0 then 0 
                             else (if x = 1 
                                   then 1 else g(x-2) + g(x-1))) stats) in
  
  fun n -> 
    (!memocall n, stats)
  
;;
