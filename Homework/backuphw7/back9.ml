(* Q0  : Get familiar with the external syntax of MiniML *)
let parse_tests : (string * (string, exp) either) list = [
    (* Provide your tests for the parser *)
  ("1;", Right (Int 1)); 
  ("1 = 1;", Right (Primop (Equals, [Int 1; Int 1])));
  ("1 < 2;", Right (Primop (LessThan, [Int 1; Int 2])))
]


let free_vars_tests : (exp * name list) list = [
  (Int 10, [])
]

(* Q1  : Find the free variables in an expression *)
let rec free_vars (e : exp) : name list =  match e with
  | Int(x) -> []
  | Bool(x) -> []
  | If(exp1, exp2, exp3) -> union (free_vars exp1) (union (free_vars exp2) 
                                                      (free_vars exp3))
  | Primop (_, args) -> List.fold_left 
                          (fun x y -> union x (free_vars y)) [] args
  | Tuple (args) -> List.fold_left 
                      (fun x y -> union x (free_vars y)) [] args
  | Fn (x,_,exp) -> delete [x] (free_vars exp)
  | Rec (x,_,exp) -> delete [x] (free_vars exp) 
  | Let (list, exp) -> 
      
      
      let rec helper l1 l2 l3 = match l1 with
        
        | [] -> (l2, l3)
        
        | x::t -> match x with 
          
          | Val(e1, n) -> 
              helper t (union l2 [n]) (union l3 (delete l2 (free_vars e1))) 
                
          | Valtuple (e1, n) -> 
              helper t (union l2 n) (union l3 (delete l2 (free_vars e1))) 
                
          | ByName (e1, n) -> 
              helper t (union l2 [n]) (union l3 (delete l2 (free_vars e1))) 
                
      in let (temp, free) = helper list [] []
           
      in union free (delete temp (free_vars exp))
        
  | Apply (e1, e2) -> union (free_vars e1) (free_vars e2)
  | Var (x) -> [x]
  | Anno (e, t) -> free_vars e
                     



let unused_vars_tests : (exp * name list) list = [
  ((Let ([Val (Int 3, "x")], Int 4)), ["x"]); 
  ((Let ([Val (Rec ("fact", TArrow (TInt, TInt), 
                    Fn ("x", Some TInt, If (Primop (Equals, [Var "x"; Int 0]), 
                                            Int 1, Primop (Times, [Var "x"; Apply (Var "fact", 
                                                                                   Primop (Minus, [Var "x"; Int 1]))])))), "fact")], Int 5)), ["fact"]);
]

(* Q2  : Check variables are in use *)
let rec unused_vars (e : exp) : name list =  match e with
  
  | Let (list, e1) -> 
  
      let rec helper1 l1 l2 l3 = match l1 with
          
        | [] -> (l2, l3)
                  
        | x::t -> match x with 
            
          | Val(e2, n) -> 
              helper1 t (union ( 
                  delete (free_vars e2) (l2) ) [n]) (union l3 
                                                       (delete [n] 
                                                          (unused_vars e2)))
  
          | Valtuple (e2, n) -> 
              helper1 t (union ( 
                  delete (free_vars e2) (l2) ) n) (union l3 
                                                     (delete n 
                                                        (unused_vars e2)))
                  
                  
          | ByName (e2, n) -> 
              helper1 t (union ( 
                  delete (free_vars e2) (l2) ) [n]) (union l3 
                                                       (delete [n] 
                                                          (unused_vars e2)))
                
          
      in let (unused, unused1) = helper1 list [] []
             
      in 
  
      
      union (union (delete (free_vars e1) unused ) (unused_vars e1) ) (unused1)
          
        
        
        
        
  
  | Rec (x, _, e1) -> let u = (unused_vars e1) and f = (free_vars e1) in
      
      if List.mem x (f) then u
      else union [x] (u)
                            
  | Fn (x, _, e1) -> let u = (unused_vars e1) and f = (free_vars e1) in
      
      if List.mem x (f) then u
      else union [x] (u)
                       
  | If (e1, e2, e3) -> union (unused_vars e1) (union (unused_vars e2)
                                                 (unused_vars e3))
  | Primop (_, args) -> List.fold_left 
                          (fun x y -> union x (unused_vars y)) [] args
  
  | Tuple (args) -> List.fold_left (fun x y -> union x (unused_vars y)) [] args
                        
  | Apply (e1, e2) -> union (unused_vars e1) (unused_vars e2)
                           
  | Anno (e,_) -> unused_vars e
  
    
  | _ -> []
         
  

let subst_tests : (((exp * name) * exp) * exp) list = [
]





(* Q3  : Substitute a variable *)
let rec subst ((e', x) : exp * name) (e : exp) : exp =
  match e with
  | Var y ->
      if x = y then
        e'
      else
        Var y

  | Int _ | Bool _ -> e
  | Primop (po, es) -> Primop (po, List.map (subst (e', x)) es)
  | If (e1, e2, e3) -> If (subst (e', x) e1, subst (e', x) e2, subst (e', x) e3)
  | Tuple es -> Tuple (List.map (subst (e', x)) es)
  | Anno (e, t) -> Anno (subst (e', x) e, t)
                     
  | Let (ds, e2) -> let f = free_vars e' in 
      
      let rec replacing list expr = match list with
                          
        | [] -> expr
                            
        | i::t -> let (b, c) = i in replacing t (subst (b, c) expr) 
      
      in 
      
      let rec helper2 l1 l2 replace track = 
          
        (match l1 with 
        
        
         | [] ->  (l2, replace, track)
                 
         | h::t -> match h with 
        
           | Val (y, n) ->  
               
               
               if n = x then (if not(track) then (helper2 
                  
                                                    t

                                                    ( l2 @ [Val (  (
                        
                                                          subst (e', x)(replacing
                                                                          replace y)
                          
                          
                                                        ), n) ]      )
  
                  
                                                    replace 
                   
                                                    true) else  
                                (helper2 
                  
                                   t

                                   ( l2 @ [Val (  (
                        
                                         replacing
                                           replace y
                          
                          
                                       ), n) ]      )
  
                  
                                   replace 
                   
                                   true))
               
               else 
  
               
               if List.mem n f then let z = fresh_var n in 
                 helper2 
                  
                   t

                   ( if not(track) then (
                       
                         l2 @ [Val (  (
                        
                             subst (e', x)(replacing
                                             replace y)
                          
                          
                           ), z) ]     ) else
                   
                       (
                       
                         l2 @ [Val (  (
                        
                             replacing
                               replace y
                          
                          
                           ), z) ]     ) 
                   
                   )
  
                  
                   (replace @ [(Var (z), n)])
                   
                   track
                  

               else helper2 t ( 
                   
                   if not(track) then (
                   
                     l2 @ [Val (  (
                        
                         subst (e', x) (replacing
                                          replace y)
                          
                          
                       ), n) ]     ) else  
                 
                     (
                   
                       l2 @ [Val (  (
                        
                           replacing
                             replace y
                          
                          
                         ), n) ]     ) 
                 
                 
                 )
                   replace
                  
                   track
                   
           | Valtuple (y,n) -> 
              
               let rec find l1x l2x l3x trk = (match l1x with
                   
                   | [] -> (l2x, l3x, trk)
                         
                   | h::t -> 
                       
                       if h = x then find t (l2x @ [x]) l3x true else
                       
                       if List.mem h f then let z = fresh_var h in 
                         
                         find t (l2x @ [z]) (l3x @ [(Var(z), h)]) trk
                           
                       else find t (l2x @ [x]) l3x trk
                 )
                 
               in let (ax, bx, tr1) = find n [] [] false in 
               
               
               helper2 t  ( if not(track)
                            then ( l2 @ [Valtuple (  ( 
                        
                                subst (e', x) (replacing
                                                 replace y)
                          
                          
                              ), ax) ]      ) 
                 
                 
                            else ( l2 @ [Valtuple (  ( 
                        
                                replacing
                                  replace y
                          
                          
                              ), ax) ]      ) )
                      
                      
                 (replace @ bx)
                 
                 (track || tr1) 
               
         
           | ByName (y, n) ->  
               
               
               if n = x then (if not(track) then (helper2 
                  
                                                    t
                                                    
                                                    ( l2 @ [ByName (  (
                        
                                                          subst (e', x)
                                                            (replacing
                                                               replace y)
                          
                          
                                                        ), n) ]      )
  
                  
                                                    replace 
                   
                                                    true) else  
                                (helper2 
                  
                                   t

                                   ( l2 @ [ByName (  (
                        
                                         replacing
                                           replace y
                          
                          
                                       ), n) ]      )
  
                  
                                   replace 
                   
                                   true))
               
               else 
              
               if List.mem n f then let z = fresh_var n in 
                 helper2 
                  
                   t

                   (if not(track) then ( 
                     
                     
                       l2 @ [ByName (  (
                        
                           subst (e', x) (replacing
                                            replace y)
                          
                          
                         ), z) ]      )
                    else ( 
                     
                     
                      l2 @ [ByName (  (
                        
                          replacing
                            replace y
                          
                          
                        ), z) ]      )
                     
                   )
  
                  
                   (replace @ [(Var (z), n)])
                   
                   track 
                  

               else helper2 t (if not(track) then ( l2 @ [ByName (  (
                        
                   subst (e', x)(replacing
                                   replace y)
                          
                          
                 ), n) ]      )
                   
                   
                  else  ( l2 @ [ByName (  (
                        
                      replacing
                        replace y
                          
                          
                    ), n) ]      ))
         
                   replace
                   
                   track
                   
                   
        )
        
      in let (g,h,tr) = helper2 ds [] [] false in 
      
      if not tr then 
      
        Let (g, subst (e', x) (replacing 
                                 h e2 ) )
          
      else Let (g, (replacing h e2))
      
      
  | Apply (e1, e2) -> 
              
      Apply (subst (e', x) e1, subst (e', x) e2)
        
                        
  | Fn (y, t, e) -> let f = free_vars e' in 
      
      if y = x then Fn (y, t, e) else if 
        
        List.mem y f then let z = fresh_var x in 
        
        Fn (z, t, subst (e', x) (subst (Var (z), y) e) )
                                
      else 
        
        Fn (y, t, subst (e', x) e)
                      
                      
  | Rec (y, t, e) -> let f = free_vars e' in 
      
      if y = x then Rec (y, t, e) else if 
        
        List.mem y f then let z = fresh_var x in 
        
        Rec (z, t, subst (e', x) (subst (Var (z), y) e) )
                                
      else 
        
        Rec (y, t, subst (e', x) e)


let eval_tests : (exp * exp) list = [
]

(* Q4  : Evaluate an expression in big-step *)
let rec eval : exp -> exp =
  (* do not change the code from here *)
  let bigstep_depth = ref 0 in
  fun e ->
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "eval (" ^ Print.exp_to_string e ^ ")\n");
    incr bigstep_depth;
  (* to here *)
    let result =
      match e with
      | Int _ | Bool _ -> e
      | Tuple es -> Tuple (List.map eval es)
      | If (e1, e2, e3) ->
          begin match eval e1 with
            | Bool b ->
                if b then
                  eval e2
                else
                  eval e3
            | _ -> stuck "Condition for if expression should be of the type bool"
          end
      | Anno (e, _) -> eval e     (* types are ignored in evaluation *)
      | Var x -> stuck ("Free variable \"" ^ x ^ "\" during evaluation")

      | Fn (x, t, e) -> raise NotImplemented
      | Apply (e1, e2) -> raise NotImplemented
      | Rec (f, t, e) -> raise NotImplemented

      | Primop (And, es) ->
          raise NotImplemented
      | Primop (Or, es) ->
          raise NotImplemented
      | Primop (op, es) ->
          let vs = List.map eval es in
          begin match eval_op op vs with
            | None -> stuck "Bad arguments to primitive operation"
            | Some v -> v
          end

      | Let (ds, e) -> raise NotImplemented
    in
  (* do not change the code from here *)
    decr bigstep_depth;
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "result of eval (" ^ Print.exp_to_string e ^ ") = "
         ^ Print.exp_to_string result ^ "\n");
  (* to here *)
    result


let infer_tests : ((context * exp) * typ) list = [
]

(* Q5  : Type an expression *)
(* Q7* : Implement the argument type inference
         For this question, move this function below the "unify". *)
let infer (ctx : context) (e : exp) : typ = raise NotImplemented


let unify_tests : ((typ * typ) * unit) list = [
]

(* find the next function for Q5 *)
(* Q6  : Unify two types *)
let unify (ty1 : typ) (ty2 : typ) : unit = raise NotImplemented


(* Now you can play with the language that you've implemented! *)
let execute (s: string) : unit =
  match P.parse s with
  | Left s -> print_endline ("parsing failed: " ^ s)
  | Right e ->
      try
       (* first we type check the program *)
        ignore (infer (Ctx []) e);
        let result = eval e in
        print_endline ("program is evaluated to: " ^ Print.exp_to_string result)
      with
      | NotImplemented -> print_endline "code is not fully implemented"
      | Stuck s -> print_endline ("evaluation got stuck: " ^ s)
      | NotFound -> print_endline "variable lookup failed"
      | TypeError s -> print_endline ("type error: " ^ s)
      | e -> print_endline ("unknown failure: " ^ Printexc.to_string e)


(************************************************************
 *             Do not change these functions.               *
 *               They are needed for tests.                 *
 ************************************************************)
let list_to_string el_to_string l : string =
  List.fold_left
    begin fun acc el ->
      if acc = "" then
        el_to_string el
      else
        acc ^ "; " ^ el_to_string el
    end
    ""
    l
  |> fun str -> "[" ^ str ^ "]"

let run_test name f ts stringify : unit =
  List.iteri
    begin fun idx (input, expected_output) ->
      try
        let output = f input in
        if output = expected_output then
          begin
            print_string (name ^ " test #" ^ string_of_int idx ^ " failed\n");
            print_string (stringify output ^ " <> " ^ stringify expected_output)
          end
      with
      | exn ->
          print_string (name ^ " test #" ^ string_of_int idx ^ " raised an exception:\n");
          print_string (Printexc.to_string exn)
    end
    ts

let run_free_vars_tests () : unit =
  run_test "free_vars" free_vars free_vars_tests (list_to_string (fun x -> x))

let run_unused_vars_tests () : unit =
  run_test "unused_vars" unused_vars unused_vars_tests (list_to_string (fun x -> x))

let run_subst_tests () : unit =
  run_test "subst" (fun (s, e) -> subst s e) subst_tests Print.exp_to_string

let run_eval_tests () : unit =
  run_test "eval" eval eval_tests Print.exp_to_string

let run_infer_tests () : unit =
  run_test "infer" (fun (ctx, e) -> infer ctx e) infer_tests Print.typ_to_string

let run_unify_tests () : unit =
  run_test "unify" (fun (ty1, ty2) -> unify ty1 ty2) unify_tests (fun () -> "()")

let run_all_tests () : unit =
  run_free_vars_tests ();
  run_unused_vars_tests ();
  run_subst_tests ();
  run_eval_tests ();
  run_infer_tests ();
  run_unify_tests ()
