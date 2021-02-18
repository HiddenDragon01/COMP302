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
  | Primop (_, [arg]) -> free_vars arg
  | Primop (_, [arg1; arg2]) -> union (free_vars arg1) (free_vars arg2)
  | Tuple (args) -> List.flatten (List.map (free_vars) args) 
  | Fn (x,_,exp) -> delete [x] (free_vars exp)
  | Rec (x,_,exp) -> delete [x] (free_vars exp) 
  | Let (list, exp) -> 
      
      let temp = ref [] in 
      
      let free = List.flatten (List.map
                                 (fun x -> match x with 
       
                                    | Val(e, n) -> 
                                        temp := union !temp [n]; free_vars e
                                      
                                    | Valtuple (e, n) -> 
                                        temp := union !temp n;
                                        free_vars e
                                    | ByName (e, n) -> 
                                        temp := union !temp [n]; free_vars e
                                 ) list) 
        
      in delete (!temp) (union free (free_vars exp))
        
        
        
  | Apply (e1, e2) -> union (free_vars e1) (free_vars e2)
  | Var (x) -> [x]
  | Anno (e, t) -> free_vars e
  | _ -> stuck "wrong format"



let unused_vars_tests : (exp * name list) list = [
]

(* Q2  : Check variables are in use *)
let rec unused_vars (e : exp) : name list =  match e with
  
  | Let (list, e1) -> 
          
      let temp = ref [] in
      
      let decs = List.flatten (List.map
                                 (fun x ->  match x with 
       
                                    | Val(e2, n) -> 
                                        temp := 
                                          delete [n]
                                            (union !temp (unused_vars e2)); 
                                        [n]
                                      
                                    | Valtuple (e2, n) -> 
                                        temp := 
                                          delete n
                                            (union !temp (unused_vars e2)); 
                                        n
                                    | ByName (e2, n) -> 
                                        temp := 
                                          delete [n]
                                            (union !temp (unused_vars e2)); 
                                        [n]
                                 ) list) 
        
      in
  
      union (delete (free_vars e1) (union decs (unused_vars e1))) (!temp)
        
  | Rec (x, _, e1) -> let u = (unused_vars e1) in
      
      if List.mem x (u) then delete [x] u else union [x] (u)
                            
  | Fn (x, _, e1) -> let u = (unused_vars e1) in
      
      if List.mem x (u) then delete [x] u else union [x] (u)
                       
  | If (e1, e2, e3) -> union (unused_vars e1) (union (unused_vars e2)
                                                 (unused_vars e3))
  | Primop (_, [arg]) -> unused_vars arg
                           
  | Primop (_, [arg1; arg2]) -> union (unused_vars arg1) (unused_vars arg2)
  
  | Tuple (args) -> List.flatten (List.map (unused_vars) args) 
                        
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

  | Let (ds, e2) -> raise NotImplemented
  | Apply (e1, e2) -> raise NotImplemented
  | Fn (y, t, e) -> raise NotImplemented
  | Rec (y, t, e) -> raise NotImplemented


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
