(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [
  
  ([A;A;A;A;G;G;A;T;T;T;C;T;C], [(4,A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)]);
  ([A], [(1,A)]);
  ([], []);
  ([G;G], [(2,G)])
  
  
  
]

(* TODO: Implement compress. *)
let compress (l : nucleobase list) : (int * nucleobase) list =
  
  let rec compresser l ac : (int * nucleobase) list =  match l with
    | [] -> []
    | x::[] -> [(ac,x)]
    | x::y::t -> if x = y then 
          let x::t = l in compresser t (ac + 1)
        else 
          let x::t = l in [(ac, x)] @ compresser t 1
        
  
  
  in
  compresser l 1

(* TODO: Write a good set of tests for decompress *)
let decompress_tests = [
  
  ([(4,A); (2, G); (1, A); (3, T); (1, C); (1, T); (1, C)], [A;A;A;A;G;G;A;T;T;T;C;T;C]);
  ([(1,A)], [A]);
  ([], []);
  ([(2,G)], [G;G])
  
]

(* TODO: Implement decompress. *)
let rec decompress (l : (int * nucleobase) list) : nucleobase list = match l with
  | [] -> [] 
  | x::t -> let (a,b) = x in let rec iter (l2, a, b) = match a with
      | 0 -> []
      | _ -> [b] @ iter (l2, a - 1, b)
                 
                 
      in iter ([], a, b) @ decompress t
               
               
                 
          
          
  


(* Q2 *)
(* TODO: Write a good set of tests for eval *)
let eval_tests = [
  
  (MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0), 27.5);
  (SIN (FLOAT 0.0), 0.0);
  (FLOAT 5.0, 5.0);
  (COS(FLOAT 0.0), 1.0);
  (EXP(FLOAT 0.0),1.0);
  (MINUS(FLOAT 2.0, FLOAT 1.0), 1.0)
  
    
  

]

(* TODO: Implement eval. *)
let rec eval e = match e with 
  | FLOAT(x) -> x
  | SIN(x) -> sin (eval x)
  | COS(x) -> cos (eval x)
  | EXP(x) -> exp (eval x)
  | PLUS(x,y)  -> eval x +. eval y
  | MINUS(x,y) -> eval x -. eval y
  | MULT(x,y) -> eval x *. eval y
  | DIV(x,y) -> eval x /. eval y
  
                 


  

(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  (MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0), [Float 2.2; Float 3.3; Plus; Float 5.0; Mult]);
  (FLOAT(5.0), [Float 5.0]);
  (PLUS(FLOAT 5.0, FLOAT 5.0), [Float 5.0; Float 5.0; Plus]);
  (COS(FLOAT 0.0), [Float 0.0; Cos]);
  (EXP(FLOAT 0.0), [Float 0.0; Exp])
]

(* TODO: Implement to_instr. *)
let rec to_instr e = match e with 
  | FLOAT(x) -> [Float x]
  | SIN(x) -> to_instr x @ [Sin] 
  | COS(x) -> to_instr x @ [Cos]
  | EXP(x) -> to_instr x @ [Exp]
  | PLUS(x,y) -> to_instr x @ to_instr y @ [Plus]
  | MINUS(x,y) -> to_instr x @ to_instr y @ [Minus]
  | DIV(x,y) -> to_instr x @ to_instr y @ [Div]
  | MULT(x,y) -> to_instr x @ to_instr y @ [Mult]
  
  
  


(* TODO: Write a good set of tests for instr *)
let instr_tests = [
  
  ((Mult, [5.0; 5.5]), Some [27.5]);
  ((Plus, [5.0; 5.5]), Some [10.5]);
  ((Exp, [0.0; 5.5]), Some [1.0; 5.5]);
  ((Sin, [0.0; 5.5]), Some [0.0; 5.5]);
  ((Float 3.0, [5.0; 5.5]), Some [3.0; 5.0; 5.5])
  
]


(* TODO: Implement to_instr. *)               
let instr i s = match s with
  | [] -> (match i with 
      | Float z -> Some([z] @ s) 
      | _ -> None)
  
  | [x] -> 
      (let x::t = s in match i with 
        | Sin -> Some([sin x] @ t)
        | Cos -> Some([cos x] @ t)
        | Exp -> Some([exp(x)] @ t)
        | Float z -> Some([z] @ s) 
        | _ -> None)
  
                 
  | x::y::t -> match i with 
    | Float z -> Some([z] @ s) 
    | Plus -> Some([y +. x] @ t)
    | Minus -> Some([y -. x] @ t)
    | Mult -> Some ([y *. x] @ t)
    | Div -> Some ([y /. x] @ t)
    | _ -> let x::t = s in match i with 
      | Sin -> Some([sin x] @ t)
      | Cos -> Some([cos x] @ t)
      | Exp -> Some([exp(x)] @ t)
      | Float z -> Some([z] @ s) 
      | _ -> None

        
  


(* TODO: Write a good set of tests for prog *)
let prog_tests = [
  (([Float 2.2; Float 3.3; Plus; Float 5.; Mult]), Some 27.5);
  (([Float 0.0; Sin]), Some 0.0)
]

(* TODO: Implement prog. *)
let prog instrs = 
  
  let rec progrec instrs stack = match instrs with 
    | [] -> let x::t = stack in Some(x)
    | [Float x] -> None
    | _ -> let x::t = instrs in match instr x stack with
      | Some (z) -> progrec t (z)
      | None -> None
    
    
  in progrec instrs []
