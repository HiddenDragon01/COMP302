(* Q0  : Get familiar with the external syntax of MiniML *)
let parse_tests : (string * (string, exp) either) list = [
    (* Provide your tests for the parser *)
  ("1;", Right (Int 1)); 
  ("1 = 1;", Right (Primop (Equals, [Int 1; Int 1])));
  ("1 < 2;", Right (Primop (LessThan, [Int 1; Int 2])))
]

let free_vars_tests : (exp * name list) list = [
  (Int 10, []); 
  ((Let ([Valtuple (Tuple [Primop (Plus, [Int 2; Int 1]); Primop (Times, [Int 2; Var "z"])], ["x"; "z"])], Primop (Times, [Var "x"; Var "y"]))) ,  ["z"; "y"] ); 
  ((Let ([Val (Rec ("ryan", TArrow (TInt, TInt), Fn ("n", Some TInt, If (Primop (Equals, [Var "n"; Int 0]), Var "x", Apply (Var "ryan", Primop (Minus, [Var "n"; Int 1]))))), "ryan")], Int 3)), ["x"]); 
  ((Anno (If (Bool true, Tuple [Var "x1"; Var "x2"; Var "x3"], Tuple [Var "x4"; Var "x5"]), TInt)) ,  ["x1"; "x2"; "x3"; "x4"; "x5"] ); 
  ((Apply (Apply (Var "trump", Fn ("biden", None, Primop (Times, [Var "harris"; Int 3]))), Int 100)), ["trump"; "harris"]); 
  ((Let ([Valtuple (Int 3, ["x"; "y"; "z"]); Val (Var "z", "n2")], Primop (Plus, [Primop (Plus, [Var "x"; Int 1]); Var "y"]))), []); 
  ((Let ([Val (Int 5, "x")], Let ([Val (Primop (Plus, [Var "x"; Int 3]), "x")], Primop (Plus, [Var "x"; Var "x"])))) ,[]); 
  ((Fn ("x", None, Var "x")),[]); 
  ((Fn ("x", None, Primop (Plus, [Var "x"; Var "y"]))), ["y"]); 
  ((Fn ("x", None, Apply (Var "z", Var "x"))),["z"]); 
  ((Let ([Val (Primop (Plus, [Var "x"; Int 3]), "y")], Fn ("x", None, Primop (Times, [Var "x"; Var "y"])))), ["x"] ); 
  (Let ([Val (Primop (Plus, [Primop (Times, [Var "x"; Primop (Plus, [Var "x"; Int 1])]); Int 3]), "1x")], Primop (Times, [Var "1x"; Primop (Plus, [Var "x"; Int 1])])), ["x"]); 
  (Let ([Val (Let ([Val (Primop (Times, [Var "x"; Int 2]), "y")], Primop (Times, [Var "y"; Var "x"])), "w")], Primop (Times, [Primop (Times, [Var "w"; Var "x"]); Var "x"])), ["x"]); 
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
          | Val(e1, n) | ByName (e1, n) -> 
              helper t (union l2 [n]) (union l3 (delete l2 (free_vars e1))) 
          | Valtuple (e1, n) -> 
              helper t (union l2 n) (union l3 (delete l2 (free_vars e1))) 
      in let (temp, free) = helper list [] [] 
      in union free (delete temp (free_vars exp)) 
  | Apply (e1, e2) -> union (free_vars e1) (free_vars e2)
  | Var (x) -> [x]
  | Anno (e, _) -> free_vars e
  
let unused_vars_tests : (exp * name list) list = [ 
  
  ((Let ([Valtuple (Tuple [Primop (Plus, [Int 2; Int 1]); Primop (Times, [Int 2; Var "z"])], ["x"; "z"])], Primop (Times, [Var "x"; Var "y"]))) ,  ["z"] );
  ((Let ([Val (Rec ("ryan", TArrow (TInt, TInt), Fn ("n", Some TInt, If (Primop (Equals, [Var "n"; Int 0]), Var "x", Apply (Var "ryan", Primop (Minus, [Var "n"; Int 1]))))), "ryan")], Int 3)), ["ryan"]);
  ((Anno (If (Bool true, Tuple [Var "x1"; Var "x2"; Var "x3"], Tuple [Var "x4"; Var "x5"]), TInt)) ,  [] );
  ((Apply (Apply (Var "trump", Fn ("biden", None, Primop (Times, [Var "harris"; Int 3]))), Int 100)), ["biden"]);
  ((Let ([Valtuple (Int 3, ["x"; "y"; "z"]); Val (Var "z", "n2")], Primop (Plus, [Primop (Plus, [Var "x"; Int 1]); Var "y"]))), ["n2"]);
  ((Fn ("x", None, Int 3)), ["x"]);
  ((Fn ("x", None, Fn ("y", None, Int 1))),["x"; "y"]);
  (Let ([Val (Primop (Plus, [Primop (Times, [Int 1; Var "y"]); Int 3]), "x")], Primop (Times, [Int 1; Var "y"])), ["x"]);
  (Let ([Val (Let ([Val (Primop (Times, [Var "w"; Int 2]), "y")], Primop (Times, [Int 1; Var "x"])), "w")], Primop (Times, [Primop (Times, [Var "w"; Var "y"]); Int 1])), ["y"]); 
  
]

(* Q2  : Check variables are in use *)
let rec unused_vars (e : exp) : name list =  match e with
  
  | Let (list, e1) -> 
      let rec delete1 ds set = match set with 
        | [] -> [] 
        | h :: t -> 
            if member h ds then delete1 (delete [h] ds) t else 
              h :: delete1 ds t in 
      let rec helper1 l1 l2 l3 = match l1 with 
        | [] -> (l2, l3) 
        | x::t -> match x with 
          | Val(e2, n)  | ByName (e2, n) -> 
              helper1 t ( ( delete1 (free_vars e2) (l2) ) @ [n]) 
                (l3 @ ( (unused_vars e2))) 
          | Valtuple (e2, n) -> 
              helper1 t (( delete1 (free_vars e2) (l2) ) @ n) 
                (l3 @ ( (unused_vars e2))) 
      in let (unused, unused1) = helper1 list [] [] in 
      ((delete1 (free_vars e1) unused ) @ (unused_vars e1) ) @ (unused1) 
  | Rec (_, _, e1) -> unused_vars e1 
  | Fn (x, _, e1) -> let u = (unused_vars e1) and f = (free_vars e1) in 
      if member x f then u else x :: u 
  | If (e1, e2, e3) -> (unused_vars e1) @ ((unused_vars e2) @ (unused_vars e3))
  | Primop (_, args) -> List.fold_left 
                          (fun x y -> x @ (unused_vars y)) [] args 
  | Tuple (args) -> List.fold_left (fun x y -> x @ (unused_vars y)) [] args 
  | Apply (e1, e2) -> (unused_vars e1) @ (unused_vars e2) 
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
      let replacing list expr = 
        let rec replacing2 list2 expr2 = match list2 with
          | [] -> expr2
          | i::t -> let (b, c) = i in replacing2 t (subst (b, c) expr2) 
        in replacing2 (List.rev list) expr in 
      let rec helper2 l1 l2 replace track = (match l1 with 
          | [] ->  (l2, replace, track)
          | h::t -> match h with 
            | Val (y, n) -> 
                if n = x then (if not(track) then (helper2 t 
                                                     ( l2 @ [Val (  ( 
                                                           subst (e', x)
                                                             (replacing replace y) 
                                                         ), n) ]      ) 
                                                     replace true) else 
                                 (helper2 t ( l2 @ [Val (  ( replacing replace y
                                                           ), n) ]) replace true)) 
                else if member n f then let z = fresh_var n in helper2 t 
                    ( if not(track) then ( 
                          l2 @ [Val (( subst (e', x)(replacing replace y)
                                     ), z) ]) else ( l2 @ [Val (( 
                        replacing replace y ), z) ]) ) (replace @ [(Var (z), n)])
                    track 
                else helper2 t ( if not(track) then ( l2 @ [Val (  ( 
                    subst (e', x) (replacing replace y) ), n) ]) else 
                      ( l2 @ [Val (( replacing replace y ), n) ]) ) replace track 
            | Valtuple (y,n) -> 
                let rec find l1x l2x l3x trk = (match l1x with 
                    | [] -> (l2x, l3x, trk) 
                    | h::t -> 
                        if h = x then find t (l2x @ [x]) l3x true else 
                        if member h f then let z = fresh_var h in 
                          find t (l2x @ [z]) (l3x @ [(Var(z), h)]) trk 
                        else find t (l2x @ [h]) l3x trk )
                in let (ax, bx, tr1) = find n [] [] false in 
                helper2 t  ( if not(track) then ( l2 @ [Valtuple (  ( 
                    subst (e', x) (replacing replace y) ), ax) ]) 
                    else ( l2 @ [Valtuple (( replacing replace y ), ax) ]) ) 
                  (replace @ bx) (track || tr1) 
            | ByName (y, n) -> 
                if n = x then (if not(track) then (helper2 t ( l2 @ [ByName ( (
                    subst (e', x) (replacing replace y) ), n) ]) replace true) 
                   else (helper2 t ( l2 @ [ByName (  ( replacing replace y
                                                     ), n) ]) replace true)) else 
                if member n f then let z = fresh_var n in helper2 t 
                    (if not(track) then ( l2 @ [ByName (( subst (e', x) 
                                                            (replacing replace y)
                                                        ), z) ]      )
                     else ( l2 @ [ByName (( replacing replace y ), z) ] ) ) 
                    (replace @ [(Var (z), n)]) track 
                else helper2 t (if not(track) then ( l2 @ [ByName (  ( 
                    subst (e', x)(replacing replace y) ), n) ]) 
                   else  ( l2 @ [ByName (( replacing replace y ), n) ])) 
                    replace track ) in let (g,h,tr) = helper2 ds [] [] false in 
      if not tr then Let (g, subst (e', x) (replacing h e2 ) ) 
      else Let (g, (replacing h e2)) 
  | Apply (e1, e2) -> 
      Apply (subst (e', x) e1, subst (e', x) e2) 
  | Fn (y, t, e) -> let f = free_vars e' in if y = x then Fn (y, t, e) else 
      if member y f then let z = fresh_var y in 
        Fn (z, t, subst (e', x) (subst (Var (z), y) e) ) else 
        Fn (y, t, subst (e', x) e) 
  | Rec (y, t, e) -> let f = free_vars e' in 
      if y = x then Rec (y, t, e) else if member y f then let z = fresh_var y in 
        Rec (z, t, subst (e', x) (subst (Var (z), y) e) ) 
      else 
        Rec (y, t, subst (e', x) e) 
          
let eval_tests : (exp * exp) list = [
  ((Let ([Valtuple (Tuple [Primop (Plus, [Int 2; Int 1]); Primop (Times, [Int 2; Int 50])], ["x"; "y"]); Val (Var "x", "z")], Primop (Times, [Primop (Times, [Primop (Times, [Var "x"; Var "x"]); Var "y"]); Var "z"]))), Int 2700 ); 
  (Let ([Val (Rec ("double", TArrow (TInt, TInt), Fn ("x", Some TInt, If (Primop (Equals, [Var "x"; Int 0]), Int 0, Primop (Plus, [Int 2; Apply (Var "double", Primop (Minus, [Var "x"; Int 1]))])))), "double")], Apply (Var "double", Int 2)), Int 4);
  ((Let ([Valtuple (Tuple [Int 1; Bool true], ["x"; "x"])], Var "x")), Bool true);
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
      | Var x -> stuck ("Free variable " ^ x ^ " during evaluation") 
      | Fn (x, t, e) -> Fn (x, t, e)
      | Apply (e1, e2) -> (match eval e1 with
          | Fn (x,_,e) -> eval (subst (eval e2, x) e )
          | _ -> stuck ("Didn't evaluate to a function")
        ) 
      | Rec (f, t, e) -> eval (subst ( (Rec (f,t, e) ), f) e) 
      | Primop (And, es) -> Bool (
          List.fold_left (fun x y -> match eval y with
              | Bool b -> if b then x else b
              | _ -> stuck ("Didn't get a bool") 
            ) true es  ) 
      | Primop (Or, es) ->
          Bool (
            List.fold_left (fun x y -> match eval y with
                | Bool b -> if b then b else x
                | _ -> stuck ("Didn't get a bool") 
              ) false es  ) 
      | Primop (op, es) ->
          let vs = List.map eval es in
          begin match eval_op op vs with
            | None -> stuck "Bad arguments to primitive operation"
            | Some v -> v
          end 
      | Let (ds, e) -> 
          let replacing list expr = 
            let rec replacing2 list2 expr2 = match list2 with 
              | [] -> expr2 
              | i::t -> let (b, c) = i in replacing2 t (subst (b, c) expr2) 
            in replacing2 (List.rev list) expr in 
          let rec evalet l1' l2' = match l1' with 
            | [] -> l2' 
            | h::t ->   (match h with 
                | Val (e1, n)  -> evalet t (l2' @ ([eval (replacing l2' e1), n]))
                | ByName (e1, n) -> evalet t (l2' @ ([replacing l2' e1, n]))
                | Valtuple (e1, lz') -> (match eval e1 with
                    | Tuple (l2z') -> if List.length lz' <> List.length l2z' then 
                          stuck "Wrong tuple length" else 
                          let rec tuplesubstitute l1z l2z repi = match (l1z, l2z) 
                            with 
                            | ([], _) -> repi
                            | (y::g, i'::z) -> 
                                tuplesubstitute g z 
                                  (repi @ ([replacing repi i', y]))
                            | _ -> repi 
                          in let repi' = tuplesubstitute lz' l2z' l2' in 
                          evalet t repi' 
                    | _->  stuck "Needed a tuple" ) ) 
          in let rempli' = evalet ds [] in let e6' = replacing rempli' e
          in eval e6' 
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

let unify_tests : ((typ * typ) * unit) list = [
  
]

(* find the next function for Q5 *)
(* Q6  : Unify two types *)
let rec unify (ty1 : typ) (ty2 : typ) : unit = 
  let rec freebies t' a' = match t' with 
    | TArrow (t1', t2') -> (freebies t1' a') && (freebies t2' a') 
    | TProduct (l') -> List.for_all (fun x -> freebies x a' ) l' 
    | TInt -> true
    | TBool -> true 
    | TVar r -> if r == a' then type_fail "In free varibles, fail!" else 
          match (!r) with
          | Some r1' -> freebies r1' a' 
          | None -> true 
  in 
  let rec freebiesvar t' a' track = match t' with
    | TArrow (t1', t2') -> (freebiesvar t1' a' true) && (freebiesvar t2' a' true)
    | TProduct (l') -> List.for_all (fun x -> freebiesvar x a' true) l'
    | TInt -> true
    | TBool -> true
    | TVar r -> if r == a' then track &&
                                (type_fail "In free varibles, fail!") 
        else match (!r) with 
          | Some r1' -> freebiesvar r1' a' track
          | None -> true 
  in 
  if typ_eq ty1 ty2 then () else match (ty1, ty2) with 
    | TVar r, TVar r' -> ( match (!r, !r') with 
        | Some (hz), Some (hz') -> unify hz hz' 
        | Some (gx'), None -> if freebiesvar gx' r' false then r':= !r else () 
        | None, Some (gy')  ->  if freebiesvar gy' r false then  r := !r' else ()
        | None, None -> r := Some (TVar r') ) 
    | TVar r, t2' -> 
        ( match !r with
          | None -> 
              if freebies t2' r then r := Some (t2') else 
                type_fail "In free varibles, fail!" 
          | Some(x) -> unify x t2' )
    | t2', TVar r-> 
        ( match !r with 
          | None -> 
              if freebies t2' r then r := Some (t2') else 
                type_fail "In free varibles, fail!" 
          | Some(x) -> unify x t2') 
    | TArrow (t1, t2), TArrow (s1, s2) -> (unify t1 s1; unify t2 s2) 
    | TProduct (l1'), TProduct (l2') -> if List.length l1' <> List.length l2'
        then type_fail "Need tuple length to be the same!" else 
          let rec helproduct l1h l2h = match (l1h, l2h) with 
            | [], [] -> ()
            | (x1'::x2'), (x3'::x4') -> (unify x1' x3'; helproduct x2' x4') 
            | _ -> () 
          in (helproduct l1' l2') 
    | _ -> type_fail "Could not unify!"

let infer_tests : ((context * exp) * typ) list = [
]

  (* Q5  : Type an expression *)
(* Q7* : Implement the argument type inference
For this question, move this function below the "unify". *)
let rec infer (ctx : context) (e : exp) : typ =  match e with 
  
  | Int _ -> TInt 
  | Bool _ -> TBool 
  | If (e1, e2, e3) -> (unify (infer ctx e1) TBool; unify (infer ctx e2) 
                          (infer ctx e3); infer ctx e2 ) 
  | Primop (o, li) -> (match o with 
      | Negate -> (match li with 
          | [el] -> (unify (infer ctx el) TInt; TInt )
          | _ -> type_fail "Expected one argument to Negate" ) 
      | And | Or -> if (List.for_all 
                          (fun x -> (unify (infer ctx x) TBool; true)) li ) 
          then TBool else type_fail "Expected all arguments to be booleans" 
      | LessThan | LessEqual | GreaterEqual | GreaterThan | Equals | NotEquals 
        -> if (List.for_all (fun x -> (unify (infer ctx x) TInt; true)) li ) 
          then TBool else type_fail "Expected all arguments to be ints" 
      | Plus | Minus | Times | Div -> 
          if (List.for_all (fun x -> (unify (infer ctx x) TInt; true)) li ) 
          then TInt else type_fail "Expected all arguments to be ints" ) 
  | Tuple (tuplist) -> let tup = List.fold_left (fun x y -> x @ [infer ctx y]) 
                           [] tuplist in TProduct (tup)
  | Fn (n, ctx2, e1) -> (match ctx2 with
      | Some (t') -> TArrow (t', infer (extend ctx (n, t') ) e1)
      | None -> let a8' = fresh_tvar() in 
          let advanced = infer (extend ctx (n, a8') ) e1 in
          TArrow (infer (extend ctx (n, a8')) (Var n) , advanced) )
  | Rec (f, ctx2, e1) -> ctx2
  | Let (li,e1) ->
      let rec containsKey map k = let Ctx(m) = map in match m with
        | [] -> false
        | (key, _)::t -> if key = k then true else containsKey (Ctx (t)) k 
      in 
      let replacing list expr = 
        let rec replacing2 list2 expr2 = 
          match list2 with
          | [] -> expr2
          | i::t -> let (b, c) = i in replacing2 t (subst (b, c) expr2) 
        in replacing2 (List.rev list) expr in 
      let rec fold_ex l1z l2z replace = match l1z with 
        | [] -> (l2z, replace)
        | gx::tx -> (match gx with  
            | Val (ex, na) | ByName (ex, na) -> 
                if containsKey l2z na then let naz = fresh_var na in 
                  fold_ex tx (extend l2z (naz,infer l2z (replacing replace ex)) ) 
                    (replace @ [((Var (naz)), na)]) else 
                  fold_ex tx (extend l2z (na,infer l2z (replacing replace ex)) )
                    replace 
            | Valtuple (ex, na) -> (match infer l2z (replacing replace ex) with 
                | TProduct (l3) -> if List.length na <> List.length l3 then
                      type_fail "Tuple sizes need to be the same"
                    else let rec combinehelp l1x l2x l3x rep = 
                           match (l1x, l2x) with 
                           | ([], _) -> (l3x, rep)
                           | (g::t), (i::h) -> 
                               if containsKey l3x g then let gz = fresh_var g in 
                                 combinehelp t h (extend l3x (gz,i) ) 
                                   (rep @ [((Var (gz)), g)] ) 
                               else combinehelp t h (extend l3x (g, i)) rep 
                           | _ -> (l3x,rep) 
                      in let (l3x', rep') = combinehelp na l3 l2z replace in 
                      fold_ex tx l3x' rep' 
                | _ -> type_fail "Needed a tuple" ) )  
      in let (l2z', rep') = fold_ex li ctx [] in infer l2z' (replacing rep' e1)
  | Apply (e1, e2) -> (match infer ctx e1 with 
      | TArrow (t1, t2) ->(unify (infer ctx e2) t1; t2 )
      | _ -> type_fail "Need a function to apply argument" ) 
  | Var (n) -> (try match ctx_lookup ctx n with
      | TVar (lim) -> (match !lim with
          | Some (lim') -> lim'
          | None -> ctx_lookup ctx n )
      | _ -> ctx_lookup ctx n
     with NotFound -> type_fail "Free variable" )
  | Anno (e1, t2') -> (unify (infer ctx e1 ) t2'; t2')
  
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
            print_string (name ^ " test #" ^ string_of_int idx ^ " success\n");
            print_string (stringify output ^ " = " ^ stringify expected_output)
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
