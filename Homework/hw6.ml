(*------------------ Q1------------------ *)
let rec parseExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  parseSExp
    toklist
    (fun toklist' exp -> match toklist' with
       | SEMICOLON :: toklist'' -> sc toklist'' exp
       | _ -> raise (Error "Expected a single semicolon"))

and parseSExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  
  parsePExp
    toklist
    (fun toklist' exp -> match toklist' with
       
       | PLUS :: toklist'' -> parseSExp toklist''
                                (fun toky exp2 -> sc (toky) 
                                    (Sum(exp, exp2)))
                                
       | SUB :: toklist'' -> parseSExp toklist'' 
                               (fun toky exp2 -> sc (toky) 
                                   (Minus(exp, exp2)))
  
       | toklist''-> sc toklist'' exp
         
    
    )
  
  

and parsePExp (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  
  
  parseAtom
    toklist
    (fun toklist' exp -> match toklist' with
       
       | TIMES :: toklist'' -> parsePExp toklist''
                                 (fun toky exp2 -> sc (toky) 
                                     (Prod(exp, exp2)))
                                 
       | DIV :: toklist'' -> parsePExp toklist'' 
                               (fun toky exp2 -> sc (toky) 
                                   (Div(exp, exp2)))
                               
       | toklist''-> sc toklist'' exp
         
    
    )
  

and parseAtom (toklist: token list) (sc: (token list -> exp -> 'a)) : 'a =
  
  match toklist with 
  
  | INT(n) :: toklist' -> sc toklist' (Int n)
                            

  | LPAREN::toklist'' -> parseSExp
                           toklist''
                           (fun toklist' exp -> match toklist' with
                              | RPAREN :: tok -> sc tok exp
                              | _ -> raise (Error "Expected a single right 
                                                   parenthesis"))
  
                           
  | _ -> raise (Error "Wrong")
   

           
(* parse : string -> exp *)
let parse string =
  parseExp
    (lex string)
    (fun s e -> match s with
       | [] -> e
       | _ -> raise (Error "Incomplete expression"))

(* eval : string -> int *)
let eval e = eval' (parse e) 

(* ---------- Hamming Numbers ----------- *)

let rec merge (s1: 'a str) (s2: 'a str) : 'a str = 
  
  {
  
    hd = if s1.hd > s2.hd then s2.hd 
      else s1.hd;
      
    tl = delay (fun() -> if s1.hd > s2.hd then merge s1 (force s2.tl)
                 else if s2.hd > s1.hd then merge (force s1.tl) s2
                 else merge (force s1.tl) (force s2.tl))
  
  }

let rec hamming_series = 
  
  
  {
    
    hd = 1;
    
    tl = Susp (fun() -> 
        merge (times 2 hamming_series) (merge (times 3 hamming_series) 
                                          (times 5 hamming_series)))    
    
          
  }
  
  
  

    