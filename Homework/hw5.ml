(* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* We've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  
  (({nodes = ["v1";"v2";"v3"];
     edges = [("v1","v2",1);("v1","v3",2)]}, "v1"), [("v2",1);("v3",2)]);
  (({nodes = ["v1";"v2";];
     edges = [("v1","v2",1)]}, "v1"), [("v2",1)]);
  (({nodes = [];
     edges = []}, ""), []);
  (({nodes = ["v1";"v2";"v3"];
     edges = [("v3","v2",1);("v1","v3",2)]}, "v1"), [("v3",2)]);
  
  
]

(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list = 
  
  List.fold_left (fun neighbors edge -> 
      
      let (v,v2,w) = edge in 
      if v = vertex then neighbors @ [(v2,w)] else neighbors
      
      
      
    ) [] g.edges
  
  
  

(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  let rec aux_node node visited sum = match neighbours g node with

    

    | [] -> raise Fail

    | [(vertex,weight)] -> if vertex = b then (visited @ [vertex]), 

                                              (sum + weight)

        else 

        if not(List.mem vertex visited) then 

          aux_node vertex (visited @ [vertex]) (sum + weight)

        else raise Fail 

    | _ -> aux_list (neighbours g node) visited sum 

  
    
  and aux_list nodes visited sum = match nodes with
    
    | [] -> raise Fail 
    | (x,weight)::xs -> 
        
        if x = b then (visited @ [x]), 
                      (sum + weight)
        else 
        
        if not(List.mem x visited) then 
          try (aux_node x (visited @ [x]) (sum + weight)) 
          with Fail -> aux_list xs (visited) (sum)
        else aux_list xs visited sum
                       
  
  in
  aux_node a [a] 0

(* TODO: Implement find_path'. *)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =

  let rec aux_node node visited sum fc sc = match neighbours g node with

    | [] -> fc ()

    | [(vertex, weight)] -> if vertex = b then sc  ( (visited @ [vertex]), 

                                                     (sum + weight) )

        else
          
        if not(List.mem vertex visited) then 
          aux_node vertex (visited @ [vertex]) (sum + weight) fc sc
        else fc()
    | _ -> aux_list (neighbours g node) visited sum fc sc 

  
  and aux_list nodes visited sum fc sc = match nodes with

    | [] -> fc ()

    | (x,weight)::xs -> 

        if x = b then sc ( (visited @ [x]), 

                           (sum + weight) )

        else 

        if not(List.mem x visited) then 

          aux_node x (visited @ [x]) (sum + weight) 

            (fun () -> aux_list xs (visited) (sum) fc sc) sc 

        else aux_list xs visited sum fc sc 

  in aux_node a [a] 0 (fun() -> raise Fail) (fun x -> x)
  


(* TODO: Implement find_all_paths *)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list = 
  let rec aux_node node visited sum = match neighbours g node with 
    
    | [] -> []
    
    | [(vertex, weight)] -> if vertex = b then [(visited @ [vertex]), 

                                                (sum + weight)]
                                                 
                                               
        else 

        if not(List.mem vertex visited) then 
          aux_node vertex (visited @ [vertex]) (sum + weight)
            
        else []
            
    | _ -> aux_list (neighbours g node) visited sum


    
  and aux_list nodes visited sum = match nodes with
    
    | [] -> []
    | (x,weight)::xs -> 
        
        if x = b then [(visited @ [x]), 

                       (sum + weight)] @ aux_list xs (visited) (sum)
        else 
        
        if not(List.mem x visited) then 
          aux_node x (visited @ [x]) (sum + weight) @ aux_list xs (visited) (sum)
          
        else aux_list xs visited sum
    
  in aux_node a [a] 0
  


(* TODO: Implement find_shortest_path *)
let find_shortest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option = 
  
  match find_all_paths g a b with
  
  | [] -> None
  | somelist -> Some ( List.hd (
      List.sort (fun x y -> let (_,num1) = x and (_,num2) = y in
                  if num1 > num2 then 1
                  else if num2 > num1 then -1
                  else 0
      
                ) somelist) )
  
  