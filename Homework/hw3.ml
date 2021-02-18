(* TODO: Write some tests for tabulate. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let tabulate_tests: (((int -> int) * int) * int list) list = [
  (* Remember: your test cases should have this form:
     ((f, n), output)
     The following test case asserts that:
       tabulate (fun x -> x) (-1)
     should have the output []
  *)
  (((fun x -> x), -1), []);
  (((fun x -> x + 2), 3), [2; 3; 4; 5]);
  (((fun x -> x + 2), 0), [2])
  
]

(* TODO: Implement dist_table: (int * int) -> int -> float list *)
let dist_table ((marblesTotal, marblesDrawn): (int * int)) (x: int) : float list =
  
  let dist2 n = dist_black n x (marblesTotal, marblesDrawn) in
  
      
  tabulate dist2 marblesTotal
  
  

(* TODO: Write some test cases for is_empty. *)
let is_empty_tests: (float list list * bool) list = [
  ([[]], true);
  ([], true);
  ([[]; [3.0]; [4.0]], false)
]

(* TODO: Implement is_empty: 'a list list -> bool *)
let is_empty (matrix: 'a list list) : bool =
  List.for_all (fun a -> a = []) matrix

(* TODO: Implement dist_matrix: int * int -> int list -> float list list *)
let dist_matrix ((total, drawn): int * int) (resultList: int list) : float list list =
  
  List.map(fun x -> dist_table(total, drawn) x) resultList

(* TODO: Implement combined_dist_table: float list list -> float list *)
let rec combined_dist_table (matrix: float list list) = 
  if is_empty matrix then [] else 
    match matrix with 
    | [x] -> x
    | x::xs -> List.fold_left (fun a b -> List.map2 (fun c d -> c *. d) a b ) x xs 
  

(* Once you have implemented all the above functions, you can
   use this function to compute the maximum likelihood.
   You can try it on the given example by running:
     max_likelihood (6, 3) [2; 0; 1]
*)
let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
       (dist_matrix (total, drawn) resultList))


(* TODO: Implement all: (ingredients list -> bool) -> cake -> bool *)
let rec all (p: (ingredients list -> bool)) (c: cake) : bool = 
  
  match c with 
  
  | Slice(x) -> p x
  | Cake(x, y) -> all p x && all p y

(* TODO: Write some test cases for is_chocolate_cake. *)
let is_chocolate_cake_tests = [
  
  (Cake (Slice [Chocolate ; Flour], Cake (Slice [Chocolate ; Almonds] , Slice [Chocolate ; BlackBeans])), true);
  (Cake(Slice[Chocolate], Slice[Orange]), false);
  (Cake(Slice[Orange], Slice[Orange]), false);
  (Slice[Chocolate], true);
  (Slice[Orange; Chocolate], true);
  (Slice[], false)
  
]

(* TODO: Implement is_chocolate_cake: cake -> bool *)
let is_chocolate_cake (c: cake) : bool = 
  
  
  all (fun x -> List.exists(fun y -> match y with | Chocolate -> true | _ -> false) x) c


(* TODO: Implement map: (ingredients list -> ingredients list) -> cake -> cake *)
let rec map (p: (ingredients list -> ingredients list)) (c: cake) = 
  match c with 
  
  | Slice(x) -> Slice(p x)
  | Cake(x, y) -> Cake(map p x, map p y)

(* TODO: Write some test cases for add_ingredient. *)
let add_ingredient_tests = [
  ((Chocolate, Cake(Slice[Orange], Slice[Chocolate])), Cake(Slice([Orange; Chocolate]), Slice([Chocolate])))
]

(* TODO: Implement add_ingredient: ingredients -> cake -> cake *)
let add_ingredient (x: ingredients) (c: cake) : cake = 
  
  map (fun y -> insert x y) c
          

(* TODO: Implement fold_cake: (ingredients list -> 'a -> 'a) -> 'a -> cake -> 'a  *)
let rec fold_cake (f: (ingredients list -> 'a -> 'a)) (base: 'a) (c: cake) : 'a = 
  
  match c with 
  
  | Slice(x) -> f x base
  | Cake (x,y) -> fold_cake f (fold_cake f base y) x


(* TODO: Implement get_all_ingredients: cake -> ingredients list *)
let get_all_ingredients (c: cake) : ingredients list = 
  
  let union2 l2 l1 = union l1 l2
  
  in fold_cake union2 [] c
    
    


