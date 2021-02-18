(* 
  Question 1 Table:
      Mom | Dad | Gender | Disease
      Yes | No  | Male   | True
      Yes | No  | Female | False
      Yes | Yes | -      | True
      No  | No  | -      | False
      No  | YEs | -      | False
*)

(* 
   a) Define types needed to describe a child with this family 
  
    Syntax:
    [ type NAME = type1 | type2 | ... ]
    
*)
type gender = Male | Female 
type disease = True | False 
type mom = disease
type dad = disease
type child = mom * dad * gender

(*
  b) Write a simple pattern matching function to match the table above.
    Note the function header is child -> bool
    
    Hint: Generalize the table above to be simpler
  
      Mom | Dad | Gender | Disease
      Yes | No  | Male   | True
      Yes | No  | Female | False
      Yes | Yes | -      | True
      No  | No  | -      | False
      No  | YEs | -      | False
  
    Mom | Dad | Gender | Disease
    Yes | No  | Male   | True
    Yes | Yes | -      | True
    Yes | No  | Female | False
    No  | -  | -      | False
    
*)

let childHasDisease (c:child) = match c with 
  | (True, False, Male) -> true 
  | (True, True, _) -> true 
  | (_, _, _) -> false ;;









(* ------------------------------------------------------------- *)

(* 

Question 2 Info: 

   Disease allele: D
   Non-disease allele: A
   Possible combinations: AA, AD, DD 
     (AD and DA is treated the same)

  AA -> normal 
  AD -> carrier, but healthy
  DD -> has cystic fibrosis
  
       A    D
  A   AA   AD
  
  D   AD   DD
  
  [ The child has equal probability of getting an allele from each parent]

*)

(* 
  a) Create a function that finds the probabilty 
    given the parents allele, the child will have cystic fibrosis. 

    
    Define the types needed: 

    type genotype: AA | AD | DD
    type person: genotype * genotype
    
  
   Function:
   person  -> float
   
   Helper Function:
   genotype -> float 
*)

type genotype =  AA | AD | DD
type person = genotype * genotype
  
(* Helper function *)
let probOfGettingD (g: genotype) = match g with 
  | AA -> 0.0 
  | AD -> 0.5
  | DD -> 1.0 ;;


let prob_disease (p: person) = match p with (x, y) 
  -> probOfGettingD(x) *. probOfGettingD(y);;
                                                        






(* ------------------------------------------------------------- *)



(*
   b) Tree traversal. Given a family tree, find out how many people have a 
     certain genotype in this family tree. 
      (i.e. Find the occurance of a value in a tree)

    Types:
    
      type genotype = AA | AD | DD
      type tree = Empty | Node of tree * genotype * tree
      let leaf x = Node (Empty, x, Empty)

   The tree will be in the form of 
        child
        /   \ 
  parent A  parent B

  Simple Tree:
  let tree1 = Node ( Node ( leaf AD, DD, leaf AD), AD, leaf AA)
let tree1 = Node ( Node ( Node( Empty, AD, Empty) , DD, leaf AD), AD, leaf AA)

        AD 
      /   \ 
    DD     AA
   / \
  AD  AD
  
   Function header: tree -> genotype -> int
   
   Ex: 
       countGenotype tree1 AA = 1
       countGenotype tree1 AD = 3
       countGenotype tree1 DD = 1
       
*) 

type genotype = AA | AD | DD
type tree = Empty | Node of tree * genotype * tree

let leaf x = Node (Empty, x, Empty);;

let rec countGenotype (t: tree) (g: genotype) = match t with
  | Empty -> 0
  | Node (p1, c, p2) -> 
      if (c = g) then 
        1 + (countGenotype p1 g) + (countGenotype p2 g) 
      else 
        (countGenotype p1 g) + (countGenotype p2 g) ;;

let tree1 = Node (Empty, AA, Empty) ;; (* leaf AA*)
let tree2 = Node ( Node( Empty, AD, Empty), AA, Empty);;

(*
  LS:  Node( Empty, AD, Empty)
  MD: AA
  RS: Empty
  
   AA
   / 
   
  LLS: Empty
  LMD: AD
  LRS: Empty
  
  AA
  /
AD
*)
let tree2 = Node ( leaf AD, AA, Empty)


