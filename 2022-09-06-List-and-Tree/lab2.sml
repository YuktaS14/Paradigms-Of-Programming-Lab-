(* LISTS AND TREES
   _________________*)

(* 
Q1.
   map: (a->b) -> a list->b list 

*)

fun map f [] = []
  | map f (x::xs) = (f x):: map f xs
				
(*val add4 = fn x => x+4
val ans = add4
*)


(*
Q2.
  binary tree
*)

datatype 'a tree = nulltree
                 | node of ('a tree * 'a * 'a tree)
			       

					 
(*
Q3.
  treemap : ('a-> 'b) -> 'a tree -> 'b tree
*)

fun treemap f nulltree = nulltree
  | treemap f (node(lt,r,rt)) = node(treemap f lt, f r, treemap f rt)

(*
Q4.
  inorder: 'a tree -> 'a list
  preorder: 'a tree -> 'a list
  postorder: 'a tree -> 'a list

  createLeafNode : 'a -> 'a tree

*)

(*fun concat [] y = [y]
  | concat (x::xs) y = x :: concat xs y 
*)

fun inorder nulltree = []
  | inorder (node(lt,r,rt)) = inorder lt @ (r:: inorder(rt))

					       
fun createLeafNode x = node(nulltree,x,nulltree)
val t1 = createLeafNode 1
val t2 = createLeafNode 3
val t3 = createLeafNode 5
val t4 = createLeafNode 4
val t12 = node(t1,7,t2)
val t34 = node(t3,2,t4)
val root = node(t12,6,t34)

fun preorder nulltree = []
  | preorder (node(lt,r,rt)) = (r::preorder(lt)) @ preorder(rt)

fun postorder nulltree = []
  | postorder (node(lt,r,rt)) = (postorder(lt) @ postorder(rt)) @ [r]  
			 
    
(*
Q5.
  rotClock : 'a tree -> 'a tree

*)

fun rotClock (node((node(llt,ln,lrt)),r,rt)) = (node(llt,ln,((node(lrt,r,rt)))))
  | rotClock x = x

		     
(*

Checking functions
val checkInorder = inorder root
val checkPreorder = preorder root
val checkPostOrder = postorder root
val checkRotClock = rotClock root

val ans = preorder checkRotClock
*)
