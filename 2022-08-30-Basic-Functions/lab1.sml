(*LAB 1: BASIC FUNCTIONS: *)
(*________________________*)


(*
Q1

 curry : (a*b*c ->d) -> (a->b->c->d)
 uncurry: (a->b->c->d) -> (a*b*c ->d)
*)



fun curry f (x,y,z) = f x y z
fun uncurry f x y z = f (x,y,z)

(*fun add x y z = x+y+z*)


(*
Q2
	fst: (a*b) -> a
	snd: (a*b) -> b
*)

fun fst (x,y) = x
fun snd (x,y) = y


(*
Q3
	length : a list->a
*)

fun length [] = 0
  | length (x::xs) = 1+ length xs



(*
Q4
	reverse : a list -> a list

	1,2,3 ---> 3,2,1 

	1,2,3 []
	2,3  [1]
	3  [2 1]
	[] [3,2,1]
*)

fun helper [] (x::xs) = helper [x] xs
  | helper ys (x::xs) = helper (x:: ys) xs
  | helper ys [] = ys


fun reverse (x::xs) = helper [] (x::xs)
 



(*
q5
	fib: int -> int

	1- 1         5,8,13..
	2- 1			
	3- 2
	4- 3


	fib (1) = 1; fib (2) = 1; fib (n) = fib (n-1) + fib (n-2)
*)



fun helper x y 0 = x
  | helper x y n = helper (x+y) x (n-1)
  
  
 fun fib n = helper 0 1 n





















