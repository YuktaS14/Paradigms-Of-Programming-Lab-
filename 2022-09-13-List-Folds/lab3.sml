(*
Q1
  foldr : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary

  foldl : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary
*)



fun foldl f s0 [] = s0
  | foldl f s0 (x::xs) =  foldl f (f (x,s0)) xs

fun foldr f s0 [] = s0
  | foldr f s0 (x::xs) = f (x, (foldr f s0 xs))    


(*
Q2
	sum: int list -> int
*)

fun sumInt (x,y) = x+y
val sum = foldl sumInt 0
val value = sum [1,2,3,4,5]

(*
Q3.a
	partition: ('a -> bool) -> 'a list -> 'a list * 'a list
	summary = 'a list * 'a list
*)

(*
fun partition pr [] = ([],[])
  | partition pr (x::xs) =   let 
  								val (first,second) = partition pr xs
  							 in
  							 	if (pr x)
  							 	then (x:: first, second)
  							 	else (first, x::second)
  							 	end;

*)


fun partition pr (x::xs) = let
								fun helper (e,(y,ys)) = 
								if (pr e)
								then
									(e::y, ys)
								else
									(y, e::ys)					   
							in 
							    foldr helper ([],[]) (x::xs)
							end

(*
Q4.b
	map: (a->b) -> a list->b list
	fun map f [] = []
	  | map f (x::xs) = (f x):: map f xs
	  
	
	summary = 'b list
*)

fun map f (x::xs) = let
						fun helper (e, ls) =
							(f e) :: ls				
					in
						foldr helper [] (x::xs)
					end

(*
Q4.c
	reverse : 'a list -> 'a list
	summary: 'a list
*)

fun reverse l = let
					fun helper (e,ls) =
						e :: ls							
				in
					foldl helper [] l
				end
					
(*
Q4.d
	nth : 'a list * int -> 'a option
 nthAux : 'a list * int -> 'a Find
 summaryfun (for nthAux): 'a * 'a Find -> 'a Find
 summary : 'a Find
*)

(*
fun nth ([] , y) = NONE
  | nth (x::xs, 0) = SOME x
  | nth (x::xs, y) = nth (xs, y-1)
  
  
fun nthAux ((x::xs), z) = LookingFor z-1
  | nthAux ((x::xs), 0) = Found x
*)

datatype 'a option = NONE | SOME of 'a

datatype 'a Find = LookingFor of int
				 | Found	  of 'a


fun nthAux (xs, z) = let
							fun helper (u, LookingFor(v)) = if( v = 0)
													   then
													   	Found(u)
													   else
													   	LookingFor(v-1)
							| helper(u, Found(v)) = Found(v)
					in
						  	foldl helper (LookingFor(z)) xs
					end

fun nth (xs,y) =  case (nthAux (xs, y)) of
					Found(z) => SOME(z)
				  | LookingFor(z) => NONE

		
	  		
						
