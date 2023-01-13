(*
Q1.
*)

signature SORT = sig
    type t
    val sort: t list -> t list
end

signature ORD_KEY =
sig
    type ord_key
    val compare: ord_key* ord_key ->order
end




functor QSort ( O: ORD_KEY ) : SORT = struct
type t = O.ord_key
fun sort [] = []
	| sort (z::zs) =
    let
	fun f x = case O.compare (x,z) of
		      LESS => true
		    | EQUAL => true
		    | GREATER => false
	 val (left,right) = List.partition f zs
    in
	 (sort left) @ [z] @ (sort right)
    end
end


					  


			   
(*
Q2.
*)
			  
structure IntOrd : ORD_KEY = struct
type ord_key = int
val compare = Int.compare
end

structure IntQSort = QSort(IntOrd) 

