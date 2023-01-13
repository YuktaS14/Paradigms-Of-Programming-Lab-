(* lab6 *)

(*
Q1.
*)

datatype expr = VAR of Atom.atom
        | Apply of expr * expr
        | Lambda of Atom.atom * expr


(*Q2
  free: expr -> atomSet
  bound: expr -> atomSet
*)

type atomSet = AtomRedBlackSet.set
fun free (VAR x) = AtomSet.singleton(x) : atomSet
  | free (Apply (expr1, expr2)) = AtomSet.union( free(expr1), free(expr2))
  | free (Lambda (x, expr)) = AtomSet.difference( free(expr), free(VAR(x))) 


fun bound (VAR x) = AtomSet.empty : atomSet
  | bound (Apply (expr1, expr2)) = AtomSet.union( bound(expr1), bound(expr2))
  | bound (Lambda (x, expr)) = AtomSet.add(bound(expr),x) 




(* Q3
  subst : expr -> Atom.atom -> expr -> expr
*)
			  
fun subst (VAR y) x e = if Atom.same( x,y )
			then
			    e
			else
			    (VAR y)

  | subst (Apply (e1,e2)) x e = let val e1new = subst e1 x e
				    val e2new = subst e2 x e
				in
				    Apply (e1new, e2new)
				end
  | subst (Lambda (y,e1)) x e = if Atom.same( x,y )
				then
				    Lambda (y, e1)
				else
				    Lambda (y, (subst e1 x e))


(*Q4
   fresh : atom set -> Atom.atom
   diagA : string -> Atom.atom -> string
   diag = string -> string -> string
   helper = Atom.atom * string -> string
*)

					 
fun diag x y = case((explode x), (explode y)) of ([],[]) => "a"
		| ([],(x::xs)) => if (str x) = "a"
		         	  then
				      "b"
			          else
	                	      "a"
		| (u,[]) =>  (implode u)
		| ((x::xs),(y::ys)) => if (str x) = (str y)
				      then
					  implode(x::(explode (diag (implode xs) (implode ys))))
				      else
					  (implode [x])^(diag (implode xs) (implode (y::ys)))
							    
	      	   
fun diagA x s = diag x (Atom.toString(s))

fun helper (s,x) = diagA x s 
fun fresh (atomS: atomSet) = Atom.atom (AtomSet.foldl helper "" atomS)


(* Testing:


val items = [ Atom.atom("a"), Atom.atom("ba"),Atom.atom("yukta"), Atom.atom("cd"), Atom.atom("babaa"), Atom.atom("reeee"),Atom.atom("bab"), Atom.atom("babaaaaa"),Atom.atom("xyz"),Atom.atom("zx"), Atom.atom("baba"),Atom.atom("aaaa")]

val setv = AtomSet.fromList items
			    
val ans = fresh setv
val ansstr = Atom.toString ans *)
			   
