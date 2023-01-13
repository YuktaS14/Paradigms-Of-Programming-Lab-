(*
Q1.
*)			   
datatype DBLambda = Var of int
		  | App of DBLambda * DBLambda
		  | Lamb of DBLambda
				     
(*
Eg:
Lamb (Var (1)) 
Lamb (Lamb (Var (2))
Lamb (App(Var(1), Lamb(Var(2))))
*)


(*
Q2
  nthAux = 'a list * int -> 'a Find
  nth = Atom.atom list * int -> Atom.atom
  diag =  string -> string -> string
  diagA = string -> Atom.atom -> string
  helper = Atom.atom * string -> string
  fresh =  atomSet -> Atom.atom
  DBtoLambda = DBLambda -> Atom.atom list -> lambda
  printAns = lambda -> unit
*)

type atomSet = AtomRedBlackSet.set
		   
datatype lambda = VAR of Atom.atom
        | Apply of lambda * lambda
        | Lambda of Atom.atom * lambda


datatype 'a option = NONE | SOME of 'a

datatype 'a Find = LookingFor of int
				 | Found of 'a


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

fun nth (xs : Atom.atom list,y) =  case (nthAux (xs, y)) of
					Found(z) => z
				  | LookingFor(z) => Atom.atom("0")


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

	     
fun DBtoLambda (Var (x)) nls = let val ansList = nls
			   in
			       VAR(nth (ansList,x-1))
			   end
  | DBtoLambda (App (e1,e2)) nls =  Apply (DBtoLambda e1 nls, DBtoLambda e2 nls)
					  
  | DBtoLambda (Lamb (e1)) nls = let val newl = fresh(AtomSet.fromList(nls))::nls
				 in
				     Lambda (fresh (AtomSet.fromList(nls)), DBtoLambda e1 newl)
	 			 end

			
fun printAns (VAR (x)) = (print (Atom.toString(x)); print (" "))
  | printAns (Lambda (x, e)) = (print("Lambda "); printAns (VAR(x)); print (". ");printAns(e))			
  | printAns (Apply (e1,e2)) = (print("Apply of ("); printAns (e1); print (". "); printAns(e2); print (") "))


(*
Testing:
________
 printAns (DBtoLambda (Lamb (Var (1))));								  
 printAns (DBtoLambda (Lamb (Lamb (Var (2)))));
 printAns DBtoLambda ((Lamb (App (Var (1), Lamb (Var (2))))));
*)
