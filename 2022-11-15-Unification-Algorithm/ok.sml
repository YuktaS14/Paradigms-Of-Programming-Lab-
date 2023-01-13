signature SIGNATURE = sig
    type symbol
    val compare : symbol * symbol -> order
    val arity : symbol -> int
end

(*Baisc types are booleans and integers*)			 
structure TypeSig : SIGNATURE  = struct
    datatype symbol = INT 
                    | BOOL
                    | ARROW

fun arity INT = 0
  | arity BOOL = 0
  | arity ARROW = 2

fun toINT INT = 0
  | toINT BOOL = 1
  | toINT ARROW = 2 

fun compare (s1, s2) = Int.compare(toINT s1, toINT s2)

end		      			  

signature VAR = sig
    type var
    val fresh : unit -> var
    val toString : var -> string
    val compare : var * var -> order
end


functor Term(structure S:SIGNATURE
	     structure V:VAR) = struct
    datatype term = symbol of S.symbol
                  | Var of V.var
	          | Apply of S.symbol * term list
					     
    fun present(symbol(f), x: V.var) = false
      | present (Var(y),x: V.var) = if V.compare (y,x) = EQUAL then true else false
      | present (Apply(f,ls), x:V.var) = let fun check ([],x) = false
					       | check (y::ys,x) = if present(y,x) then true else check(ys,x)
					 in
					     check(ls,x)
					 end	    
end

				    
functor Telescope(structure S:SIGNATURE
		  structure V:VAR) = struct
     structure term = Term(structure S=S structure V=V)
     structure map = AtomMap
     type telescope = term.term AtomMap.map
     type eq = term.term

 (*Substitute term with value present in telescope*)
     fun changeVal (tel: telescope, eq: eq) = case eq of
						  (term.symbol(f))=> term.symbol(f)
						| (term.Var(x)) => (let val y = Atom.atom(V.toString(x))
									 in term.Apply(f, check(tel, ls))
									 end

					 
     fun notPresent (tel : telescope, x: V.var, eq: eq)= case eq of
							    (term.symbol(f))=> true
							  | (term.Var(v)) => (let val y = Atom.atom(V.toString(x))
									      in
										  (case AtomMap.find(tel,y) of
										       SOME value => term.present(value,x) = false andalso notPresent(tel,x,value)
										     | NONE => true)
									      end)
							  | (term.Apply(f,ls)) => let fun check(t,x,[]) = true
									       | check(t,x,y::ys) = notPresent(tel,x,y) andalso check(t,x,ys)
									     in check(tel,x,ls)
									     end										
 
					  
end
								
					 
(*
A telescope (x1<-t1......Xn <- tn ) is captured
as a map between Atoms to terms
*)

(*
type telescope = term AtomMap.map
type equation = term * term
*)
				    
functor Unify (
    structure S : SIGNATURE
    structure V : VAR ) = struct

    structure telescope = Telescope(structure S=S structure V=V)
    structure term = telescope.term
    structure map=telescope.map
    
    type telescope= telescope.telescope
    type equation = term.term * term.term
    type equationsList = term.term list * term.term list
			
    fun unify (tel: telescope ) (sEqt: equation) =
	case sEqt of
	    (term.symbol(f1), term.symbol(f2)) => NONE
	  | (term.symbol(f), term.Var(x)) => unify (tel) (term.Var(x), term.symbol(f))
	  | (term.symbol(f1), term.Apply(f2,ls)) => NONE
	  | (term.Var(x), vl: term.term) => if telescope.notPresent(tel, x, vl) = false then NONE else SOME (map.insert(tel,Atom.atom(V.toString(x)), telescope.changeVal(tel,vl)))
	  | (term.Apply(f1,ls), term.symbol(f2)) => NONE
	  | (term.Apply(f,ls), term.Var(x)) => unify (tel) (term.Var(x), term.Apply(f,ls))
	  | (term.Apply(f1,ls1), term.Apply(f2,ls2)) => if S.compare(f1,f2) = EQUAL then unifyList (tel) (ls1,ls2) else NONE

    and unifyList (tel: telescope) (eqns: equationsList) = case eqns of ([],[]) => SOME tel
								      | (x::xs, y::ys) => (case unify tel (x,y) of
											       SOME z => unifyList tel (xs, ys)
											     | NONE => NONE )
							              | (_,_) => NONE
end					  

			      
(*
fun checkRecursion (tel : telescope) (x: Atom.atom) (t : term)  = false
								      
fun unifyVar  (tel : telescope) (x: Atom.atom) (t : term ) :telescope = AtomMap.empty
				
fun unifyList (tel : telescope) (eqns : equation list) : telescope = AtomMap.empty
    	
end
*)
