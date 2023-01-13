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
  
val depth = ref 0
				     
functor Unify (structure S: SIGNATURE) = struct
         datatype term = Var of Atom.atom
	               | Apply of S.symbol * term list

         type telescope = term AtomMap.map
         type equation = term * term
	 type equationsList = term list * term list
					       
         fun checkRecursion (tel: telescope) (Var(x)) (Var(y)) (d:int) = if ((Atom.same (x,y)) andalso (d=0 or d=1)) then true
									 else
									     let val nd= !depth
										 val _ = depth := nd+1
									     in
										 (case (AtomMap.find(tel, y)) of
										      SOME v => checkRecursion (tel:telescope) (Var(x):term) (v:term) (nd+1)
										    | NONE => false)
	   | checkRecursion (tel: telescope) (Var(x)) (Apply(f, ls)) = let val l = List.filter (checkRecursion tel (Var(x))) ls 
								       in
									   if List.null l then false
									   else
									       true
								       end


	 fun helper (tel:telescope option) (x: Atom.atom) (t: term)= let val fl = checkRecursion (Option.valOf(tel)) (Var(x)) t 0
								       in
									   if (fl) then Option.NONE
									   else
									       Option.SOME (AtomMap.insert(Option.valOf tel, x, t))
									end								    

	   fun unify (tel: telescope option) (sEqt: equation) =
	       case sEqt of
		   (f, Var(x))  => helper tel x f
		 | (Var(y),g) => helper tel y g
		 | (Apply (f,ls), Apply(g,lst)) =>  if S.compare(f,g) = EQUAL
						     then
							 unifyList tel (ls,lst)
						     else
							 NONE
	   and

	   unifyList (tel: telescope option) (eqns: equationsList) = case eqns of
									 ([],[]) => tel
								       | (x::xs, y::ys) => (case unify tel (x,y) of
												SOME z => unifyList tel (xs,ys)
											      | NONE => NONE)
								       | (_,_) => NONE
										      
	  
	   end
