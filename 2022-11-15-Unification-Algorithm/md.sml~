signature SIGNATURE = sig

    type symbol
    val arity   : symbol -> int
    val compare : symbol * symbol -> order

end


structure TypeSig : SIGNATURE = struct
    
    datatype symbol = INT | BOOL | ARROW

    fun arity INT       = 0
        |   arity BOOL  = 0
        |   arity ARROW = 2

    fun toINT INT       = 0
        |   toINT BOOL  = 1
        |   toINT ARROW = 2

    fun compare (s1, s2) = Int.compare ( toINT s1, toINT s2 )

end


signature SIG =  sig
    type term
    type telescope
    type equation
    val checkRecursion : telescope -> term -> term -> bool
    val unify : telescope option -> equation -> telescope option
    val unifyList : telescope option -> (term * term) list -> telescope option
    val unifyVar : telescope option -> Atom.atom -> term -> telescope option
end

functor Unify ( structure S : SIGNATURE ) : SIG = struct 

    datatype term = VAR of Atom.atom
    | APPLY of S.symbol * term list

    type telescope = term AtomMap.map
    type equation  = term * term


    (* checkRecursion : telescope -> term -> term -> bool *)
    fun checkRecursion (tel : telescope) (VAR X) ( VAR Y ) = if Atom.same (X , Y) then true
            else 
                (case AtomMap.find( tel, Y ) of
                        SOME value => checkRecursion tel (VAR X) (Option.valOf(AtomMap.find(tel , Y)))
                    | NONE => false)

        |   checkRecursion (tel : telescope) ( VAR X ) ( APPLY ( f , args ) ) = let val l = List.filter (checkRecursion tel (VAR X) ) args
            in 
                if List.null l then false
                else true
            end


    (* unify : equation -> telescope option -> telescope option *)
    fun unify (tel : telescope option) (S_equation_T : equation) = 
            case S_equation_T of
                ( VAR X, t )                             => unifyVar tel X t
            | ( s, VAR y )                               => unifyVar tel y s
            | ( APPLY ( f, fargs ) , APPLY ( g, gargs) ) => let fun get (x::xs) (y::ys) = (x,y)::(get xs ys)
                in
                    unifyList tel (get fargs gargs)
                end

    and

        (* unifyList : telescope option -> (term * term) list -> telescope option *)
        unifyList (tel : telescope option) (equations : equation list)  = let
                fun FUN (x, y) = unify y x
            in
                List.foldl FUN tel equations
            end

    and

        (* unifyVar : telescope option -> Atom.atom -> term -> telescope option *)
        unifyVar (tel : telescope option) (X : Atom.atom) (t : term) = let val l = checkRecursion (Option.valOf tel) (VAR X) t 
            in
                if l then Option.NONE
                else Option.SOME ( AtomMap.insert (Option.valOf tel, X, t) )
            end
end
