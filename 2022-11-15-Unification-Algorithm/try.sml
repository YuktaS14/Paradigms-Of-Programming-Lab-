
(* Define the type of type variables *)
type typevar = int
signature SIGNATURE = sig
    type symbol
    val compare : symbol * symbol -> order
    val arity : symbol -> int
end


(* Basic types are booleans and integers *)
structure TypeSig : SIGNATURE = struct

    datatype symbol = INT
		    | BOOL
		    | ARROW

    fun arity INT   = 0
      | arity BOOL  = 0
      | arity ARROW = 2

    fun toInt INT   = 0
      | toInt BOOL  = 1
      | toInt ARROW = 2

    fun compare (s1, s2) = Int.compare (toInt s1, toInt s2)

end


signature VAR = sig
    type var
    type ord_key = var
    val compare : var * var -> order
end

functor Unify (
    structure S : SIGNATURE
    structure V : VAR

) = struct

   datatype term = Var of V.var
	         | Apply of S.symbol * term list


   structure VarMap = RedBlackMapFn(V)
   (*
    A telescope {x₁ <- t₁ ...., xₙ <- tₙ } is captured

   as a map between Atoms to terms.

   *)

   type telescope = term VarMap.map
   type equation  = term * term

   fun checkRecursion (tel : telescope) (x : V.var)(t : term) = false

   fun unify (tel : telescope) (sEQt : equation) : telescope =
       case sEQt of
	   (Var x , t    )                          => unifyVar tel x t
	 | (s     , Var y)                          => unifyVar tel y s
	 | (Apply (f, fargs) , Apply (g, gargs) )   => VarMap.empty
   and unifyVar (tel : telescope) (x : V.var) (t : term) : telescope = VarMap.empty

   and unifyList (tel : telescope) (eqns : equation list)  : telescope = VarMap.empty

end
(* Define the type of types *)
type typ =
    | Var of typevar
    | Arrow of typ * typ

(* Define the type of substitution maps, which map type variables to types *)
type subst = (typevar * typ) list

(* Define a function to apply a substitution to a type *)
fun apply (s: subst, t: typ) =
    (* Use a function to recursively apply the substitution to the type *)
    let
        fun apply_rec (s: subst, t: typ) =
            (* If the type is a variable, look up its substitution in the map *)
            (* If the variable is not in the map, return the type itself *)
            | Var x => List.lookup (op =) s x |> Option.getOrElse (Var x)
            (* If the type is an arrow, apply the substitution to its arguments *)
            | Arrow (t1, t2) => Arrow (apply_rec (s, t1), apply_rec (s, t2))
    in
        (* Apply the substitution to the type using the recursive function *)
        apply_rec (s, t)
    end

(* Define a function to compose two substitutions *)
fun compose (s1: subst, s2: subst) =
    (* Use a function to recursively compose the substitutions *)
    let
        fun compose_rec (s1: subst, s2: subst) =
            (* If the first substitution is empty, return the second one *)
            | [] => s2
            (* If the first substitution is non-empty, apply it to the second one *)
            | (x, t) :: s1' => (x, apply (s2, t)) :: compose_rec (s1', s2)
    in
        (* Compose the substitutions using the recursive function *)
        compose_rec (s1, s2)
    end

(* Define a function to unify two types using the unification algorithm *)
fun unify (t1: typ, t2: typ) =
    (* Use a function to recursively unify the types *)
    let
        fun unify_rec (t1: typ, t2: typ) =
            (* If the types are equal, return an empty substitution *)
            | (t1, t2) => []
            (* If the first type is a variable, return a substitution that maps it to the second type *)
            | (Var x, t) => [(x, t)]
            (* If the second type is a variable, return a substitution that maps it to the first type *)
            | (t, Var x) => [(x, t)]
            (* If the types are arrows, unify their arguments and return the resulting substitution *)
            | (Arrow (t11, t12), Arrow (t21, t22)) => compose (unify_rec (t11, t21), unify_rec (t12, t22))
    in
        (* Unify the types using the recursive function *)
        unify_rec (t1, t2)
    end

(* Define a function to infer the type of a lambda expression *)
fun infer (e: expr)











signature SIGNATURE = sig
    type symbol
    val compare : symbol * symbol -> order
    val arity : symbol -> int
end


(* Basic types are booleans and integers *)
structure TypeSig : SIGNATURE = struct

    datatype symbol = INT
		    | BOOL
		    | ARROW

    fun arity INT   = 0
      | arity BOOL  = 0
      | arity ARROW = 2

    fun toInt INT   = 0
      | toInt BOOL  = 1
      | toInt ARROW = 2

    fun compare (s1, s2) = Int.compare (toInt s1, toInt s2)

end


signature VAR = sig
    type var
    type ord_key = var
    val compare : var * var -> order
end

functor Unify (
    structure S : SIGNATURE
    structure V : VAR
) = struct

   datatype term = Var of V.var
	         | Apply of S.symbol * term list


   structure VarMap = RedBlackMapFn(V)
   (*
    A telescope {x₁ <- t₁ ...., xₙ <- tₙ } is captured

   as a map between Atoms to terms.

   *)

   type telescope = term VarMap.map
   type equation  = term * term

   fun checkRecursion (tel : telescope) (x : V.var)(t : term) = false

   fun unify (tel : telescope) (sEQt : equation) : telescope =
       case sEQt of
	   (Var x , t    )                          => unifyVar tel x t
	 | (s     , Var y)                          => unifyVar tel y s
	 | (Apply (f, fargs) , Apply (g, gargs) )   => VarMap.empty
   and unifyVar (tel : telescope) (x : V.var) (t : term) : telescope = VarMap.empty

   and unifyList (tel : telescope) (eqns : equation list)  : telescope = VarMap.empty

end
