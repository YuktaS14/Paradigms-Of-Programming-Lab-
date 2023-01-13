(* Expression Evaluator 
 -----------------------*)

(*Q1*)

datatype Expr = Const of real
	       | Var of string
	       | Plus of Expr * Expr
	       | Mul of Expr * Expr

datatype Stmt = Assign of string * Expr
	       | Print of Expr

type Program = Stmt list



(*Q2*)

type Env = real AtomMap.map
  
		
(* val eval      : Env -> Expr -> real option
    (* Evaluates an expression. The result is real option to take care of
	   cases when there are undefined variables *)
   val execute   : Env -> Stmt -> Env
    (* Executes a single statement and returns the modified environment *)
   
   val interpret : Program -> unit 
    (* Run the program starting with an empty environment
	This is essentially a fold from the left.*)
 *)

fun eval _ (Const x) = SOME x			      
  | eval (env:Env) (Var x) = let val atomX = Atom.atom x
			     in
				 AtomMap.find(env, atomX)
			     end
				 
  | eval (env:Env) (Plus (x,y)) = let val result =
				    case (eval env x) of
					NONE => NONE
				      | (SOME v1) => case (eval env y) of
							NONE => NONE
						      | (SOME v2) => SOME (v1+v2)
			    in
				result
                            end
  | eval (env:Env) (Mul (x,y)) = let val result =
				  case (eval env x) of
					NONE => NONE
				      | (SOME v1) => case (eval env y) of
							NONE => NONE
						      | (SOME v2) => SOME (v1*v2)
			    in
				result
                           end

fun execute (env:Env) (Assign (x,y)) =  let val result = 
                                          case (eval env y) of
					       NONE => env
					     | (SOME v) => AtomMap.insert(env, (Atom.atom x), v)
				  in
				      result
				  end
				       
  | execute (env:Env) (Print x) = let val result =
				    case (eval env x) of
					NONE => ((print "None\n");env)
				      | (SOME v) => ((print (Real.toString v));(print "\n");env)
			    in
				result
			    end
				
val p1 : Program = [(Assign ("a",Mul (Const 3.0, (Plus (Const 5.0, Const 1.0))))), (Assign ("b", Mul (Plus (Const 2.0, Const 6.0), Var "a"))),(Print (Var "a")),(Print (Var "b")),(Print (Plus (Const 4.0, Const 8.0)))]

(*Inorder to use foldl, using helper function to convert type of execute 
from :(Env -> Stmt -> Env) to :( (Stmt * Env) -> Env )
*)
		       
fun helper f (x,y)  = f y x

fun interpret program = let val envMap : Env = AtomMap.empty
			    val exec =  helper execute				    
			in
			    foldl exec envMap program;()
			end


