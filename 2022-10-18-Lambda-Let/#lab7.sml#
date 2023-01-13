(*Lab7 *)

(*Q1*)


datatype lambda = Var of Atom.atom
        | Apply of lambda * lambda
        | Lambda of Atom.atom * lambda
			      
				    
datatype lambda_let = Let of Atom.atom * lambda_let * lambda_let
		    | Var_let of Atom.atom
		    | Apply_let of lambda_let * lambda_let
		    | Lambda_let of Atom.atom * lambda_let


datatype lambda_letrec = Letrec of Atom.atom * lambda_letrec * lambda_letrec
		       | Var_letRec of Atom.atom
		       | Apply_letRec of lambda_letrec * lambda_letrec
		       | Lambda_letRec of Atom.atom * lambda_letrec


(*Q2
   letToCal : lambda_let -> lambda 
*)
	     
fun letToCal (Let (x, e1, e2)) = Apply (Lambda (x, letToCal e2), letToCal e1)
  | letToCal (Var_let x) = Var x
  | letToCal (Apply_let (e1,e2)) = Apply (letToCal e1, letToCal e2)
  | letToCal (Lambda_let (x,e1)) = Lambda (x, letToCal e1)
				  
