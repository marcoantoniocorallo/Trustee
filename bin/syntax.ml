(** Syntax of the language : definition of AST, EVT, types and Environment *)

type 'v env = (string * 'v) list
[@@deriving show]

let rec lookup env x =
  match env with
  | []        -> raise(Exceptions.Binding_Error(x ^ " not found"))
  | (y, v)::r -> if x=y then v else lookup r x
;;

type ide = string
[@@deriving show]

(** Located node *)
type 'a located = { loc : Lexing.position * Lexing.position [@opaque]; value : 'a}
[@@deriving show]

(** Algebraic Data Types *)
type exp =
	| Empty 																					(* Empty Expression *)
	| CstI of int                              	 			(* Integer constants *)
	| CstB of bool                               			(* Boolean constants *)
	| CstF of float															 			(* Float constants *)
	| CstC of char															 			(* Char literals *)
	| CstS of string														 			(* String literals *)

	| Uop of ide * located_exp												(* Unary operators *)
	| Bop of located_exp * ide * located_exp         	(* Binary operators *)

	| Var of ide                              	 			(* Variables/Identifiers *)
	| Let of ide * ttype option * located_exp * located_exp        	
                                                    (* Typed declaration *)
	| If of located_exp * located_exp * located_exp   (* If-then-else *)
	| Fun of ide * ide * ttype * located_exp 					(* Fun expr (f, x, type of f, fBody)  *)
	| Call of located_exp * located_exp               (* Fun application *)

	| Tup of located_exp list			 				 	 					(* Heterogeneous Fixed-length list of expressions *)
	| Proj of located_exp * located_exp               (* i-th element of tuple *)

	| Lst of located_exp list 		 					 					(* Homogeneous List of expressions *)
	| Cons_op of located_exp * located_exp						(* Concatenates an exp in head of a list *)
	| Head of located_exp															(* Return the first element of a list *)
	| Tail of located_exp															(* Return the list without the first el *)
	| NativeFunction of ( value -> value ) * ide option 
																										(* (ocaml code, arg_name) *)
	| Trust of ide * located_exp											(* Trust block of code and data *)
	| SecretData of located_exp												(* Secret expression *)
	| Handle of located_exp list											(* Interface fn between trusted and untrusted code *)
	| Access of located_exp * located_exp							(* Access to trusted block name *)
	| PluginData of located_exp												(* Untrusted block of code and data *)
	| Assert of located_exp * bool										(* Snd arg tell if we want to test the taintness *)
																										(* assert(x, true) fail if x is taint *)
	[@@deriving show]

and located_exp = exp located                 			(* ( exp * location ) *)
	[@@deriving show]

(** Types definition *)
and ttype = 
	| Tunit																						(*  Type unit *)
  | Tint                                            (*  Type int *)
  | Tbool                                           (*  Type bool *)
  | Tfloat                                          (*  Type float *)
  | Tchar                                           (*  Type char *)
  | Tstring                                         (*  Type string *)
  | Tfun of ttype * ttype                           (*  Type of function *)
  | Ttuple of ttype list                        		(*  Compound type: tuple *)
  | Tlist of ttype option                           (*  Compound type: list *)
	(* Block of data and code are associated to type environments *)
	|	TtrustedBlock of ((ttype * confidentiality)	env	[@opaque])
	| TuntrustedBlock of ttype
	[@@deriving show]

(** Expressible and denotable values. *)
and value =
	| Unit																									(*	evaluation of an empty program *)
	| Int of int
	| Bool of bool
	| Float of float
	| Char of char
	| String of string
	| Closure of string * string * located_exp * (value * integrity) env			
																													(*	(f, x, fBody, fDeclEnv) *)
	| Tuple of value list   																(*	Heterogeneous fixed-length tuple of values*)
	| ListV of value list   																(*	Homogeneous list of values *)
	(* Block of data and code are associated to value environments *)
	| TrustedBlock of ((value * integrity) env	[@opaque])
	| UntrustedBlock of (value * integrity)
	[@@deriving show]

and confidentiality = 
	| Top 									
	| Public | Secret of ide	(* Data (of a given block) subject to information flow *)
	| Private									(* Non-secret data and non-handled functions in trusted blocks *)
	| Bottom									(* Public *)
	[@@ deriving show]

(* Taintness *)
and integrity = 
	| Taint
	| Untaint
	[@@deriving show]

(* Auxiliary type to keep trace of where the control flow is *)
and block = No | Trusted | Untrusted
	[@@deriving show]
;;

let (++) (t1 : integrity) (t2 : integrity) : integrity = 
	if t1 = t2 then t1 else Taint;;

let join e e' = 
	match e, e' with
	| c1, c2 when c1 = c2 -> c1
	| Secret _, Secret _
	| Public, Secret _ 
	| Secret _, Public
	| Top, _ 
	| _, Top 							-> Top 
	| Bottom , _ 					-> e'
	| _, Bottom 					-> e 
	| Secret i, Private 	
	| Private, Secret i 	-> Secret i
	| Public, Private
	| Private, Public 		-> Public
	| _ -> raise(Exceptions.Error_of_Inconsistence("Join combination unmatched: "^(show_confidentiality e)^" - "^(show_confidentiality e')))
;;