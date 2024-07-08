(** Interpreter for the language. *)

open Syntax;;	(* ADT, EVT, types and env *)
open Utils;;	(* Facilities and auxiliary functions *)
open Eval_ops;;
open Exceptions;;

(**
	Interpreter for expression. 
	Given a located_exp {e} and an enviroment {env} that closes {e},
	this function evaluates {e} and returns the result of the computation.
	Note: this function implements the big-step operational semantics with environment.
  Note: type annotations are here ignored: they are already checked by the type checker.
 *)
let rec eval ?(into_block=false) (e : located_exp) (env : value env) : value = match e.value with
	| Empty -> Unit
	| CstI i -> Int i
	| CstB b -> Bool b
	| CstF f -> Float f
	| CstC c -> Char c
	| CstS s -> String s
	| Uop(op, x) -> 
		(try (eval ~into_block:into_block x env) |> eval_uop op 
		with |_ ->	raise(Unsupported_Primitive("eval:Uop of "^op
              	^" at Token: "^(string_of_loc (e.loc) ) ) ))
	| Bop(e1, op, e2) -> 
    let v1 = eval ~into_block:into_block e1 env in 
    let v2 = eval ~into_block:into_block e2 env in 
    (try eval_bop v1 op v2 
		with |_ ->	raise(Unsupported_Primitive("eval:Bop of "^op
                ^" at Token: "^(string_of_loc (e.loc) ) ) )
		)
	| Var x  -> lookup env x
	| Let(x, _, eRhs, letBody) ->
		let xVal = eval ~into_block:into_block eRhs env in
		let letEnv = (x, xVal) :: env in
		eval ~into_block:into_block letBody letEnv
	| If(e1, e2, e3) ->
		let evaluated = eval ~into_block:into_block e1 env in 
		(match evaluated with
		| Bool true -> eval ~into_block:into_block e2 env
		| Bool false -> eval ~into_block:into_block e3 env
		| _ ->  raise (Type_system_Failed("eval:If non-bool guard - "
            ^(string_of_value evaluated)^" at Token: "^(string_of_loc (e.loc) ) ) )
		)
	| Fun(f, x, _, fBody) -> Closure(f, x, fBody, env)
	| Call(eFun, eArg) ->
		let fClosure = eval ~into_block:into_block eFun env in
		(match fClosure with
		| Closure (f, x, fBody, fDeclEnv) ->
			let xVal = eval ~into_block:into_block eArg env in
			let fBodyEnv = (x, xVal) :: (f, fClosure) :: fDeclEnv
			in eval ~into_block:into_block fBody fBodyEnv
		| _ ->  raise (Type_system_Failed("eval:Call: a function was expected! "
            ^(string_of_value fClosure)^" at Token: "^(string_of_loc (e.loc) ) ) )
		)
	| Tup(tuple) -> 
		let evaluateTuple t = 
			let rec f t acc = match t with
				| [] -> Tuple(List.rev acc)
				| x::xs -> f xs (eval ~into_block:into_block x env::acc)
			in f t []
		in evaluateTuple tuple
  | Proj(t,i) -> 
    let tuple = eval ~into_block:into_block t env in 
    let index = eval ~into_block:into_block i env in 
    (match tuple, index with 
    | Tuple(t), Int n -> get t n
    | _, _ -> raise (Type_system_Failed("eval:Proj a tuple and an integer was expected - "
    ^(string_of_value tuple)^" - "^(string_of_value index)^" at Token: "^(string_of_loc (e.loc) ) ) )
		)
	| Lst(list) -> 
		let evaluateList l = 
			let rec f l acc = match l with
				| [] -> ListV(List.rev acc)
				| x::xs -> f xs (eval ~into_block:into_block x env::acc)
			in f l []
		in evaluateList list
	| Cons_op(e, l) ->
		let v1 = eval ~into_block:into_block e env in 
		let v2 = eval ~into_block:into_block l env in
		(match v1, v2 with
		| x, ListV(xs) -> ListV(x::xs)
		| _,_ ->  raise (Type_system_Failed("eval:cons a list was expected - "^(string_of_value v1)
              ^" - "^(string_of_value v2)^" at Token: "^(string_of_loc (e.loc) ) ) ) 
		)
	| Head(l) ->
		let list = eval ~into_block:into_block l env in 
		(match list with
		| ListV(x::_) -> x
		| _ ->  raise (Type_system_Failed("eval:Head - "^(string_of_value list)
            ^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Tail(l) -> let list = eval ~into_block:into_block l env in 
		(match list with
		| ListV(_::xs) -> ListV(xs)
		| _ ->  raise (Type_system_Failed("eval:Tail - "^(string_of_value list)
            ^" at Token: "^(string_of_loc (e.loc) ) ) )
		)
	| IsEmpty(l) -> let list = eval ~into_block:into_block l env in 
	(match list with
	| ListV([]) -> Bool(true)
	| ListV(_) ->  Bool(false)
	| _ -> raise (Type_system_Failed("eval:IsEmpty - "^(string_of_value list)
					^" at Token: "^(string_of_loc (e.loc) ) ) )
	)
	| NativeFunction(f, name_arg) -> 
		( match name_arg with
		| Some x -> f (lookup env x)
		| None	 -> f Unit
		)
	| Trust(b) -> 
		if into_block = false then TrustedBlock(eval_trusted b env [])
		else raise (Type_Error("Cannot have nested blocks."))
	| Access(tb, field) -> 
		( match eval ~into_block:into_block tb env, field.value with 
		| TrustedBlock(tb_env), Var(id) -> List.assoc id tb_env |> fst
		| UntrustedBlock(b_env), Var(id) -> List.assoc id b_env
		| _,_ -> raise(Type_system_Failed("Access op. with wrong types at: "^(string_of_loc e.loc)))
		)
	| Secret(_) -> 
		raise (Error_of_Inconsistence("eval: unexpected secret data outside trusted block: "^(string_of_loc e.loc) ))
	| Handle(_) -> 
		raise (Error_of_Inconsistence("eval: unexpected handled exp outside trusted block: "^(string_of_loc e.loc) ))
	| Plugin(e) ->
		if into_block = false then UntrustedBlock(eval_untrusted e env [])
		else raise (Type_Error("Cannot have nested blocks."))

(* Evaluates a trusted block of expression to an <ide -> value * confidentiality> environment 
 * note: the only constructs possible in a trusted block are (also secret) declaration and handle
 *)
and eval_trusted (e : located_exp) (env : value env) (tb : (value * confidentiality) env) : (value * confidentiality) env = 
	match e.value with
	| Let(x, _, eRhs, letBody) -> (* evaluates rhs, adds to env and tb and eval(_trusted) the body *)
		let xVal = 
			( match eRhs.value with
			| Secret(s) -> eval ~into_block:true s env
			| Trust(_) -> raise (Type_system_Failed("Cannot have nested blocks."))
			| _ -> eval ~into_block:true eRhs env
			) in 
		let letEnv = (x, xVal) :: env in
		let tb' = (x, (xVal, Private))::tb in 
		eval_trusted letBody letEnv tb'
	| Handle(l) -> (* for each item i, adds (i, (eval i, Public)) to tb *)
		let add_f (f : located_exp) = 
			(match eval ~into_block:true f env with
			| Closure(name,_,_,_) as c -> (name, (c, Public))
			| _ -> raise(Type_system_Failed("eval_trusted: not-function value in handle at: "^(string_of_loc e.loc)))
			) in		
		(List.map (add_f) l)@tb
	| other ->	raise (Error_of_Inconsistence("eval_trusted: unexpected construct! "
							^(Syntax.show_exp other)^" at: "^(string_of_loc e.loc) ))

(* Evaluates a trusted block of expression to an <ide -> value * confidentiality> environment 
* note: the only constructs possible in a trusted block are (also secret) declaration and handle
*)
and eval_untrusted (e : located_exp) (env : value env) (b : value env) : value env = 
	match e.value with
	| Let(x, _, eRhs, letBody) -> 
    let xVal = eval ~into_block:true eRhs env in 
		let letEnv = (x, xVal) :: env in
		let b' = (x, xVal)::b in 
		eval_untrusted letBody letEnv b'
  | _ -> b  (* recall: the only possible exp in a plugin are declarations; 
              the other ones indicate the end of the block *)
;;

let eval (e : located_exp) : value = eval e Native_functions.env;;