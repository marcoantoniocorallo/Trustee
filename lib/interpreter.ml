(** Interpreter for the language. *)

open Syntax;;	(* ADT, EVT, types and env *)
open Utils;;	(* Facilities and auxiliary functions *)
open Eval_ops;;
open Exceptions;;

(**
 * Interpreter that implements dynamic taint analysis.
 * Note: type annotations are here ignored: they are already checked by the type checker.
 * @params:	into_block: keep trace of where we are: Trusted, Untrusted, Outside of blocks
 *					start_env: the starting def env, where blocks are type-checked
 *					e: expression to type-check
 *					env: environment
 *					t : integrity level context						
 *)
let rec eval ?(into_block=No) ?(start_env=(Native_functions.env)) (e : located_exp) 
							(env : (value * integrity) env) (t : integrity) : (value * integrity) = 
	match e.value with
	| Empty -> Unit, t
	| CstI i -> Int i, t
	| CstB b -> Bool b, t
	| CstF f -> Float f, t
	| CstC c -> Char c, t
	| CstS s -> String s, t
	| Uop(op, x) -> 
		(try 
			let v, t = eval ~into_block:into_block x env t in 
			eval_uop op v, t
		with |_ ->	raise(Unsupported_Primitive("eval:Uop of "^op^" at Token: "^(string_of_loc (e.loc) ) ) ))
	| Bop(e1, op, e2) -> 
    let v1, t1 = eval ~into_block:into_block e1 env t in 
    let v2, t2 = eval ~into_block:into_block e2 env t in 
    (try eval_bop v1 op v2, (t1 ++ t2)
		with |Unsupported_Primitive(s) ->	raise(Unsupported_Primitive("eval:Bop of "^s
                ^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Var x  -> lookup env x 
	| Let(x, _, eRhs, letBody) ->
		let xVal, t' = eval ~into_block:into_block eRhs env t in
		let letEnv = (x, (xVal, t')) :: env in
		let v, t'' = eval ~into_block:into_block letBody letEnv t in 
		v, (t ++ t'')
	| If(e1, e2, e3) ->
		let v1, t1 = eval ~into_block:into_block e1 env t in 
		(match v1 with
		| Bool true -> 
			let v2, t2 =  eval ~into_block:into_block e2 env t in
			v2, t1 ++ t2
		| Bool false -> 
			let v3, t3 = eval ~into_block:into_block e3 env t in
			v3, t1 ++ t3
		| _ ->  raise (Type_system_Failed("eval:If non-bool guard - "
            ^(string_of_value v1)^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Fun(f, x, _, fBody) -> Closure(f, x, fBody, env), t
	| Call(eFun, eArg) ->
		let f', t' = eval ~into_block:into_block eFun env t in 
		let xVal, xTaint = eval ~into_block:into_block eArg env t in
		(match f', t', xVal with
		| Closure (f, x, fBody, fDeclEnv) as fClosure, f_t, _
		| UntrustedBlock((Closure (f, x, fBody, fDeclEnv)) as fClosure, f_t), _, _ ->
			let fBodyEnv = (x, (xVal, xTaint)) :: (f, (fClosure, f_t)) :: fDeclEnv in 
			let f_res, t_res = eval ~into_block:into_block fBody fBodyEnv t in 
			f_res, t_res ++ f_t ++ xTaint
		| _ ->  raise (Type_system_Failed("eval:Call: a function was expected! at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Tup(tuple) ->
		let evaluateTuple tup = 
			let rec f tup acc taint = match tup with
				| [] -> Tuple(List.rev acc), taint
				| x::xs -> 
					let xv, xt = eval ~into_block:into_block x env t in 
					f xs (xv::acc) (taint ++ xt)
			in f tup [] t
		in evaluateTuple tuple
  | Proj(tup,i) -> 
    let tuple, tt = eval ~into_block:into_block tup env t in 
    let index, it = eval ~into_block:into_block i env t in 
    (match tuple, index with 
    | Tuple(tup), Int n -> get tup n, tt ++ it
    | _, _ -> raise (Type_system_Failed("eval:Proj a tuple and an integer was expected - "
    ^(string_of_value tuple)^" - "^(string_of_value index)^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Lst(list) -> 
		let evaluateList l = 
			let rec f l acc taint = match l with
				| [] -> ListV(List.rev acc), taint
				| x::xs -> 
					let xv, xt = eval ~into_block:into_block x env t in 
					f xs (xv::acc) (taint ++ xt)
			in f l [] t
		in evaluateList list
	| Cons_op(e, l) ->
		let v1, t1 = eval ~into_block:into_block e env t in 
		let v2, t2 = eval ~into_block:into_block l env t in
		(match v1, v2 with
		| x, ListV(xs) -> ListV(x::xs), t1 ++ t2
		| _,_ ->  raise (Type_system_Failed("eval:cons a list was expected - "^(string_of_value v1)
              ^" - "^(string_of_value v2)^" at Token: "^(string_of_loc (e.loc) ) ) ) 
		)
	| Head(l) ->
		let list, t' = eval ~into_block:into_block l env t in 
		(match list with
		| ListV(x::_) -> x, t'
		| _ ->  raise (Type_system_Failed("eval:Head - "^(string_of_value list)
            ^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Tail(l) -> 
		let list, t' = eval ~into_block:into_block l env t in 
		(match list with
		| ListV(_::xs) -> ListV(xs), t'
		| _ ->  raise (Type_system_Failed("eval:Tail - "^(string_of_value list)
            ^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| NativeFunction(f, name_arg) ->
		( match name_arg with
		| Some x -> (* print primitives *)
			let v, t = lookup env x in f v, t
		| None	 -> (* get primitives *)
			f Unit, Taint 
		)
	| Trust(_, b) -> 
		if into_block <> No then raise (Type_Error("Cannot have nested blocks."))
		else 
			if t = Taint then raise (Security_Error("(Possible) malicious access to trusted block. Abort."))
			else 
				let v' = eval_trusted b start_env [] Untaint in 
				TrustedBlock(v'), t
	| Access(tb, field) -> 
		let tbv, _ =  eval ~into_block:into_block tb env t in 
		( match tbv, field.value with 
		| TrustedBlock(tb_env), Var(id) -> 
			if t = Taint then raise (Security_Error("(Possible) malicious access to trusted block. Abort."))
			else List.assoc id tb_env
		| _,_ -> raise(Type_system_Failed("Access op. with wrong types at: "^(string_of_loc e.loc)))
		)
	| SecretData(_) -> 
		raise (Error_of_Inconsistence("eval: unexpected secret data outside trusted block: "^(string_of_loc e.loc) ))
	| Handle(_) -> 
		raise (Error_of_Inconsistence("eval: unexpected handled exp outside trusted block: "^(string_of_loc e.loc) ))
	| PluginData(e) ->
		if into_block <> No then raise (Type_Error("Cannot have nested blocks."))
		else
			let v' = eval ~into_block:Untrusted e start_env Taint in 
			UntrustedBlock(v'), t
	| Assert(p, taint_flag) -> 
		let v', t' = eval ~into_block:into_block p env t in
		if taint_flag then (* just assert if the expression p is taint *)
			if t' = Taint then raise(Assertion_Failure("Possible taint expression: "^(string_of_loc p.loc)))
			else Unit, t' ++ t
		else (* assert (as usual) the predicate p *)
			if v' = Bool true then Unit, t' ++ t
			else raise(Assertion_Failure(string_of_loc (p.loc)))
	| Declassify(e) -> eval ~into_block:into_block e env t 

(** Evaluates a trusted block of expression to an environment that keep trace of the integrity
 * note: the only constructs possible in a trusted block are declaration and handle
 * @params:	e: expression to type-check
 *          env: global type environment
 *          tb: environment of the trusted block, that this function returns
 *					t: integrity level context						
 *)
and eval_trusted	(e : located_exp) (env : (value * integrity) env) 
									(tb : (value * integrity) env) (t : integrity) 
											: (value * integrity) env = 
	if t = Taint then raise(Security_Error("Trusted block in taint status. Abort."));
	match e.value with
	| Let(x, _, eRhs, letBody) -> (* evaluates rhs, adds to env and tb and eval(_trusted) the body *)
		let xVal = 
			( match eRhs.value with
			| SecretData(s) -> let v, _ = eval ~into_block:Trusted s env t in v, t
			| Trust(_) -> raise (Type_system_Failed("Cannot have nested blocks."))
			| _ -> eval ~into_block:Trusted eRhs env t
			) in 
		let letEnv = (x, xVal) :: env in
		let tb' = (x, xVal)::tb in 
		eval_trusted letBody letEnv tb' t
	| Handle(l) -> (* for each item i, adds (i, (eval i, Public)) to tb *)
		let add_f (f : located_exp) = 
			(match eval ~into_block:Trusted f env t with
			| Closure(name,_,_,_) as c, t -> (name, (c,t))
			| _ -> raise(Type_system_Failed("eval_trusted: not-function value in handle at: "^(string_of_loc e.loc)))
			) in		
		(List.map (add_f) l)@tb
	| other ->	raise (Error_of_Inconsistence("eval_trusted: unexpected construct! "
							^(Syntax.show_exp other)^" at: "^(string_of_loc e.loc) ))
;;

let eval (e : located_exp) : value = 
	let v, t = eval e (Native_functions.env) Untaint in
	if t = Taint then print_endline "Warning: the computed value can be tainted"; 
	v
;;