(** Interpreter for the language. *)

open Syntax;;	(* ADT, EVT, types and env *)
open Utils;;	(* Facilities and auxiliary functions *)
open Eval_ops;;
open Exceptions;;

(**
	Interpreter that implements dynamic taint analysis.
  Note: type annotations are here ignored: they are already checked by the type checker.
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
		with |_ ->	raise(Unsupported_Primitive("eval:Bop of "^op
                ^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| Var x  -> lookup env x 
	| Let(x, _, eRhs, letBody) ->
		let xVal = eval ~into_block:into_block eRhs env t in
		let letEnv = (x, xVal) :: env in
		eval ~into_block:into_block letBody letEnv t
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
		(match eval ~into_block:into_block eFun env t with
		| Closure (f, x, fBody, fDeclEnv) as fClosure, f_t 
		| UntrustedBlock((Closure (f, x, fBody, fDeclEnv)) as fClosure, f_t), _ ->
			if f_t = Taint then failwith"";
			let xVal, xTaint = eval ~into_block:into_block eArg env t in
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
			in f tup [] Untaint
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
			in f l [] Untaint
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
	| IsEmpty(l) -> 
		let list, t' = eval ~into_block:into_block l env t in 
		(match list with
		| ListV([]) -> Bool(true), t'
		| ListV(_) ->  Bool(false), t'
		| _ -> raise (Type_system_Failed("eval:IsEmpty - "^(string_of_value list)
						^" at Token: "^(string_of_loc (e.loc) ) ) ) )
	| NativeFunction(f, name_arg) ->
		( match name_arg with
		| Some x -> (* print primitives *)
			let v, t = lookup env x in f v, t
		| None	 -> (* get primitives *)
			f Unit, Taint 
		)
	| Trust(b) -> 
		if into_block <> No then raise (Type_Error("Cannot have nested blocks."))
		else 
			if t = Taint then raise (Security_Error("(Possible) malicious access to trusted block. Abort."))
			else 
				let v' = eval_trusted b env [] Untaint in 
				TrustedBlock(v'), t
	| Access(tb, field) -> 
		let tbv, _ =  eval ~into_block:into_block tb env t in 
		( match tbv, field.value with 
		| TrustedBlock(tb_env), Var(id) -> 
			if t = Taint then raise (Security_Error("(Possible) malicious access to trusted block. Abort."))
			else List.assoc id tb_env |> fst
		| _,_ -> raise(Type_system_Failed("Access op. with wrong types at: "^(string_of_loc e.loc)))
		)
	| Secret(_) -> 
		raise (Error_of_Inconsistence("eval: unexpected secret data outside trusted block: "^(string_of_loc e.loc) ))
	| Handle(_) -> 
		raise (Error_of_Inconsistence("eval: unexpected handled exp outside trusted block: "^(string_of_loc e.loc) ))
	| Plugin(e) ->
		if into_block <> No then raise (Type_Error("Cannot have nested blocks."))
		else
			let v' = eval ~into_block:Untrusted e start_env Taint in 
			UntrustedBlock(v'), t

(* Evaluates a trusted block of expression to an <ide -> value * confidentiality> environment 
 * note: the only constructs possible in a trusted block are (also secret) declaration and handle
 *)
and eval_trusted	(e : located_exp) (env :(value * integrity) env) 
									(tb : ((value * integrity) * confidentiality) env) (t : integrity) 
											: ((value * integrity) * confidentiality) env = 
	if t = Taint then raise(Security_Error("Trusted block in taint status. Abort."));
	match e.value with
	| Let(x, _, eRhs, letBody) -> (* evaluates rhs, adds to env and tb and eval(_trusted) the body *)
		let xVal = 
			( match eRhs.value with
			| Secret(s) -> let v, _ = eval ~into_block:Trusted s env t in v, t
			| Trust(_) -> raise (Type_system_Failed("Cannot have nested blocks."))
			| _ -> eval ~into_block:Trusted eRhs env t
			) in 
		let letEnv = (x, xVal) :: env in
		let tb' = (x, (xVal, Private))::tb in 
		eval_trusted letBody letEnv tb' t
	| Handle(l) -> (* for each item i, adds (i, (eval i, Public)) to tb *)
		let add_f (f : located_exp) = 
			(match eval ~into_block:Trusted f env t with
			| Closure(name,_,_,_) as c, t -> (name, ((c,t), Public))
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