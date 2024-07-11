(** Type system: type algorithm with a type annotation for the definition of function
 *  the definition of types is in file exp.ml
 *)

open Exceptions
open Syntax
open Utils

let rec (<=) (t1 : ttype) (t2 : ttype) : bool = match t1, t2 with
	| t1', t2' when t1'=t2' -> true
	| Tlist(None), Tlist(_) -> true
	| Tlist(Some t1'), Tlist(Some t2') when t1' <= t2' -> true
	| Ttuple(l1), Ttuple(l2) when List.length l1 = List.length l2 -> List.for_all2 (<=) l1 l2
  | TtrustedBlock(_), TtrustedBlock(_)  (* block types are not comparable *)
  | TuntrustedBlock(_), TuntrustedBlock(_) -> false
	| _ -> false

(** The type environment.  
 *  It contains the type of primitives binary operators.
 *)
let type_env = [
  "+",  Tfun(Tint, Tfun(Tint, Tint));         (* int -> int -> int *)
  "-",  Tfun(Tint, Tfun(Tint, Tint));         (* int -> int -> int *)
  "/",  Tfun(Tint, Tfun(Tint, Tint));         (* int -> int -> int *)
  "*",  Tfun(Tint, Tfun(Tint, Tint));         (* int -> int -> int *)
  "%",  Tfun(Tint, Tfun(Tint, Tint));         (* int -> int -> int *)
  "+.", Tfun(Tfloat, Tfun(Tfloat, Tfloat));   (* float -> float -> float *)
  "-.", Tfun(Tfloat, Tfun(Tfloat, Tfloat));   (* float -> float -> float *)
  "/.", Tfun(Tfloat, Tfun(Tfloat, Tfloat));   (* float -> float -> float *)
  "*.", Tfun(Tfloat, Tfun(Tfloat, Tfloat));   (* float -> float -> float *)
  "&&", Tfun(Tbool, Tfun(Tbool, Tbool));      (* bool -> bool -> bool *)
  "||", Tfun(Tbool, Tfun(Tbool, Tbool));      (* bool -> bool -> bool *)
  "^",  Tfun(Tstring, Tfun(Tstring, Tstring));(* string -> string -> string *)
  "get_int", Tfun(Tunit, Tint);
  "get_float", Tfun(Tunit, Tfloat);
  "get_bool", Tfun(Tunit, Tbool);
  "get_char", Tfun(Tunit, Tchar);
  "get_string", Tfun(Tunit, Tstring);
  "print_int", Tfun(Tint, Tunit);
  "print_float", Tfun(Tfloat, Tunit);
  "print_bool", Tfun(Tbool, Tunit);
  "print_char", Tfun(Tchar, Tunit);
  "print_string", Tfun(Tstring, Tunit);
];;

type block = No | Trusted | Untrusted;;

(** Typing rule in a given type environment gamma *)
let rec type_of ?(into_block=No) ?(start_env=type_env) (gamma : ttype env) (e : located_exp) : ttype =
  match e.value with
  | Empty -> Tunit
  | CstI(_) -> Tint
  | CstB(_) -> Tbool
  | CstF(_) -> Tfloat
  | CstC(_) -> Tchar
  | CstS(_) -> Tstring
  | Uop(op, x) -> 
    ( match op, (type_of ~into_block:into_block gamma x) with
    | "!", Tbool -> Tbool
    | "!", _ -> raise (Type_Error ("Not of non-bool type - at Token: "^(string_of_loc (e.loc))))
    | "-", t when t = Tint || t = Tfloat -> t
    | "-", _ -> raise (Type_Error ("Not of non-number type - at Token: "^(string_of_loc (e.loc))))  
    | _, _ -> raise (Unsupported_Primitive(op))
    )
  | Var(x)  -> lookup gamma x 
  (* Define equality and comparison for each simple type *)
  | Bop(e1, "=", e2)
  | Bop(e1, "<", e2)
  | Bop(e1, "<=", e2)
  | Bop(e1, ">", e2)
  | Bop(e1, ">=", e2)
  | Bop(e1, "<>", e2) ->
    let t1 = type_of ~into_block:into_block gamma e1 in
    let t2 = type_of ~into_block:into_block gamma e2 in
    ( match t1, t2 with
    | Ttuple(_), Ttuple(_)
    | Tlist(_), Tlist(_) ->     raise (Type_Error ("Equality of compound values"
                                ^(string_of_loc (e.loc))))
    | Tfun(_,_), Tfun(_,_) ->   raise (Type_Error ("Equality of functional values"
                                ^(string_of_loc (e.loc))))
    | t1', t2' when t1' = t2' -> Tbool
    | _, _ -> raise (Type_Error ("Error in the arguments of equality"^(string_of_loc (e.loc))))
    )
  | Bop(e1, op, e2) ->
    let t1 = type_of ~into_block:into_block gamma e1 in
    let t2 = type_of ~into_block:into_block gamma e2 in
    let top = lookup gamma op in
    ( match top with
    | Tfun(t1', Tfun(t2', tr')) ->
      if (t1' = t1 && t2' = t2) then tr'
      else raise (Type_Error ("Error in the arguments of "^op^": "^(string_of_loc (e.loc))))
    | _ ->  raise(Error_of_Inconsistence("Inconsistence in Bop "^op^(string_of_loc (e.loc))))
    )
  | Let(x, t, e1, e2) ->
    ( match t with 
    | Some tt -> let t1 = type_of ~into_block:into_block gamma e1 in 
      if t1 = tt then type_of ~into_block:into_block ((x,t1)::gamma) e2
      else raise(Type_Error("Bad type annotation at "^(string_of_loc (e.loc))))
    | None -> let t1 = type_of ~into_block:into_block gamma e1 in type_of ~into_block:into_block ((x,t1)::gamma) e2
    )
  | If(e1, e2, e3) ->
    if (type_of ~into_block:into_block gamma e1) = Tbool then
      let t2 = type_of ~into_block:into_block gamma e2 in
      let t3 = type_of ~into_block:into_block gamma e3 in
      if t2 <= t3 then t2 
      else raise (Type_Error 
        ("\"If-Rule\": branches have different types: then is "^(string_of_ttype t2)^", else is "^(string_of_ttype t3)
        ^" - at Token: "^(string_of_loc (e.loc))))
    else
      raise (Type_Error ("\"If-Rule\": if with no a boolean guard"^(string_of_loc (e.loc))))
  | Fun(f, x, fun_type, body) ->
    ( match fun_type with 
      (Tfun(t1,t2) as t) ->
        let gamma' = (f, t) :: (x, t1) :: gamma in
        if (type_of ~into_block:into_block gamma' body) <= t2 then t
        else
        raise (Type_Error("Function return type does not match. "
            ^"Expected "^(string_of_ttype (type_of ~into_block:into_block gamma' body))^" got "
            ^(string_of_ttype t2)^" at "^(string_of_loc (e.loc))))
      | _ -> raise (Type_Error("Function type does not match"^(string_of_loc (e.loc))))
    )
  | Call(e1, e2) ->
    let t1 = type_of ~into_block:into_block gamma e1 in
    let t2 = type_of ~into_block:into_block gamma e2 in
    ( match t1 with
    | Tfun(tx, tr) as tfun ->
      if t2 <= tx then tr
      else raise (Type_Error("functional application: argument type mismatch"^(string_of_loc (e2.loc))
                  ^"function "^(string_of_ttype tfun)^" got "^(string_of_ttype t2)^" instead"))
    | _ -> raise (Type_Error("application to a non functional value"^(string_of_loc (e2.loc))))
    )
  | Tup(tuple) ->
    let type_of_tuple t = 
      let rec f t acc = match t with
        | [] -> Ttuple(List.rev acc)
        | x::xs -> f xs ((type_of ~into_block:into_block gamma x::acc))
      in f t []
    in type_of_tuple tuple
  | Proj(tup,i) ->
    let type_of_tuple = type_of ~into_block:into_block gamma tup in 
    let type_of_i = type_of ~into_block:into_block gamma i in 
    ( match type_of_tuple, type_of_i with
    | Ttuple(types), Tint -> 
      (match i.value with CstI x -> get types x 
      |_ -> raise(Type_Error("An int literal was expected in projection of tuple! "^" at Token: "^(string_of_loc (e.loc) ))) )
    | Ttuple(_), _ -> raise(Type_Error("An integer was expected in projection of tuple! "^" at Token: "^(string_of_loc (e.loc) )))
    | _, _ -> raise(Type_Error("A tuple was expected in projection of tuple! "^" at Token: "^(string_of_loc (e.loc) )))
    )
  | Lst(list) -> 
    ( match list with 
    | [] -> Tlist None 
    | x::_ -> Tlist (Some(type_of ~into_block:into_block gamma x)))
  | Cons_op(e, l) -> (* 'a -> 'a list -> 'a list *)
    let type_of_l = type_of ~into_block:into_block gamma l in 
    let type_of_e = type_of ~into_block:into_block gamma e in 
    ( match type_of_e, type_of_l with
    | t1, Tlist(Some t2) when t1 <= t2 -> type_of_l 
    | t1, Tlist(Some _) ->  raise(Type_Error("Type error: Cons between "
                            ^(string_of_ttype t1)^" and a "^(string_of_ttype type_of_l)
                            ^" at: "^(string_of_loc (e.loc))))
    | _, Tlist(None) -> Tlist(Some type_of_e)
    | _,_ ->  raise(Type_Error("Type error: Cons between "
              ^(string_of_ttype type_of_e)^" and a "^(string_of_ttype type_of_l)
              ^" at: "^(string_of_loc (e.loc))))
    )
  | Head(l) -> (* 'a list -> 'a *)
    let type_of_l = type_of ~into_block:into_block gamma l in 
    ( match type_of_l with
    | Tlist(Some t) -> t
    | Tlist(None) -> raise(Type_Error("Type error: attempting to pop an element from an empty list!"
                    ^(string_of_loc (l.loc))))
    | _ -> raise(Type_Error("Head of a non-list value!"^(string_of_loc (l.loc))))
    )
  | Tail(l) -> (* 'a list -> 'a *)
    let type_of_l = type_of ~into_block:into_block gamma l in 
    ( match type_of_l with
    | Tlist(Some t) -> Tlist(Some t)
    | Tlist(None) -> raise(Type_Error("Type error: attempting to tail an empty list!"
                                    ^(string_of_loc (e.loc))))
    | _ -> raise(Type_Error("Tail of a non-list value!"^(string_of_loc (l.loc))))
    )
  | IsEmpty(l) -> (* 'a list -> bool *)
    ( match type_of ~into_block:into_block gamma l with
    | Tlist(_) -> Tbool
    | _ -> raise(Type_Error("Check emptiness of a non-list value!"^(string_of_loc (l.loc))))
    )
  | NativeFunction(_) ->
    raise ( Error_of_Inconsistence("type system: !!! Prohibit use of Native Functions !!! at: "^(string_of_loc e.loc)))
  | Trust(b) -> 
    if into_block = No then TtrustedBlock(type_of_trusted b gamma [])
    else raise (Type_Error("Cannot have nested blocks."))
  | Access(tb, field) -> 
    ( match type_of ~into_block:into_block gamma tb, field.value with
    | TtrustedBlock(env), Var(id) -> (* Only public functions can be accessed in trusted blocks *)
      ( match List.assoc_opt id env with
      | Some (Tfun(_) as t ,Public) -> t
      | Some (_, Private) -> raise (Type_Error("Secret field cannot be accessed. Error at: "^(string_of_loc e.loc)))
      | Some (_, Public) -> raise(Type_system_Failed("type_of:access: found public non-function object; at "^(string_of_loc e.loc)))
      | None -> raise (Type_Error("Field "^id^" not found in block at: "^(string_of_loc tb.loc)))
      )
    | _, Var(_) -> raise (Type_Error("A block was expected in access operation at: "^(string_of_loc tb.loc)))
    | _, _ -> raise (Type_Error("An identifier was expected in access operation at: "^(string_of_loc field.loc))) )
  | Secret(_) -> (* Is not possible to have secret data outside trusted blocks - syntax constraint *)
    raise (Error_of_Inconsistence("type_of: unexpected secret data outside trusted block: "
    ^(string_of_loc e.loc) ))
  | Handle(_) -> (* Is not possible to have handled exp outside trusted blocks - syntax constraint *)
    raise (Error_of_Inconsistence("type_of: unexpected handled exp outside trusted block: "
    ^(string_of_loc e.loc) ))
  | Plugin(e) -> (* eval in an empty env *)
    if into_block = No then 
      let v = type_of ~into_block:Untrusted start_env e in
      (match v with
      | Tfun(_) -> TuntrustedBlock(v)
      | _ -> raise(Type_Error("A plugin must implement a function. At: "^(string_of_loc e.loc)))
      )
    else raise (Type_Error("Cannot have nested blocks."))

(* Evaluates a trusted block of expression to an <ide -> ttype * confidentiality> environment 
 * note: the only constructs possible in a trusted block are (also secret) declaration and handle
 *)
and type_of_trusted (e : located_exp) (env : ttype env) (tb : (ttype * confidentiality) env) : (ttype * confidentiality) env = 
  match e.value with
  | Let(x, t, e1, e2) -> (* checks rhs type, adds to env and tb and checks the body type *)
    let t' = 
      ( match e1.value with
      | Secret(s) -> 
        let ts = type_of ~into_block:Trusted env s in
        (match ts with  (* Only data can be secret *)
        | Tint
        | Tbool
        | Tfloat
        | Tchar
        | Tstring
        | Ttuple(_)
        | Tlist(_) -> ts
        | _ -> raise (Type_Error("Only data can be secret. At: "^(string_of_loc s.loc)))
        )
      | Trust(_) -> raise (Type_Error("Cannot have nested blocks. At: "^(string_of_loc e.loc)))
      | _ -> type_of ~into_block:Trusted env e1
      ) in 
    (match t with 
    | Some tt -> if t' = tt then type_of_trusted e2 ((x,t')::env) ((x, (t',Private))::tb)
      else raise(Type_Error("Bad type annotation at "^(string_of_loc (e.loc))))
    | None -> type_of_trusted e2 ((x,t')::env) ((x, (t',Private))::tb)
    )
  | Handle(l) -> (* for each item i, adds (i, (type_of i, Public)) to tb -> shadowing *)
    let add_f (f : located_exp) = 
      (match f.value with
      | Var(name) -> 
        (match List.assoc_opt name tb with  (* Check that the function has been defined in the block *)
        | Some (Tfun(_) as c,_) -> (name, (c, Public))
        | Some (_) -> raise(Type_Error("A Function was expected at: "^(string_of_loc f.loc)))
        | _ -> raise(Type_Error("Handled Function must be declared into the block. At: "^(string_of_loc f.loc)))
        )
      | _ -> raise(Type_Error("An identifier was expected at: "^(string_of_loc f.loc)))
      ) in		
    (List.map (add_f) l)@tb
  | other ->	raise (Error_of_Inconsistence("type_of_trusted: unexpected construct! "
              ^(Syntax.show_exp other)^" at: "^(string_of_loc e.loc) ))
;;

let type_check e = type_of type_env e