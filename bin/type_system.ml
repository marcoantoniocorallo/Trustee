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
  | TuntrustedBlock(t1'), TuntrustedBlock(t2') (* plugin wrap function types *)
  | TuntrustedBlock(t1'), t2'
  | t1', TuntrustedBlock(t2') -> t1' <= t2'
  | TtrustedBlock(_), TtrustedBlock(_)  (* block types are not comparable *)
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

let type_env = List.map (fun (i,t) -> (i, (t, Public))) type_env;;

(** Typing rule in a given type environment gamma *)
let rec type_of ?(into_block=No) ?(start_env=type_env) (gamma : (ttype * confidentiality) env)
                 (cxt : confidentiality) (e : located_exp) : (ttype * confidentiality) =
  match e.value with
  | Empty -> Tunit, cxt
  | CstI(_) -> Tint, cxt
  | CstB(_) -> Tbool, cxt
  | CstF(_) -> Tfloat, cxt
  | CstC(_) -> Tchar, cxt
  | CstS(_) -> Tstring, cxt
  | Uop(op, x) -> 
    let ty, c = type_of ~into_block:into_block gamma cxt x in 
    ( match op, ty with
    | "!", Tbool -> Tbool, c
    | "-", t when t = Tint || t = Tfloat -> t, c
    | "!", _ -> raise (Type_Error ("Not of non-bool type - at Token: "^(string_of_loc (e.loc))))
    | "-", _ -> raise (Type_Error ("Not of non-number type - at Token: "^(string_of_loc (e.loc))))  
    | _, _ -> raise (Unsupported_Primitive(op))
    )
  | Var(x)  -> 
    let tx, c = lookup gamma x in 
    (match tx with 
    | TuntrustedBlock(_) when into_block = Trusted ->
      raise(Security_Error("Type: Cannot access to plugin from inside trusted blocks."))
    | _ -> tx, c
    )
  (* Define equality and comparison for each simple type *)
  | Bop(e1, "=", e2)
  | Bop(e1, "<", e2)
  | Bop(e1, "<=", e2)
  | Bop(e1, ">", e2)
  | Bop(e1, ">=", e2)
  | Bop(e1, "<>", e2) ->
    let t1, c1 = type_of ~into_block:into_block gamma cxt e1 in
    let t2, c2 = type_of ~into_block:into_block gamma cxt e2 in
    ( match t1, t2 with
    | Ttuple(_), Ttuple(_)
    | Tlist(_), Tlist(_) ->     raise (Type_Error ("Equality of compound values"
                                ^(string_of_loc (e.loc))))
    | Tfun(_,_), Tfun(_,_) ->   raise (Type_Error ("Equality of functional values"
                                ^(string_of_loc (e.loc))))
    | t1', t2' when t1' = t2' -> Tbool, (join c1 c2)
    | _, _ -> raise (Type_Error ("Error in the arguments of equality"^(string_of_loc (e.loc))))
    )
  | Bop(e1, op, e2) ->
    let t1, c1 = type_of ~into_block:into_block gamma cxt e1 in
    let t2, c2 = type_of ~into_block:into_block gamma cxt e2 in
    let top, _ = lookup gamma op in
    ( match top with
    | Tfun(t1', Tfun(t2', tr')) ->
      if (t1' = t1 && t2' = t2) then tr', (join c1 c2)
      else raise (Type_Error ("Error in the arguments of "^op^": "^(string_of_loc (e.loc))))
    | _ ->  raise(Error_of_Inconsistence("Inconsistence in Bop "^op^(string_of_loc (e.loc))))
    )
  | Let(x, t, e1, e2) ->
    let t1, c1 = type_of ~into_block:into_block gamma cxt e1 in 
    let t2, c2 = type_of ~into_block:into_block ((x,(t1,c1))::gamma) cxt e2 in 
    ( match t with 
    | Some tt -> 
      if t1 = tt then t2, join c1 (join c2 cxt)
      else raise(Type_Error("Bad type annotation at "^(string_of_loc (e.loc))))
    | None -> t2, join c1 (join c2 cxt)
    )
  | If(e1, e2, e3) ->
    let t1, c1 = type_of ~into_block:into_block gamma cxt e1 in 
    if t1 = Tbool then
      let cxt' = join cxt c1 in 
      let t2, c2 = type_of ~into_block:into_block gamma cxt' e2 in
      let t3, c3 = type_of ~into_block:into_block gamma cxt' e3 in
      if t2 <= t3 then t2, join (join c1 cxt) (join c2 c3)
      else raise (Type_Error 
        ("\"If-Rule\": branches have different types: then is "^(string_of_ttype t2)^", else is "^(string_of_ttype t3)
        ^" - at Token: "^(string_of_loc (e.loc))))
    else raise (Type_Error ("\"If-Rule\": if with no a boolean guard"^(string_of_loc (e.loc))))
  | Fun(f, x, fun_type, body) ->
    ( match fun_type with 
      (Tfun(t1,t2) as t) ->
        let gamma' = (f, (t, Public)) :: (x, (t1, Public)) :: gamma in
        let t_res, cxt' = type_of ~into_block:into_block gamma' cxt body in 
        if (t_res) <= t2 then t, join cxt' cxt
        else
        raise (Type_Error("Function return type does not match. "
            ^"Expected "^(string_of_ttype (t_res))^" got "
            ^(string_of_ttype t2)^" at "^(string_of_loc (e.loc))))
      | _ -> raise (Type_Error("Function type does not match"^(string_of_loc (e.loc))))
    )
  | Call(e1, e2) ->
    let t1, c1 = type_of ~into_block:into_block gamma cxt e1 in
    let t2, c2 = type_of ~into_block:into_block gamma cxt e2 in
    ( match t1 with
    | Tfun(tx, tr) as tfun 
    | TuntrustedBlock(Tfun(tx, tr) as tfun) ->
      if t2 <= tx then 
        ( match tr with
        | TuntrustedBlock(tf) -> tf, join c1 c2
        | _ -> tr, join c1 c2
        )
      else raise (Type_Error("functional application: argument type mismatch"^(string_of_loc (e2.loc))
                  ^"function "^(string_of_ttype tfun)^" got "^(string_of_ttype t2)^" instead"))
    | other -> 
      raise (Type_Error("Function was expected, got "^(string_of_ttype other)^" at: "^(string_of_loc (e1.loc))))
    )
  | Tup(tuple) ->
    let type_of_tuple t = 
      let rec f t acc sec = match t with
      | [] -> Ttuple(List.rev acc), sec
      | x::xs -> 
        let xt, xc = type_of ~into_block:into_block gamma cxt x in 
        f xs (xt::acc) (join xc sec)
      in f t [] cxt
    in type_of_tuple tuple
  | Proj(tup,i) ->
    let type_of_tuple, conft = type_of ~into_block:into_block gamma cxt tup in 
    let type_of_i, confi = type_of ~into_block:into_block gamma cxt i in 
    ( match type_of_tuple, type_of_i with
    | Ttuple(types), Tint -> 
      (match i.value with CstI x -> get types x, join confi conft
      |_ -> raise(Type_Error("An int literal was expected in projection of tuple! "^" at Token: "^(string_of_loc (e.loc) ))) )
    | Ttuple(_), _ -> raise(Type_Error("An integer was expected in projection of tuple! "^" at Token: "^(string_of_loc (e.loc) )))
    | _, _ -> raise(Type_Error("A tuple was expected in projection of tuple! "^" at Token: "^(string_of_loc (e.loc) )))
    )
  | Lst(list) -> 
    ( match list with 
    | [] -> Tlist None, cxt
    | x::_ as ls -> 
      let rec f t acc sec = match t with
      | [] -> Tlist(Some acc), sec
      | x::xs -> 
        let xt, xc = type_of ~into_block:into_block gamma cxt x in 
        if xt = acc then f xs acc (join xc sec)
        else raise(Type_Error("Lists must contain homogeneous-type items. At: "^(string_of_loc e.loc)))
      in 
      let type_of_x, _  = type_of ~into_block:into_block gamma cxt x in 
      f ls type_of_x cxt
    )
  | Cons_op(e, l) -> (* 'a -> 'a list -> 'a list *)
    let type_of_l, c1 = type_of ~into_block:into_block gamma cxt l in 
    let type_of_e, c2 = type_of ~into_block:into_block gamma cxt e in 
    ( match type_of_e, type_of_l with
    | t1, Tlist(Some t2) when t1 <= t2 -> type_of_l, join c1 c2
    | t1, Tlist(Some _) ->  raise(Type_Error("Type error: Cons between "
                            ^(string_of_ttype t1)^" and a "^(string_of_ttype type_of_l)
                            ^" at: "^(string_of_loc (e.loc))))
    | _, Tlist(None) -> Tlist(Some type_of_e), join c1 c2
    | _,_ ->  raise(Type_Error("Type error: Cons between "
              ^(string_of_ttype type_of_e)^" and a "^(string_of_ttype type_of_l)
              ^" at: "^(string_of_loc (e.loc))))
    )
  | Head(l) -> (* 'a list -> 'a *)
    let type_of_l, cl = type_of ~into_block:into_block gamma cxt l in 
    ( match type_of_l with
    | Tlist(Some t) -> t, join cxt cl
    | Tlist(None) -> raise(Type_Error("Type error: attempting to pop an element from an empty list!"
                    ^(string_of_loc (l.loc))))
    | _ -> raise(Type_Error("Head of a non-list value!"^(string_of_loc (l.loc))))
    )
  | Tail(l) -> (* 'a list -> 'a *)
    let type_of_l, c = type_of ~into_block:into_block gamma cxt l in 
    ( match type_of_l with
    | Tlist(Some t) -> Tlist(Some t), join cxt c
    | Tlist(None) -> raise(Type_Error("Type error: attempting to tail an empty list!"
                                    ^(string_of_loc (e.loc))))
    | _ -> raise(Type_Error("Tail of a non-list value!"^(string_of_loc (l.loc))))
    )
  | IsEmpty(l) -> (* 'a list -> bool *)
    ( match type_of ~into_block:into_block gamma cxt l with
    | Tlist(_), c -> Tbool, join cxt c
    | _ -> raise(Type_Error("Check emptiness of a non-list value!"^(string_of_loc (l.loc))))
    )
  | NativeFunction(_) -> (* Need to handle print confidentiality check !!! *)
    raise ( Error_of_Inconsistence("type system: !!! Prohibit use of Native Functions !!! at: "^(string_of_loc e.loc)))
  | Trust(b) -> 
    if into_block = No then TtrustedBlock(type_of_trusted b start_env []), cxt
    else raise (Type_Error("Cannot have nested blocks."))
  | Access(tb, field) -> 
    let tbtype, _ =  type_of ~into_block:into_block gamma cxt tb in 
    ( match tbtype, field.value with
    | TtrustedBlock(env), Var(id) -> (* Only public functions can be accessed in trusted blocks *)
      ( match List.assoc_opt id env with
      (* non-handled functions *)
      | Some (Tfun(_), Private) -> raise (Type_Error("Cannot access to non-handled functions. Error at: "^(string_of_loc e.loc)))
      (* handled functions or functions that work with secret data *)
      | Some (Tfun(_) as t, c) -> t, join cxt c
      | Some (_, _) -> raise(Type_Error("Cannot access to non-handled items; at "^(string_of_loc e.loc)))
      | None -> raise (Type_Error("Field "^id^" not found in block at: "^(string_of_loc tb.loc)))
      )
    | _, Var(_) -> raise (Type_Error("A block was expected in access operation at: "^(string_of_loc tb.loc)))
    | _, _ -> raise (Type_Error("An identifier was expected in access operation at: "^(string_of_loc field.loc))) )
  | SecretData(_) -> (* Is not possible to have secret data outside trusted blocks - syntax constraint *)
    raise (Error_of_Inconsistence("type_of: unexpected secret data outside trusted block: "
    ^(string_of_loc e.loc) ))
  | Handle(_) -> (* Is not possible to have handled exp outside trusted blocks - syntax constraint *)
    raise (Error_of_Inconsistence("type_of: unexpected handled exp outside trusted block: "
    ^(string_of_loc e.loc) ))
  | Plugin(e) -> (* eval in an empty env *)
    if into_block = No then 
      let v, c = type_of ~into_block:Untrusted start_env cxt e in
      (match v with
      | Tfun(_) -> TuntrustedBlock(v), join cxt c
      | _ -> raise(Type_Error("A plugin must implement a function. At: "^(string_of_loc e.loc)))
      )
    else raise (Type_Error("Cannot have nested blocks."))


(* Evaluates a trusted block of expression to an <ide -> ttype * confidentiality> environment 
 * note: the only constructs possible in a trusted block are (also secret) declaration and handle
 *)
and type_of_trusted (e : located_exp) (env : (ttype * confidentiality) env) (tb : (ttype * confidentiality) env) : (ttype * confidentiality) env = 
  match e.value with
  | Let(x, t, e1, e2) -> (* checks rhs type, adds to env and tb and checks the body type *)
    let t', c' = 
      ( match e1.value with
      | SecretData(s) -> (* secret data are followed into information flow analysis *)
        let ts, cs = type_of ~into_block:Trusted env Secret s in
        (match ts with  (* Only data can be secret *)
        | Tint
        | Tbool
        | Tfloat
        | Tchar
        | Tstring
        | Ttuple(_)
        | Tlist(_) -> ts, cs
        | _ -> raise (Type_Error("Only data can be secret. At: "^(string_of_loc s.loc)))
        )
      | Trust(_) -> raise (Type_Error("Cannot have nested blocks. At: "^(string_of_loc e.loc)))
      (* non-secret data are still private, but they are not followed in information flow analysis *)
      | _ -> type_of ~into_block:Trusted env Private e1 
      ) in 
    (match t with 
    | Some tt -> if t' = tt then type_of_trusted e2 ((x,(t',c'))::env) ((x, (t', c'))::tb)
      else raise(Type_Error("Bad type annotation at "^(string_of_loc (e.loc))))
    | None -> type_of_trusted e2 ((x,(t',c'))::env) ((x,(t', c'))::tb)
    )
  | Handle(l) -> (* for each item i, adds (i, (type_of i, Public)) to tb -> shadowing *)
    let add_f (f : located_exp) = 
      (match f.value with
      | Var(name) -> 
        (match List.assoc_opt name tb with  (* Check that the function has been defined in the block *)
        | Some (Tfun(_) as t, Secret) -> (name, (t, Secret))
        | Some (Tfun(_) as t, Private) -> (name, (t, Public))
        | Some (_) -> raise(Type_Error("A Function was expected at: "^(string_of_loc f.loc)))
        | _ -> raise(Type_Error("Handled Function must be declared into the block. At: "^(string_of_loc f.loc)))
        )
      | _ -> raise(Type_Error("An identifier was expected at: "^(string_of_loc f.loc)))
      ) in		
    (List.map (add_f) l)@tb
  | other ->	raise (Error_of_Inconsistence("type_of_trusted: unexpected construct! "
              ^(Syntax.show_exp other)^" at: "^(string_of_loc e.loc) ))
;;

let type_check e = type_of type_env Public e