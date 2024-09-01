(** Type system: type algorithm with mandatory type annotation for the definition of functions
 * The type system is enriched with confidentiality types for computing 
 * static information-flow analysis to avoid data leakage.
 * The definition of types is in file syntax.ml
 *)

open Exceptions
open Syntax
open Utils

(** Type ordering *)
let rec (<=) (t1 : ttype) (t2 : ttype) : bool = match t1, t2 with
	| t1', t2' when t1'=t2' -> true
	| Tlist(None), Tlist(_) -> true
	| Tlist(Some t1'), Tlist(Some t2') when t1' <= t2' -> true
	| Ttuple(l1), Ttuple(l2) when List.length l1 = List.length l2 -> List.for_all2 (<=) l1 l2
  | TtrustedBlock(e1), TtrustedBlock(e2) 
  | TuntrustedBlock(e1), TuntrustedBlock(e2) -> equal_envs e1 e2
	| _ -> false

(* block env comparison: public function types and confidentialities are compared *)
and equal_envs lst1 lst2 =
  let cmp_conf = function
    | Secret _, Secret _
    | Plugin, Plugin
    | Normal _, Normal _ -> 0 
    | _ -> 1
  in let cmp_pairs (s1, t1, c1) (s2, t2, c2) =
    let cmp_s = String.compare s1 s2 in
    if cmp_s <> 0 then cmp_s
    else if not(t1 <= t2) then 1
    else cmp_conf (c1, c2)
  in let reduce lst = 
    List.filter (function _,(_,Public,_) -> true | _ ->false) lst |> 
    List.map (fun (i,(t,_,c)) -> (i,t,c)) |> 
    List.sort cmp_pairs
  in List.for_all2
    (fun (i1, t1, c1) (i2, t2, c2) -> i1=i2 && t1 <= t2 && (cmp_conf(c1, c2) = 0))
    (reduce lst1) (reduce lst2)
;; 

(** The starting type environment. *)
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

let type_env = List.map (fun (i,t) -> (i, (t, Bottom))) type_env;;

(** Typing rule in a given type environment gamma.
 * @params: into_block: keep trace of where we are: Trusted, Untrusted, Outside of blocks
 *          start_env: the starting type env, where blocks are type-checked
 *          gamma: environment
 *          cxt: confidentiality level context
 *          e: expression to type-check
 *)
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
    (try
      let tx, c = lookup gamma x in 
      (match tx with 
      | TuntrustedBlock(_) when into_block = Trusted ->
        raise(Security_Error("Cannot access to plugin from inside trusted blocks."))
      | _ -> tx, c
      )
    with Binding_Error s -> raise(Binding_Error(s^" at: "^(string_of_loc e.loc))))
  | Bop(e1, "=", e2) 
  | Bop(e1, "<>", e2) -> 
    let t1, c1 = type_of ~into_block:into_block gamma cxt e1 in
    let t2, c2 = type_of ~into_block:into_block gamma cxt e2 in
    ( match t1, t2 with
    | Tlist(_), Tlist(_)
    | Ttuple(_), Ttuple(_) 
    | TtrustedBlock _, TtrustedBlock _
    | TuntrustedBlock _, TuntrustedBlock _->  Tbool, (join c1 c2)
    | Tfun(_,_), Tfun(_,_) ->   raise (Type_Error ("Equality of functional values"
                                ^(string_of_loc (e.loc))))
    | t1', t2' when t1' <= t2' -> Tbool, (join c1 c2)
    | _, _ -> raise (Type_Error ("Error in the arguments of equality"^(string_of_loc (e.loc))))
    )
  | Bop(e1, "<", e2)
  | Bop(e1, "<=", e2)
  | Bop(e1, ">", e2)
  | Bop(e1, ">=", e2) ->
    let t1, c1 = type_of ~into_block:into_block gamma cxt e1 in
    let t2, c2 = type_of ~into_block:into_block gamma cxt e2 in
    ( match t1, t2 with
    | Ttuple(_), Ttuple(_) ->   raise (Type_Error ("Tuple only support comparison (=)."
                                ^(string_of_loc (e.loc))))
    | Tlist(_), Tlist(_) ->     raise (Type_Error ("Lists only support comparison (=)."
                                ^(string_of_loc (e.loc))))
    | Tfun(_,_), Tfun(_,_) ->   raise (Type_Error ("Equality of functional values"
                                ^(string_of_loc (e.loc))))
    | t1', t2' when t1' <= t2' -> Tbool, (join c1 c2)
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
      if t1 = tt then t2, join c2 cxt
      else raise(Type_Error("Bad type annotation at "^(string_of_loc (e.loc))))
    | None -> t2, join c2 cxt
    )
  | If(e1, e2, e3) ->
    let t1, c1 = type_of ~into_block:into_block gamma cxt e1 in 
    if t1 = Tbool then
      let cxt' = join cxt c1 in 
      let t2, c2 = type_of ~into_block:into_block gamma cxt' e2 in
      let t3, c3 = type_of ~into_block:into_block gamma cxt' e3 in
      match t2, t3 with 
      | _, _ when t2 <= t3 -> t2, join (join c1 cxt) (join c2 c3)
      | TtrustedBlock _, TtrustedBlock _ when not(t2 <= t3) -> 
        raise (Type_Error ("If-Rule: branches return different trusted blocks. At Token: "^(string_of_loc (e.loc))))
      | _, _ -> 
        raise (Type_Error ("If-Rule: branches have different types: then is "^(string_of_ttype t2)^", else is "^(string_of_ttype t3)
        ^" - at Token: "^(string_of_loc (e.loc))))
    else raise (Type_Error ("If-Rule: if with no a boolean guard"^(string_of_loc (e.loc))))
  | Fun(f, x, fun_type, body) ->
    ( match fun_type with 
      (Tfun(t1,t2) as t) ->
        let level = if into_block=Untrusted then Plugin else Bottom in 
        let gamma' = (f, (t, level)) :: (x, (t1, level)) :: gamma in
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
    | Tfun(tx, tr) as tfun  ->
      if t2 <= tx then 
        let tf, conf = tr, join c1 c2
        in match conf with
        | Top       (* handled-functions must not leak data! *)
        | Secret _ -> raise(Security_Error("The program could contain a Data leakage."))
        | _ -> tf, conf
      else (match (t2, tx) with
        | TtrustedBlock _, TtrustedBlock _ 
        | TuntrustedBlock _, TuntrustedBlock _ -> 
          raise (Type_Error("functional application: argument type mismatch: "^(string_of_loc (e2.loc))
                  ^"function "^(string_of_ttype tfun)^" got a block with different interface"))
        | _ -> 
          raise (Type_Error("functional application: argument type mismatch: "^(string_of_loc (e2.loc))
                  ^"function "^(string_of_ttype tfun)^" got "^(string_of_ttype t2)^" instead")))
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
  | NativeFunction(_) ->
    raise ( Error_of_Inconsistence("type system: !!! Prohibit use of Native Functions !!! at: "^(string_of_loc e.loc)))
  | Trust(id, b) -> 
    (*if into_block = No then TtrustedBlock(type_of_trusted id b start_env []), cxt*)
    if into_block = No then TtrustedBlock(type_of_block Trusted (Some(id)) b start_env []), cxt
    else raise (Type_Error("Cannot have nested blocks. At: "^(string_of_loc e.loc)))
  | PluginData(e) -> 
    (*if into_block = No then TuntrustedBlock(type_of_untrusted e start_env []), cxt*)
    if into_block = No then TuntrustedBlock(type_of_block Untrusted None e start_env []), cxt
    else raise (Type_Error("Cannot have nested blocks. At: "^(string_of_loc e.loc)))
  | Access(tb, field) -> 
    let tbtype, tbconf =  type_of ~into_block:into_block gamma cxt tb in 
    ( match tbtype, field.value with
    | TtrustedBlock(env),   Var(id)    (* Only public functions can be accessed in blocks *)
    | TuntrustedBlock(env), Var(id) ->
      ( match List.assoc_opt id env with
      | Some (_, Private, _) -> raise (Type_Error("Field "^id^" not found or not public in block at: "^(string_of_loc tb.loc)))
      | Some (Tfun(_) as t, Public, c) -> t, join cxt (join tbconf c)
      | Some (_, _, _) -> raise(Error_of_Inconsistence("Not-function public object in a block! At: "^(string_of_loc e.loc)))
      | None -> raise (Type_Error("Field "^id^" not found or not public in block at: "^(string_of_loc tb.loc)))
      )
    | _, Var(_) -> raise (Type_Error("A block was expected in access operation at: "^(string_of_loc tb.loc)))
    | _, _ -> raise (Type_Error("An identifier was expected in access operation at: "^(string_of_loc field.loc))) )
  | SecretData(_) -> (* Is not possible to have secret data outside trusted blocks - syntax constraint *)
    raise (Error_of_Inconsistence("type_of: unexpected secret data outside trusted block: "
    ^(string_of_loc e.loc) ))
  | Handle(_) -> (* Is not possible to have handled exp outside trusted blocks - syntax constraint *)
    raise (Error_of_Inconsistence("type_of: unexpected handled exp outside trusted block: "
    ^(string_of_loc e.loc) ))
  | Assert(p, taint_flag) -> 
    let t, c = type_of ~into_block:into_block gamma cxt p in
    if taint_flag then t, join c cxt (* just assert if the expression p is taint *)
    else (* assert (as usual) the predicate p *)
      ( match t with 
      | Tbool -> t, join c cxt
      | _ -> raise(Type_Error("A boolean predicate was expected in assertion: "^(string_of_loc p.loc)))
      )
  | Declassify(e) -> 
    if into_block = Trusted then 
      let t, c = type_of ~into_block:into_block gamma cxt e in
      match c with 
      | Secret s -> t, join (Normal s) cxt
      | other  -> 
        raise(Error_of_Inconsistence("Attempt to declassify non-secret data: "
        ^(Syntax.show_confidentiality other)^" at: "^(string_of_loc e.loc)))
    else raise(Type_Error("Declassification is only possible inside trusted blocks. At: "^(string_of_loc e.loc)))

(** Type checker for expressions defined inside a trusted block.
 *  @params:  b_type: type of block: Trusted or Untrusted (plugin)
 *            name: the name of the trusted block. Secret vars in block t have conf Secret(t)
 *            e : expression to type-check
 *            env: global type environment
 *            b: environment of the block, the returned one
 *)
and type_of_block (b_type : block_type) (name : ide option) (e : located_exp)
                  (env : (ttype * confidentiality) env)  
                  (b : (ttype * qualifier * confidentiality) env) 
                  : (ttype * qualifier * confidentiality) env = 
  (* checks of consistence *)
  if b_type = No then raise(Error_of_Inconsistence("type_of_block with block_type = No ! At: "^(string_of_loc e.loc)))
  else if b_type = Trusted && name = None then raise(Error_of_Inconsistence("Trusted block without name at: "^(string_of_loc e.loc)));
  
  match e.value with
  | Let(x, t, e1, e2) -> (* checks rhs type, adds to env and tb and checks the body type *)
    let t', c' =
      if b_type = Trusted then (* Trusted Block *) 
        ( match e1.value with
        | SecretData(s) -> 
          let ts, cs = type_of ~into_block:Trusted env (Secret(Option.get name)) s in
          (match ts with  
          | Tint
          | Tbool
          | Tfloat
          | Tchar
          | Tstring
          | Ttuple(_)
          | Tlist(_) -> ts, cs
          | _ -> raise (Type_Error("Only data can be secret. At: "^(string_of_loc s.loc)))
          )
        | Trust(_) -> raise (Type_Error("Cannot have nested blocks. At: "^(string_of_loc e1.loc)))
        | _ -> type_of ~into_block:Trusted env (Normal(Option.get name)) e1
        )
      else (* Plugin *)
        ( match e1.value with
        | SecretData(_) -> raise (Type_Error("Secret kw not allowed here. At: "^(string_of_loc e1.loc)))
        | Trust(_) 
        | PluginData(_) -> raise (Type_Error("Cannot have nested blocks. At: "^(string_of_loc e1.loc)))
        | _ -> type_of ~into_block:Untrusted env Plugin e1 
        ) in 
      (match t with 
      | Some tt -> 
        if t' = tt then type_of_block b_type name e2 ((x,(t', c'))::env) ((x, (t', Private, c'))::b)
        else raise(Type_Error("Bad type annotation at "^(string_of_loc (e.loc))))
      | None -> type_of_block b_type name e2 ((x,(t', c'))::env) ((x, (t', Private, c'))::b)
      )
  | Handle(l) -> (* for each item i, adds (i, (type_of i, Bottom/Plugin)) to tb -> shadowing *)
    let add_f (f : located_exp) = 
      (match f.value with
      | Var(name) ->
        (try 
          (match lookup b name with  (* Check that the function has been defined in the block *)
          | (Tfun(_) as t, _, conf) -> (name, (t, Public, conf))
          | _ -> raise(Type_Error("A Function was expected at: "^(string_of_loc f.loc)))
          )
        with Binding_Error _ -> raise(Binding_Error("Function "^name^" was not declared. At: "^(string_of_loc f.loc))))
      | _ -> raise(Type_Error("An identifier was expected at: "^(string_of_loc f.loc)))
      ) in		
    (List.map (add_f) l)@b
  | other ->	raise (Error_of_Inconsistence("type_of_block: unexpected construct! "
              ^(Syntax.show_exp other)^" at: "^(string_of_loc e.loc) ))
;;

let type_check e = 
  let t, c = type_of type_env Bottom e in 
  match c with
  | Top -> raise(Security_Error("The program could contain a Data leakage."))
  | _ -> t 