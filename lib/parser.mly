%{
    open Syntax

    (** Infix operator for creating a located exp from an exp and a location *)
    let (|@|) value loc = { value = value; loc = loc }

    (* Facilities for computing currying *)
    let fold_types l t_final = 
        List.fold_right (fun t final -> Tfun(t, final) ) l t_final;;

    let fold_lambdas ides types f_body t_final loc = 
      let count = ref (List.length types) in 
      List.fold_right2 (
        fun x t body -> 
          decr count;
          Fun(
            "",
            x,
            Tfun(t, ( fold_types (List.filteri (fun i _ -> i>=(!count) ) (List.tl types) ) t_final ) ), 
            (body|@|loc)
          )
        ) (List.tl ides) (List.tl types) (f_body.value)
    ;;

    let curry l t e loc = 
      let ides, types = List.split l in 
      let first_arg = List.hd ides in
      let types_folded = List.fold_right (fun t final -> Tfun(t, final) ) types t in
      let lambdas_folded = fold_lambdas ides types e t loc in 
      (first_arg, types_folded, lambdas_folded)
    ;;

%}

(** Tokens definition *)
%token <int> INT
%token <char> CHAR
%token <float> FLOAT
%token <bool> BOOL 
%token <string> STRING
%token <string> ID
%token IF THEN ELSE
%token TINT TBOOL TFLOAT TCHAR TSTRING TLIST TUNIT
%token LET IN 
%token FUN LAMBDA PIPE "|>"
%token LPAREN "(" RPAREN ")"
%token LBRACKET "[" RBRACKET "]"
%token LCURLY "{" RCURLY "}"
%token PLUS "+" MINUS "-" TIMES "*" DIV "/" MOD "%"
%token FPLUS "+." FMINUS "-." FTIMES "*." FDIV "/."
%token LESS "<" LEQ "<=" GREATER ">" GEQ ">=" EQ "=" NEQ "<>"
%token AND "&&" OR "||" NOT "!" CONCAT "^"
%token PROJ
%token CONS_OP "::" HEAD "hd" TAIL "tl"
%token COMMA "," COLON ":" SEMICOLON ";" ARROW "->" DOT "."
%token TRUST  SECRET  HANDLE  PLUGIN ASSERT TAINT DECLASSIFY PUBLIC
%token <Syntax.located_exp>PARSED
%token EOF

(** 
 * Associativity and precedence of operators 
 * https://v2.ocaml.org/manual/expr.html#ss:precedence-and-associativity
 *)

%nonassoc prec_let (* shifts! *)
%right "->" 
%left "=" "<" ">" "<=" ">=" "<>" "&&" "||"
%left "+" "-" "^" "+." "-." "::" 
%left "*" "/" "%" "*." "/." 
%left "."

%start <Syntax.located_exp> main

%%

main:
| e = expr EOF 
    { e }

| EOF
    { Empty |@| $loc }

expr:
| e = simple_expr
    { e }

| op = unop e = simple_expr
    { Uop(op, e) |@| $loc }

| e1 = expr op = binop e2 = expr
    { Bop(e1, op, e2) |@| $loc }

| IF guard = expr THEN e1 = expr ELSE e2 = expr                             %prec prec_let
    { If(guard,e1,e2) |@| $loc }

| LET e = let_expr IN e4 = expr                                             %prec prec_let
    { 
      let (e1, e2, e3) = e in 
      Let(e1, e2, e3, e4) |@| $loc
    }

| FUN f = ID l = list(delimited("(",separated_pair(ID, ":", ptype),")")) 
    ":" t_res = ptype "=" e1 = expr                                         %prec prec_let
    { 
      let l = match l with | [] -> [("",Tunit)] | _ -> l in 
      let (first_arg, types_folded, lambdas_folded) = curry l t_res e1 ($loc) in 
      Fun(f, first_arg, types_folded, (lambdas_folded |@| $loc)) |@| $loc
    }

| LAMBDA l = list(delimited("(",separated_pair(ID, ":", ptype),")")) 
    ":" t_res = ptype "->" e = expr                                         %prec prec_let
    { 
      let l = match l with | [] -> [("",Tunit)] | _ -> l in 
      let (first_arg, types_folded, lambdas_folded) = curry l t_res e ($loc) in 
      Fun("", first_arg, types_folded, (lambdas_folded |@| $loc)) |@| $loc 
    }
    
| f = func
    { f }

| PROJ t = simple_expr i = simple_expr                             
    { Proj(t,i) |@| $loc }

| e = expr "::" l = expr
    { Cons_op(e,l) |@| $loc }

| HEAD l = simple_expr
    { Head(l) |@| $loc }

| TAIL l = simple_expr
    { Tail(l) |@| $loc }

| e = PARSED
    { 
      match e.value with
      | Let(_,_, { loc = _; value = PluginData(_)},_) -> e
      | _ -> raise(Exceptions.Type_Error("A plugin was expected in include statement at: "^(Utils.string_of_loc e.loc)))
    }

| ASSERT t = option(TAINT) e = simple_expr 
    { 
      let t = if Option.is_some t then true else false in 
      Assert(e, t) |@| $loc 
    }

| DECLASSIFY e = simple_expr
    { Declassify(e) |@| $loc }

let_expr:
| id = ID t = option(preceded(":", ptype)) "=" e1 = expr
    { (id, t, e1) }

| FUN f = ID l = list(delimited("(",separated_pair(ID, ":", ptype),")")) 
  ":" t_res = ptype "=" e1 = expr
    { 
      let l = match l with | [] -> [("",Tunit)] | _ -> l in 
      let (first_arg, types_folded, lambdas_folded) = curry l t_res e1 ($loc) in 
      (f, Some (types_folded), 
        Fun(f, first_arg, types_folded, (lambdas_folded |@| $loc)) |@| $loc
      )
    }

| TRUST name = ID "=" "{" e = trust_expr "}"
    { (name, None, Trust(name, e) |@| $loc ) }

| PLUGIN name = ID "=" "{" e = trust_expr "}"
    { (name, None, PluginData(e) |@| $loc ) }

(** simple_expr is a syntactical category used for disambiguing the grammar. *)
simple_expr:
| "(" ")" 
    { Empty |@| $loc }

| "(" e = expr ")"
    { e }

| c = constant
    { c }

| id = ID
    { Var(id) |@| $loc }

| t = tuple
    { Tup(t) |@| $loc }

| l = lst 
    { Lst(l) |@| $loc }

| e = simple_expr "." f = simple_expr
    { Access(e,f) |@| $loc }

constant:
| i = INT
    { CstI(i) |@| $loc }

| b = BOOL
    { CstB(b) |@| $loc }

| c = CHAR
    { CstC(c) |@| $loc }

| f = FLOAT
    { CstF(f) |@| $loc }

| s = STRING
    { CstS(s) |@| $loc } 

trust_expr:
| LET s = option(SECRET) e = let_expr IN e4 = trust_expr 
    { 
      let (e1, e2, e3) = e in
      match s, e3.value with
      | None, _ -> Let(e1, e2, e3, e4) |@| $loc
      | Some _, Fun(_) -> raise(Exceptions.Type_Error("Only data can be secret. At: "^(Utils.string_of_loc e3.loc)))
      | Some _, _ -> Let(e1, e2, (SecretData(e3) |@| $loc), e4) |@| $loc 
    }
  
| HANDLE ":" e = delimited("{", separated_nonempty_list(";", simple_expr), "}")
    { Handle(e) |@| $loc }

(** Fun Call and composition *)
(** The arg of the function is a simple_expr, that is an identifier, a literal,
    or a complex expression surrounded by parentheses 
    Note: function application is left associative! *)
func:
| f = simple_expr e = simple_expr
    { Call(f,e) |@| $loc }

| f = func e = simple_expr
    { Call(f,e) |@| $loc }

| e = simple_expr "|>" f = simple_expr
    { Call(f,e) |@| $loc }

| op = unop e = simple_expr "|>" f = simple_expr
    { Call(f,Uop(op, e) |@| $loc ) |@| $loc }

| e = func "|>" f = simple_expr
    { Call(f,e) |@| $loc }

(** A tuple must have at least two elements  *)
tuple:
| "(" e = expr "," s = sequence ")"
    { e::s }

(** A list can be an empty list or a list of any size *)
lst:
| "[" "]"
    { [] }

| "[" s = sequence "]"
    { s }

sequence:
| e = expr "," s = sequence
    { e::s }

| e = expr 
    { e::[] }

(** Syntactical categories for types definition *)
ptype:
  | t = simple_ptype
    { t }

  | "(" t = ptype "*" s = ptype_sequence ")"
    { Ttuple(t::s) }

  | t1 = ptype "->" t2 = ptype
    { Tfun(t1, t2) }
  
  | TRUST l = delimited("{",separated_nonempty_list(";",pair(option(PUBLIC),separated_pair(ID, ":", ptype))),"}")
    {
      TtrustedBlock(
        List.map (
          fun (public_opt, (i, t)) -> match public_opt with
          | None ->   (i, (t, Public, Secret("")))
          | Some _ -> (i, (t, Public, Normal("")))
        ) l
      )
    }

  | PLUGIN l = delimited("{",separated_nonempty_list(";",separated_pair(ID, ":", ptype)),"}")
    { TuntrustedBlock( List.map (fun (i, t) -> (i, (t, Public, Plugin))) l) }

(** simple_type is a syntactical category used for disambiguing the grammar. 
 * In particular, it disambiguise the derivation " type -> type list "
 *)
simple_ptype:
  | TINT
    { Tint }

  | TBOOL
    { Tbool }

  | TCHAR
    { Tchar }

  | TFLOAT
    { Tfloat }

  | TSTRING
    { Tstring }

  | TUNIT 
    { Tunit }

  | t=option(simple_ptype) TLIST
    { Tlist(t) }

  | "(" t=ptype ")"
    { t }

ptype_sequence:
  | t = ptype "*" s = ptype_sequence
    { t::s }

  | t = ptype 
    { t::[] }

%inline binop:
| "+"   { "+" }
| "-"   { "-" }
| "*"   { "*" }
| "/"   { "/" }
| "+."  { "+." }
| "-."  { "-." }
| "*."  { "*." }
| "/."  { "/." }
| "%"   { "%" }
| "<"   { "<" }
| ">"   { ">" }
| "<="  { "<=" }
| ">="  { ">=" }
| "="   { "=" }
| "<>"  { "<>" }
| "^"   { "^" }
| "&&"  { "&&" }
| "||"  { "||" }

%inline unop:
| "-"   { "-" }
| "!"   { "!" }