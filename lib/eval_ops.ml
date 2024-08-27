open Exceptions;;
open Syntax;;

(* evaluates primitive operations on the evaluated operands *)
let rec eval_bop v1 op v2 = 
  match (op, v1, v2) with
  (* Math operators for integers and floats *)
  | ("*", Int i1, Int i2) -> Int (i1 * i2)
  | ("+", Int i1, Int i2) -> Int (i1 + i2)
  | ("-", Int i1, Int i2) -> Int (i1 - i2)
  | ("/", Int i1, Int i2) -> Int (i1 / i2)
  | ("%", Int i1, Int i2) -> Int (i1 mod i2)
  | ("*.", Float i1, Float i2) -> Float (i1 *. i2)
  | ("+.", Float i1, Float i2) -> Float (i1 +. i2)
  | ("-.", Float i1, Float i2) -> Float (i1 -. i2)
  | ("/.", Float i1, Float i2) -> Float (i1 /. i2)
  (* Define comparison operators for each simple type *)
  | ("=", Int i1, Int i2) -> Bool (if i1 = i2 then true else false)
  | ("=", Float i1, Float i2) -> Bool (if i1 = i2 then true else false)
  | ("=", Char i1, Char i2) -> Bool (if i1 = i2 then true else false)
  | ("=", Bool i1, Bool i2) -> Bool (if i1 = i2 then true else false)
  | ("=", String i1, String i2) -> Bool (if i1 = i2 then true else false)
  | ("=", ListV(l1), ListV(l2)) ->  Bool (l1 = l2)
  | ("=", Tuple(l1), Tuple(l2)) -> Bool (l1 = l2)
  | ("=", TrustedBlock(e1), TrustedBlock(e2)) -> Bool(e1==e2)
  | ("<", Int i1, Int i2) -> Bool (if i1 < i2 then true else false)
  | ("<", Float i1, Float i2) -> Bool (if i1 < i2 then true else false)
  | ("<", Char i1, Char i2) -> Bool (if i1 < i2 then true else false)
  | ("<", Bool i1, Bool i2) -> Bool (if i1 < i2 then true else false)
  | ("<", String i1, String i2) -> Bool (if i1 < i2 then true else false)
  | (">", Int i1, Int i2) -> Bool (if i1 > i2 then true else false)
  | (">", Float i1, Float i2) -> Bool (if i1 > i2 then true else false)
  | (">", Char i1, Char i2) -> Bool (if i1 > i2 then true else false)
  | (">", Bool i1, Bool i2) -> Bool (if i1 > i2 then true else false)
  | (">", String i1, String i2) -> Bool (if i1 > i2 then true else false)
  | ("<>", e1, e2) -> 
    (match eval_bop e1 "=" e2 with Bool x -> Bool (not x) | _ -> raise(Type_system_Failed("In comparison")))
  | (">=", e1, e2) ->
    (match eval_bop e1 "<" e2 with Bool x -> Bool (not x) | _ -> raise(Type_system_Failed("In comparison")))
  | ("<=", e1, e2) -> eval_bop (eval_bop e1 "<" e2) "||" (eval_bop e1 "=" e2)
  (* logical operators *)
  | ("&&", Bool i1, Bool i2) -> Bool (i1 && i2)
  | ("||", Bool i1, Bool i2) -> Bool (i1 || i2)
  (* strings concatenation *)
  | ("^", String s1, String s2) -> String (s1^s2)
  |  _ -> raise (Unsupported_Primitive((Utils.string_of_value v1)^"  "^op^" "^(Utils.string_of_value v2)) )
;;

let eval_uop op v = 
  match op, v with
  |	"-", Int n -> Int (-n)
  | "-", Float n -> Float (-.n)
  | "!", Bool n -> Bool (not n)
  |  _ -> raise (Unsupported_Primitive(op))