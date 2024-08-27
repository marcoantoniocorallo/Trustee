open TFhree.Syntax;;

let code = 
  {|
  let fun idx(x : int -> int) : int->int = x in
	(* [fun [idx](1) = [x](2) in](3) *)

	let fun idy(y : int) : int = y in
	(* [fun [idy](4) = [y](5) in](6) *)

	idx idy
	(* [ [idx](7) [idy](8) ](9) *)
  |}
;;

let value = Closure("idy", "y", TFhree.Utils.dummy_value, []);;

let%test "idid" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value