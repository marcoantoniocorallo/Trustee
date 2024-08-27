let code = 
  {|
  let fun fact (n:int) :int = 
  if n<=1 then 1 else n*(fact (n-1)) in 
  	let x = 5 in 
  		let y = 10 in 
  		if x<y then
			let z = x+y in fact(z)
  		else
			let z = x-y in fact(z)
  |}
;;

let value = TFhree.Syntax.Int(1307674368000);;

let%test "test_lexer" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value