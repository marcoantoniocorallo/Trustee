open TFhree.Syntax;;

let code = "let fun sum (a : int) (b : int) : int = a+b in sum 5 6";;

let value = Int(11);;

let%test "currying_1" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code = "(lambda (x : int) : int -> x+x) 5";;

let value = Int(10);;

let%test "currying_2" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code = "(lambda (x : int) (y : int) : int -> x+y) 2 3";;

let value = Int(5);;

let%test "currying_3" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code = 
  {|
  (lambda (a : int) (b : char list) (c : (int * float * string) ) (d : (string) ) (e : (string -> string)) : (int * string)
 -> (2*a+(proj c 0), (e d))
) 3 ['a','b','c'] (10, 10.0, "10") "this string!" (lambda (x : string) : string -> x^" - "^x )
  |};;

let value = Tuple([ Int(16); String("this string! - this string!")] );;

let%test "currying_4" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

  let code = 
  {|
  let fun fact (n : int) : int = if n=1 then 1 else n*fact (n-1) in fact 4
  |};;

let value = Int(24);;

let%test "currying_5" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

    let code = 
  {|
  let fun fact (n : int) : int = if n=1 then 1 else n*fact (n-1) in fact 4
  |};;

let value = Int(24);;

let%test "currying_5" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code = 
  {|
  (* Without currying *)
	let fun double(x : int) : int =
	  x * 2
	in
	let fun twice(f : int -> int) : int -> int =
	  let fun app(x : int) : int =
	    f (f x)
	  in
	  app
	in
	twice double 4
  |};;

let value = Int(16);;

let%test "currying_6" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code = 
  {|
  let fun twice (f : int -> int) (n : int) : int = f (f n) 
  in twice (lambda (x : int) : int -> x*2) 4
  |};;

let value = Int(16);;

let%test "currying_7" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code = 
  {|
  let fun f (a : int) (b : int) (c : int) (d : int) : int = a+b+c+d in f 1 2 3 4
  |};;

let value = Int(10);;

let%test "currying_8" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value