open TFhree.Syntax;;

let code = 
  {|
  "lambda" |> (lambda (s : string) : string -> s^" with annotations") 
  |}
;;

let value = String("lambda with annotations");;

let%test "test_pipe" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code = 
  {|
  "lambda" 
  |> (lambda (s : string) : string -> s^" with annotations")
  |> (lambda (s : string) : string -> s^" and other annotations")
  |}
;;

let value = String("lambda with annotations and other annotations");;

let%test "test_pipe_of_pipe" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code = 
  {|
  let fun sum (a : int) (b : int) : int = a + b in 
  5 |> sum 10
  |}
;;

let value = Int(15);;

let%test "test_multiple_arguments_fn_pipe" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code = 
  {|
  let fun f (g : int -> int) (n : int) : int = g n in 
  (lambda (x : int) : int -> x+10) |> f 5
  |}
;;

let value = Int(15);;

let%test "test_high_order_pipe" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value