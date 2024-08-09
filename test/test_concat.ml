open TFhree.Syntax;;

let code = 
  {| 
  let fun concat (s1 : string) : string -> string = 
    let fun concat2 (s2 : string) : string = s1^s2 in concat2 
    in concat "Hello " "World" 
  |}
;;

let value = String("Hello World");;

let%test "concat" =
  let (@@) v1 v2 = TFhree.Syntax.compare_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 

  vval @@ value