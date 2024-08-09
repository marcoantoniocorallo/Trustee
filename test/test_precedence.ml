open TFhree.Syntax;;

(* let fun f (x : int) : int = x + 1 in f -1  // doesn't work! As in ocaml! *)
let code = 
  {|
  // Doesn't work! f -1 is parsed as binary minus, as in ocaml
  let fun f (x : int) : int = x + 1 in -1 |> f
  |}
;;

let value = Int(0);;

let%test "test_precedence" =
  let (@@) v1 v2 = TFhree.Syntax.compare_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value