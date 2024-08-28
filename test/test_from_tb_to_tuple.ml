let code = {|
    let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = declassify(guess = pass) in
      let fun f : bool = true in
      handle: {checkpwd; f}
    } in 
    let fun f (t : ((string -> bool) * (unit -> bool))) : bool = (proj t 0) "acd"
    in f (pwd.checkpwd, pwd.f)
|}
;;

let value = TFhree.Syntax.Bool(false);;

let%test "from tb to tuple" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value