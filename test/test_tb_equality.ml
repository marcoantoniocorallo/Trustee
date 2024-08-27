let code = 
  {|
    // different
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = pass = guess in 
      handle: {checkpwd}
    } in 
    let trust t = { 
        let secret pwd = "pwd" in 
        let fun f : bool = false in 
        handle:{f} 
    } in 
    pwd = t
  |}
;;

let value = TFhree.Syntax.Bool(false)

let%test "Trusted Block equality - 1" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value
;;

let code = 
  {|
    // different
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = pass = guess in 
      handle: {checkpwd}
    } in 
    let trust t = { 
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = pass = guess in 
      handle: {checkpwd}
    } in 
    pwd = t 
  |}
;;

let value = TFhree.Syntax.Bool(false)

let%test "Trusted Block equality - 2" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value
;;

let code = 
  {|
    // same
    let trust pwd = {
      let secret pass = "abcd" in 
    
      let fun checkpwd (guess : string) : bool = pass = guess in 
      handle: {checkpwd}
    } in 
    let trust t = { 
      let secret pass = "abcd" in 
    
      let fun checkpwd (guess : string) : bool = pass = guess in 
      handle: {checkpwd}
    } in 
    pwd = pwd
  |}
;;

let value = TFhree.Syntax.Bool(true)

let%test "Trusted Block equality - 3" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value