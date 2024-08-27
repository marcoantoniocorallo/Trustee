(* Closure of checkpwd *)
let code = 
  {|
    let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = pass = guess in
      handle: {checkpwd}
    } in pwd.checkpwd
  |}
;;

let value = TFhree.Syntax.Closure("checkpwd","", TFhree.Utils.dummy_value, [])

let%test "Closure of checkpwd" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code = 
  {|
    let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = pass = guess in
      handle: {checkpwd}
    } in pwd.checkpwd "pippo"
  |}
;;

let%expect_test "Closure of checkpwd - no secret data" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

(* Failure! secret variable access -> field not found *)
let code = 
  {|
    let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = pass = guess in
      handle: {checkpwd}
    } in pwd.pass
  |}
;;

let%expect_test "secret variable access -> field not found" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Field pass not found or not public in block at: (7, 10)-(7, 13)") |}]

(* field non exists -> field not found *)
let code = 
  {|
    let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = pass = guess in
      handle: {checkpwd}
    } in pwd.pas
  |}
;;

let%expect_test "field non exists -> field not found" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Field pas not found or not public in block at: (7, 10)-(7, 13)") |}]

(* cannot use external var into blocks *)
let code = 
  {|
    let a = "pippo" in 
    let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = a = guess in
      handle: {checkpwd}
    } in pwd.checkpwd "tt"
  |}
;;

let%expect_test "external var use" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Binding_Error("a not found") |}]

(* FAILURE! pass is still secret even if has not keyword (i.e. is not handled) *)
let code = 
  {|
    let trust pwd = {
    let pass = "abcd" in

    let fun checkpwd (guess : string) : bool = guess = pass in
    handle: {checkpwd}
    } in pwd.pass
  |}
;;

let%expect_test "non-handled var" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Field pass not found or not public in block at: (7, 10)-(7, 13)") |}]

(* more functions into handle *)
let code = 
  {|
  let trust pwd = {
    let secret pass = "abcd" in

    let fun checkpwd (guess : string) : bool = guess = pass in
    let fun checkpwd2 : bool = true in
    handle: {checkpwd; checkpwd2}
  } in pwd.checkpwd2
  |}
;;

let value = TFhree.Syntax.Closure("checkpwd2","", TFhree.Utils.dummy_value, []);;

let%test "more handled fns" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value

let code =
  {|
    // access to private member + data leakage
    let trust pwd = {
      let secret pass = true in 

      let fun nonhandled : bool = pass in 

      let fun call_checkpwd : bool = pass
      in
      handle: {call_checkpwd}
    } in
    pwd.nonhandled()
  |}
;;

let%expect_test "private field access" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Field nonhandled not found or not public in block at: (12, 5)-(12, 8)") |}]
