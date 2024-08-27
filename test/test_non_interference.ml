let code = {|
  // NI satisfied
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun call_checkpwd : unit = 
      if pass = "abcd" then print_string "hello" else print_string "hi"
    in
    handle: {call_checkpwd}
  } in
  pwd.call_checkpwd()
|};;

let%expect_test "NI - 1" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;

let code = {|
  // NI still satisfied: with the declassification the confidentiality of pass became L,
  // and then there are no L values that depend on a H variable
  // --> bad use of declassify, responsability of the developer
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun call_checkpwd : unit = 
      if declassify(pass = "abcd") then print_string "hello" else print_string "hi"
    in
    handle: {call_checkpwd}
  } in
  pwd.call_checkpwd()
|};;

let%expect_test "NI - 2" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  TFhree.Interpreter.eval code |> ignore;
  [%expect {| hello |}]

  let code = {|
  // NI satisfied
  let trust pwd = {
    let secret pass = "abcd" in
    let fun checkpwd (guess : string) : bool = guess = pass in
    handle: {checkpwd}
  } in if (pwd.checkpwd "abcd") then 5 else 10
|};;

let%expect_test "NI - 3" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;

let code = {|
  // NI still satisfied: with the declassification the confidentiality of pass became L,
  // and then there are no L values that depend on a H variable
  // --> bad use of declassify, responsability of the developer
  let trust pwd = {
    let secret pass = "abcd" in
    let fun checkpwd (guess : string) : bool = declassify(guess = pass) in
    handle: {checkpwd}
  } in if (pwd.checkpwd "abcd") then 5 else 10
|};;

let%test "NI - 4" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  TFhree.Interpreter.eval code |> 
  TFhree.Utils.test_cmp_values (Int(5))