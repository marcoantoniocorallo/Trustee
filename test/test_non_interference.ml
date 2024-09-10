let code = {|
  // NI satisfied (print blocked because conf. plugin -> prevented!)
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
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;

let code = {|
  // NI still satisfied: even with the declassification, 
  // print is blocked (plugin + normal -> top)!
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
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;
  
let code = {|
  // Data leak statically blocked
  let trust pwd = {
    let secret pass = "abcd" in
    let fun checkpwd (guess : string) : bool = guess = pass in
    handle: {checkpwd}
  } in if (pwd.checkpwd "abcd") then 5 else 10
|};;

let%expect_test "NI - 3" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;

let code = {|
  // NI satisfied (prevented): guard has conf = Normal("pwd"), if/else has bottom!
  let trust pwd = {
    let secret pass = "abcd" in
    let fun checkpwd (guess : string) : bool = declassify(guess = pass) in
    handle: {checkpwd}
  } in if (pwd.checkpwd "abcd") then 5 else 10
|};;

let%expect_test "NI - 4" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  Trustee.Interpreter.eval code |> Trustee.Utils.string_of_value |> print_endline;
  [%expect {| 5 |}]
;;

let code = {|
  // NI satisfied (Prevented!)
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun call_checkpwd : unit = 
      if pass = "abcd" then "hello" else "hi"
    in
    handle: {call_checkpwd}
  } in
  pwd.call_checkpwd()
|};;

let%expect_test "NI - 5" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;