let%expect_test "test_IO1" =
  let code = "print_int 5" in
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let _ = Trustee.Interpreter.eval code in 
  [%expect {| 5 |}]

let%expect_test "test_IO2" =
  let code = {| print_string "5" |} in
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let _ = Trustee.Interpreter.eval code in 
  [%expect {| 5 |}]

let%expect_test "test_IO3" =
  let code = {| print_float 5. |} in
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let _ = Trustee.Interpreter.eval code in 
  [%expect {| 5. |}]

let%expect_test "test_IO4" =
  let code = {| print_string 5 |} in
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
    | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("functional application: argument type mismatch: (1, 15)-(1, 16)function string -> unit got int instead") |}]

let%expect_test "test_IO5" =
  let code = {| print_float 5 |} in
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
    | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("functional application: argument type mismatch: (1, 14)-(1, 15)function float -> unit got int instead") |}]

let%expect_test "test_IO6" =
  let code = {| print_bool true |} in
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let _ = Trustee.Interpreter.eval code in 
  [%expect {| true |}]

let%expect_test "test_IO7" =
  let code = {| print_bool "true" // type error |} in
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
    | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("functional application: argument type mismatch: (1, 13)-(1, 19)function bool -> unit got string instead") |}]

let%expect_test "test_IO8" =
  let code = {| print_char 'c' |} in
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let _ = Trustee.Interpreter.eval code in 
  [%expect {| c |}]

let%expect_test "test_IO9" =
  let code = {| print_char c |} in
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
    | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Binding_Error("c not found at: (1, 13)-(1, 14)") |}]

let%expect_test "test_IO10" =
  let code = {| let c = 'c' in print_char c |} in
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let _ = Trustee.Interpreter.eval code in 
  [%expect {| c |}]

let%test "test_IO11" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let code = "print_string // closure" in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ (Closure("print_string","",Trustee.Utils.dummy_value, []))

let%test "test_IO12" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let code = "get_int // closure" in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ (Closure("get_int","",Trustee.Utils.dummy_value, []))

let%test "test_IO13" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let code = "get_int // closure" in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ (Closure("get_int","",Trustee.Utils.dummy_value, []))