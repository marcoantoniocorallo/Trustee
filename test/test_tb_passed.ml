(* TB passed as parameters test *)

let code = {|
  // normal conf. block passed -> true
  let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = true in

      handle: {checkpwd}
  } in 
  let fun f (b: trust{ public checkpwd: (string -> bool)}) (s : string) : bool = (b.checkpwd) s in
  f pwd "abcd"
|};;

let value = Trustee.Syntax.Bool(true);;

let%test "normal conf. block passed -> true" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value
;;

let code = {|
  // secret conf. block passed -> Security Error: The program could contain a Data leakage.
  let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = guess = pass in

      handle: {checkpwd}
  } in 
  let fun f (b: trust{ checkpwd: (string -> bool)}) (s : string) : bool = (b.checkpwd) s in
  f pwd "abcd"
|};;

let%expect_test "secret conf. block passed" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]


let code = {|
  // secret conf. block passed as public -> Type Error!
  let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = guess = pass in

      handle: {checkpwd}
  } in 
  let fun f (b: trust{ public checkpwd: (string -> bool)}) (s : string) : bool = (b.checkpwd) s in
  f pwd "abcd"
|};;

let%expect_test "secret conf. block passed as public" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("functional application: argument type mismatch: (11, 5)-(11, 8)function Trusted Block type -> string -> bool got a block with different interface") |}]
;;

let code = {|
  // public conf. block passed as secret -> Security Error:
  // is the same of invoking pwd.checkpwd without the declassification
  let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = declassify(guess = pass) in

      handle: {checkpwd}
  } in 
  let fun f (b: trust{ checkpwd: (string -> bool)}) (s : string) : bool = (b.checkpwd) s in
  f pwd "abcd"
|};;

let%expect_test "public conf. block passed as secret" =
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
  // block with different interface 1 -> type error
  let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = true in
      let fun checkpw  (guess : string) : bool = true in

      handle: {checkpwd; checkpw}
  } in 
  let fun f (b: trust{public checkpwd: (string -> bool)}) (s : string) : bool = (b.checkpwd) s in
  f pwd "abcd"
|};;

let%expect_test "block with different interface 1" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("functional application: argument type mismatch: (12, 5)-(12, 8)function Trusted Block type -> string -> bool got a block with different interface") |}]
;;

let code = {|
  // normal with more than one handled function
  let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = true in
      let fun checkpw  (guess : string) : bool = true in

      handle: {checkpwd; checkpw}
  } in 
  let fun f (b: trust{public checkpwd: (string -> bool); public checkpw: (string -> bool)}) (s : string) : bool = (b.checkpwd) s in
  f pwd "abcd"
|};;

let value = Trustee.Syntax.Bool(true);;

let%test "normal with more than one handled function" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value
;;

let code = {|
  // normal with more than one handled function and mismatching -> type error
  let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = true in
      let fun checkpw  (guess : string) : bool = true in

      handle: {checkpwd; checkpw}
  } in 
  let fun f (b: trust{public checkpwd: (string -> bool); checkpw: (string -> bool)}) (s : string) : bool = (b.checkpwd) s in
  f pwd "abcd"
|};;

let%expect_test "normal with more than one handled function and mismatching" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("functional application: argument type mismatch: (12, 5)-(12, 8)function Trusted Block type -> string -> bool got a block with different interface") |}]
;;

let code = {|
  // tb returned
  let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = true in
      let fun checkpw  (guess : string) : bool = true in

      handle: {checkpwd; checkpw}
  } in 
  let fun f (s : string) : trust{public checkpwd: (string -> bool); public checkpw: (string -> bool)} = pwd in
  f "abcd"
|};;

let%expect_test "Tb returned" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let res = Trustee.Interpreter.eval code in 
  print_endline (Trustee.Utils.string_of_value res);
  [%expect {| ~Trusted Block~ |}]
;;

let code = {|
  // tb returned with mismatching -> type error
  let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = true in
      let fun checkpw  (guess : string) : bool = true in

      handle: {checkpwd; checkpw}
  } in 
  let fun f (s : string) : trust{public checkpwd: (string -> bool); checkpw: (string -> bool)} = pwd in
  f "abcd"
|}

let%expect_test "tb returned with mismatching" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("Function return type does not match. Expected Trusted Block type got Trusted Block type at (11, 7)-(11, 101)") |}]
;;