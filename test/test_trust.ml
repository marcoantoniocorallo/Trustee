let code =
  {|
    // Data leak value returned
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd : string = pass
      in

      handle: {checkpwd}
    } in
    pwd.checkpwd()
  |}
;;

let%expect_test "Data leak value returned" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

let code =
  {|
    // Data leak value printed
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd : unit = 
        if pass = "abcd" then print_string "correct" else print_string "wrong"
      in

      handle: {checkpwd}
    } in
    pwd.checkpwd()
  |}
;;

let%expect_test "Data leak value printed" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

let code =
  {|
    // false positive
    // declassify needs
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = pass = guess in 
      handle: {checkpwd}
    } in pwd.checkpwd "abcd"
  |}
;;

let%expect_test "False positive" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

let code = 
  {|
    // declassified!
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 
      handle: {checkpwd}
    } in pwd.checkpwd "abcd"
  |}

let value = Trustee.Syntax.Bool(true)

let%test "declassified" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value

let code =
  {|
    // don't touch secret data
    let trust pwd = {
      let secret pass = "abcd" in 
      let nonsecret = "abcd" in

      let fun checkpwd : unit = 
        if nonsecret = "abcd" then print_string "abcd" else print_string "cc"
      in

      handle: {checkpwd}
    } in
    pwd.checkpwd()
  |}
;;

(* still blocked because print (plugin) joined to tb (normal) rises to Top *)
let%expect_test "don't touch secret data" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]


let code = {| 
  let pwd =
    let pass = "abcd" in 
    let fun checkpwd (guess : string) : bool = 
      declassify(pass = guess) in
    checkpwd
    in pwd "abcd"
|};;

let%expect_test "declassify outside of trusted blocks" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("Declassification is only possible inside trusted blocks. At: (5, 18)-(5, 30)") |}]
;;

let code = {|
  let trust pwd = {
    let secret pass = "abcd" in
    let fun checkpwd (guess : string) : bool = 
      let _ = print_string pass in 
      declassify(guess = pass) in
    handle: {checkpwd}
  } in pwd.checkpwd "abcd"
|}
;;

let%expect_test "print secret data" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;