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
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

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
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

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
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

let code = 
  {|
    // declassified!
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 
      handle: {checkpwd}
    } in pwd.checkpwd "abcd"
  |}

let value = TFhree.Syntax.Bool(true)

let%test "declassified" =
  let (@@) v1 v2 = TFhree.Syntax.compare_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
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

let%expect_test "don't touch secret data" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  TFhree.Interpreter.eval code |> ignore;
  [%expect {| abcd |}]


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
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Declassification is only possible inside trusted blocks. At: (5, 18)-(5, 30)") |}]
