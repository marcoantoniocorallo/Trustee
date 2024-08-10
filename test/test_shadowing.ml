let code = {|
    let trust pwd = {
      let secret pass = "abcd1" in                           // this var has confidentiality secret(pwd)

      let fun checkpwd (guess : string) : bool = 
        pass = guess in
      handle: {checkpwd}
    } in

    let trust pwd = {
      let pass = "abcd2" in 
      let secret baba = "baba" in                           // this var has confidentiality secret(pwd)

      let fun checkpwd2 (guess : string) : bool = 
        pass = guess in
      handle: {checkpwd2}
    } in

    pwd.checkpwd
  |}
;;

let%expect_test "blocks shadowing" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Field checkpwd not found or not public in block at: (19, 5)-(19, 8)") |}]