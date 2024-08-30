let code = {| let f = if (get_int()=0) then print_string else print_int in f |}

let%expect_test "fn comparison" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("If-Rule: branches have different types: then is (string -> unit), else is (int -> unit) - at Token: (1, 10)-(1, 59)") |}]