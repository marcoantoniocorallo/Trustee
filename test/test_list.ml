open Trustee.Syntax;;

let code = 
  {|
  [1, 2] :: []
  |}
;;

let value = ListV([ListV([Int(1); Int(2)])]);;

let%test "test_list" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value

let code = 
  {|
  [1, 's'] :: []
  |}
;;

let%expect_test "test_list_type_error" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("Lists must contain homogeneous-type items. At: (2, 3)-(2, 11)") |}]