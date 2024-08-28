let code = {|
  // works
  let trust pwd = {
    let secret pass = "abcd" in 
    let fun checkpwd (guess : string) : bool = pass = guess in 
    handle: {checkpwd}
} in pwd.checkpwd
|};;

let value = TFhree.Syntax.Closure("checkpwd","", TFhree.Utils.dummy_value, []);;

let%test "Parse tb 1" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value
;;

let code = {|
  (* Failure ! checkpw is not declared *)
  let trust pwd = {
    let secret pass = "abcd" in 
    let fun checkpwd (guess : string) : bool = pass = guess in 
    handle: {checkpw}
  } in pwd
|};;

let%expect_test "Parse tb 2" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Binding_Error("checkpw not found at: (6, 14)-(6, 21)") |}]
;;

let code = {|
  (* FAILURE !!! Handle accepts only fn names *)
  let trust pwd = {
      let secret pass = "abcd" in 
      let id = "pippo" in 
      let fun checkpwd (guess : string) : bool = pass = guess in 
      handle: {checkpwd; (let x = "pippo" in x); [1, 2, 3]}
  } in pwd
|};;

let%expect_test "Parse tb 3" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("An identifier was expected at: (7, 27)-(7, 47)") |}]
;;

let code = {|
  (* FAILURE !!! NO NESTED BLOCKS *)
  let trust pwd = {
    let trust deeper = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = password = guess in 
      handle: {checkpwd; (let x = "pippo" in x); [1, 2, 3]}   
    } in 
    handle: {[1,2]}
  } in pwd
|};;

let%expect_test "Parse tb 4" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Cannot have nested blocks. At: (4, 9)-(9, 6)") |}]
;;

(* 
*)

let code = {|
  (* FAILURE !!! NO NESTED BLOCKS *)
  let trust pwd = {
  let e = 
    let f = 
      let trust deeper = {
        let secret pass = "abcd" in 

        let fun checkpwd (guess : string) : bool = password = guess in 
        handle: {checkpwd; (let x = "pippo" in x); [1, 2, 3]}   
      } in f 
    in e in 
  handle: {[1,2]}
} in pwd
|};;

let%expect_test "Parse tb 5" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Cannot have nested blocks. At: (6, 11)-(11, 8)") |}]
;;

let code = {|
  (* FAILURE !!! Only data can be secret *)
  let trust pwd = {
    let secret pass = "abcd" in 

    let secret fun checkpwd (guess : string) : bool = pass = guess in 
    handle: {[1,2]}
  } in pwd
|};;

let%expect_test "Parse tb 6" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Only data can be secret. At: (6, 16)-(6, 67)") |}]
;;

let code = {|
  (* Failure! only fn names in handle *)
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = pass = guess in 
    handle: {pass}
  } in pwd
|};;

let%expect_test "Parse tb 7" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("A Function was expected at: (7, 14)-(7, 18)") |}]
;;

let code = {|
  (* Failure! Handled Functions must be declared inside the block *)
  let fun f (x : int) : int = x + 1 in 
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = pass = guess in 
    handle: {f}
  } in pwd
|};;

let%expect_test "Parse tb 8" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Binding_Error("f not found at: (8, 14)-(8, 15)") |}]
;;

let code = {|
  (* Failure! An identifier was expected *)
  let fun f (x : int) : int = x + 1 in
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = pass = guess in
    handle: {(lambda : (string -> bool) -> checkpwd)}
  } in pwd
|};;

let%expect_test "Parse tb 9" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("An identifier was expected at: (8, 15)-(8, 52)") |}]
;;

let code = {|
  let trust pwd = {
  let secret pass = "abcd" in 

  let fun checkpwd (guess : string) : bool = pass = guess in 
  handle: {checkpwd}
} in pwd.checkpwd
|};;

let value = TFhree.Syntax.Closure("checkpwd","", TFhree.Utils.dummy_value, []);;

let%test "Parse tb 10" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value
;;

let code = {|
  (* plugin in a trust block is not recognized (nested block) *)
  let trust pwd = {
    let secret pass = "abcd" in 
    let plugin p = { let s="s"  in handle:{s}} in 
    let fun checkpwd (guess : string) : bool = pass = guess in 
    handle: {checkpwd}
  } in pwd.checkpwd
|};;

let%expect_test "Parse tb 11" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Cannot have nested blocks. At: (5, 22)-(5, 46)") |}]
;;

let code = {|
  (* Again nested blocks *)
  let trust pwd = {
    let secret pass = "abcd" in 
    let pippo = "hello" in 
      let pluto = "world" in 
        let fun f (x : int) : int = x+1 in 
          let plugin p = { let s="s" in handle: {s} } in 
    let xx = pippo^pluto in 
    let fun checkpwd (guess : string) : bool = pass = guess in 
    handle: {checkpwd}
  } in pwd.checkpwd
|};;

let%expect_test "Parse tb 12" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Cannot have nested blocks. At: (8, 28)-(8, 52)") |}]
;;