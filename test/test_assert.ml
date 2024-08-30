let code = {| assert false |};;
let%expect_test "Assertion test 1" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Assertion_Failure("(1, 9)-(1, 14)") |}]
;;

let code = {| assert true |};;
let%expect_test "Assertion test 2" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  Trustee.Interpreter.eval code |> ignore;
  [%expect {| |}]
;;

let code = {| 
  let x = 5 in 
  assert (x <> 5)
|};;

let%expect_test "Assertion test 3" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Assertion_Failure("(3, 11)-(3, 17)") |}]
;;

let code = {|
  let trust pwd = {
  let secret pass = "abcd" in 

  let fun checkpwd (guess : string) : bool = 
    declassify(pass = guess) in
    handle: {checkpwd}
  } in

  let fun string_f (predicate : string -> bool) (l : string list) : string list = 
    if l = [] then [] 
    else 
      if predicate (hd l) then (hd l)::(string_f predicate (tl l))
      else (string_f predicate (tl l))
  in
  let l = string_f pwd.checkpwd ["pippo", "abc", "abcd", "pluto", "paperino"] in 
  assert (l = [])
|};;

let%expect_test "Assertion test 4" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Assertion_Failure("(17, 11)-(17, 17)") |}]
;;

let code = {|
  let trust pwd = {
  let secret pass = "abcd" in 

  let fun checkpwd (guess : string) : bool = 
    declassify(pass = guess) in
    handle: {checkpwd}
  } in

  let fun string_f (predicate : string -> bool) (l : string list) : string list = 
    if l = [] then [] 
    else 
      if predicate (hd l) then (hd l)::(string_f predicate (tl l))
      else (string_f predicate (tl l))
  in
  let l = string_f pwd.checkpwd ["pippo", "abc", "abcd", "pluto", "paperino"] in 
  assert (l <> [])
|};;

let%expect_test "Assertion test 5" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  Trustee.Interpreter.eval code |> ignore;
  [%expect {| |}]
;;

let code = {|
  let x = 5 in 
  assert taint (x <> 5)
|};;

let%expect_test "Assertion test 6" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  Trustee.Interpreter.eval code |> ignore;
  [%expect {| |}]
;;

let code = {|
  let fun checkpwd (s : string) : bool = s = "abcd" in 
  let plugin filter = {
    let fun string_f (predicate : string -> bool) (l : string list) : string list = 
      if l = [] then [] 
      else 
        if predicate (hd l) then (hd l)::(string_f predicate (tl l))
        else (string_f predicate (tl l))
    in handle: {string_f}
  } in 
  let l = filter.string_f checkpwd ["pippo", "abc", "abcd", "pluto", "paperino"] in 
  assert taint l
|};;

let%expect_test "Assertion test 7" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Assertion_Failure("Possible taint expression: (12, 16)-(12, 17)") |}]
;;