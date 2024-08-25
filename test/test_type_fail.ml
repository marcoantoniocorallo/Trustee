let code = 
  {|
  let s1 = "Hello" in 
    let s2 = 5 in 
    s1^s2
  |}
;;

let%expect_test "test_type_fail0" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Error in the arguments of ^: (4, 5)-(4, 10)") |}]


let code = 
  {|
    let add1 = 5 in 
    let add2 = 'c' in 
    add1+add2
  |}
;;

let%expect_test "test_type_fail1" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("Error in the arguments of +: (4, 5)-(4, 14)") |}]

let code = 
  {|
    let l = [3, 2, 7] in    //useless definition
    let fun square_of_head (l : int list) : int = 
    let h = hd l in 
      h*h
    in square_of_head ['a','b']
  |}
;;

let%expect_test "test_type_fail2" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("functional application: argument type mismatch(6, 23)-(6, 32)function (int list -> int) got char list instead") |}]


let code = 
  {|
    (fun f (n:int) : int = n*n) "c"
  |}
;;

let%expect_test "test_type_fail3" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Type_Error("functional application: argument type mismatch(2, 33)-(2, 36)function (int -> int) got string instead") |}]

  let code = 
    {|
      let t : (int * int * int) = (1,"ciao",3) in t
    |}
  ;;
  
  let%expect_test "test_type_fail4" =
    let lexbuf = Lexing.from_string code in 
    let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
    try 
      let _ = TFhree.Type_system.type_check code in 
      TFhree.Interpreter.eval code |> ignore
    with 
    | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
    [%expect {| TFhree.Exceptions.Type_Error("Bad type annotation at (2, 7)-(2, 52)") |}]
  