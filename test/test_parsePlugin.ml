let code = {|
    // correct (but tainted cause is a plugin)
    let plugin p = {
        let s = "s" in
        let fun f (x : int) : int = x+1
    in f
    } in p 5
|};;


let%expect_test "Parse plugin 1" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let res = TFhree.Interpreter.eval code in
  print_endline (TFhree.Utils.string_of_value res);
  [%expect {| 
    Warning: the computed value can be tainted 
    6
  |}]

let code = {|
  (* nested blocks! *)
  let plugin p = {
    let trust t = { let fun f : int = 1 in handle:{f} }
    in t
  } in p
|};;


let%expect_test "Parse plugin 2" =
let lexbuf = Lexing.from_string code in 
let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
try 
  let _ = TFhree.Type_system.type_check code in 
  TFhree.Interpreter.eval code |> ignore
with 
| exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
[%expect {| TFhree.Exceptions.Type_Error("Cannot have nested blocks. At: (4, 9)-(4, 56)")|}]

let code = {|
  // plugin code cannot use external variables
  let a = 5 in 
  let plugin p = {
      let s = 5 in
      let fun f (x : int) : int = x+1
      in (lambda (x : int) : int -> x + a)
  } in p
|};;


let%expect_test "Parse plugin 3" =
let lexbuf = Lexing.from_string code in 
let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
try 
let _ = TFhree.Type_system.type_check code in 
TFhree.Interpreter.eval code |> ignore
with 
| exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
[%expect {| TFhree.Exceptions.Binding_Error("a not found at: (7, 41)-(7, 42)")|}]

let code = {|
  // plugin code must be evaluated in a function
  let plugin p = {
      let s = 5 in
      let fun f (x : int) : int = x+1
      in s
  } in p
|};;


let%expect_test "Parse plugin 4" =
let lexbuf = Lexing.from_string code in 
let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
try 
let _ = TFhree.Type_system.type_check code in 
TFhree.Interpreter.eval code |> ignore
with 
| exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
[%expect {| TFhree.Exceptions.Type_Error("A plugin must implement a function. At: (4, 7)-(6, 11)") |}]