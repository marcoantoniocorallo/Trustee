let code = {|
    // correct (but tainted cause is a plugin)
    let plugin p = {
        let s = "s" in
        let fun f (x : int) : int = x+1
    in handle: {f}
    } in p.f 5
|};;


let%expect_test "Parse plugin 1" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let res = Trustee.Interpreter.eval code in
  print_endline (Trustee.Utils.string_of_value res);
  [%expect {| 
    Warning: the computed value can be tainted 
    6
  |}]

let code = {|
  (* nested blocks! *)
  let plugin p = {
    let trust t = { let fun f : int = 1 in handle:{f} }
    in handle: {t}
  } in p
|};;


let%expect_test "Parse plugin 2" =
let lexbuf = Lexing.from_string code in 
let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
try 
  let _ = Trustee.Type_system.type_check code in 
  Trustee.Interpreter.eval code |> ignore
with 
| exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
[%expect {| Trustee.Exceptions.Type_Error("Cannot have nested blocks. At: (4, 9)-(4, 56)")|}]

let code = {|
  // plugin code cannot use external variables
  let a = 5 in 
  let plugin p = {
      let s = 5 in
      let fun f (x : int) : int = x+1
      in handle: {(lambda (x : int) : int -> x + a)}
  } in p
|};;


let%expect_test "Parse plugin 3" =
let lexbuf = Lexing.from_string code in 
let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
try 
let _ = Trustee.Type_system.type_check code in 
Trustee.Interpreter.eval code |> ignore
with 
| exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
[%expect {| Trustee.Exceptions.Type_Error("An identifier was expected at: (7, 20)-(7, 51)")|}]

let code = {|
  // plugin code must be evaluated in a function
  let plugin p = {
      let s = 5 in
      let fun f (x : int) : int = x+1
      in handle: {s}
  } in p
|};;


let%expect_test "Parse plugin 4" =
let lexbuf = Lexing.from_string code in 
let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
try 
let _ = Trustee.Type_system.type_check code in 
Trustee.Interpreter.eval code |> ignore
with 
| exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
[%expect {| Trustee.Exceptions.Type_Error("A Function was expected at: (6, 19)-(6, 20)") |}]

let code = {|
  // plugin code cannot use external variables
  let a = 5 in 
  let plugin p = {
      let s = 5 in
      let fun f (x : int) : int = x+a
      in handle: {f}
  } in p
|};;


let%expect_test "Parse plugin 5" =
let lexbuf = Lexing.from_string code in 
let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
try 
let _ = Trustee.Type_system.type_check code in 
Trustee.Interpreter.eval code |> ignore
with 
| exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
[%expect {| Trustee.Exceptions.Binding_Error("a not found at: (6, 37)-(6, 38)")|}]