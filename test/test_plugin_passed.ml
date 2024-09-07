(* plugin passed as parameters test *)

let code = {|
  // plugin passed
  let plugin pwd = {
      let pass = "abcd" in

      let fun checkpwd (guess : string) : bool = true in

      handle: {checkpwd}
  } in 
  let fun f (b: plugin{ checkpwd: (string -> bool)}) (s : string) : bool = (b.checkpwd) s in
  f pwd "abcd"
|};;

let%expect_test "plugin passed" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  Trustee.Interpreter.eval code |> Trustee.Utils.string_of_value |> print_endline;
  [%expect {| 
  Warning: the computed value can be tainted
  true
  |}]
;;


let code = {|
  // block with different interface 1 -> type error
  let plugin pwd = {
      let  pass = "abcd" in

      let fun checkpwd (guess : string) : bool = true in
      let fun checkpw  (guess : string) : bool = true in

      handle: {checkpwd; checkpw}
  } in 
  let fun f (b: plugin{ checkpwd: (string -> bool)}) (s : string) : bool = (b.checkpwd) s in
  f pwd "abcd"
|};;

let%expect_test "block with different interface 1" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("functional application: argument type mismatch: (12, 5)-(12, 8)function Plugin type -> string -> bool got a block with different interface") |}]
;;

let code = {|
  // normal with more than one handled function
  let plugin pwd = {
      let  pass = "abcd" in

      let fun checkpwd (guess : string) : bool = true in
      let fun checkpw  (guess : string) : bool = true in

      handle: {checkpwd; checkpw}
  } in 
  let fun f (b: plugin{ checkpwd: (string -> bool);  checkpw: (string -> bool)}) (s : string) : bool = (b.checkpwd) s in
  f pwd "abcd"
|};;

let%expect_test "normal with more than one handled function" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let res = Trustee.Interpreter.eval code in 
  print_endline (Trustee.Utils.string_of_value res);
  [%expect {| 
  Warning: the computed value can be tainted
  true
  |}] 
  
;;

let code = {|
  // plugin returned
  let plugin pwd = {
      let  pass = "abcd" in

      let fun checkpwd (guess : string) : bool = true in
      let fun checkpw  (guess : string) : bool = true in

      handle: {checkpwd; checkpw}
  } in 
  let fun f (s : string) : plugin{ checkpwd: (string -> bool);  checkpw: (string -> bool)} = pwd in
  f "abcd"
|};;

let%expect_test "Plugin returned" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let res = Trustee.Interpreter.eval code in 
  print_endline (Trustee.Utils.string_of_value res);
  [%expect {| <Plugin> |}]
;;

let code = {|
  let trust tb = {
    let secret pwd = "pwd" in 
    let fun tk_arg (f : plugin{ f: unit -> int }) : int = f.f()
    in handle: {tk_arg}
} in 
let plugin f = {let fun f : int = 5 in handle: {f}} in 
tb.tk_arg f
|};;

let%expect_test "plugin into tb blocked" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("Cannot access to plugin from inside trusted blocks.") |}]
;;

let code = {|
// plugin conf in tb -> blocked
let trust tb = {
    let secret pwd = "pwd" in 
    let fun tk_arg (f: unit -> int) : int = f()
    in handle: {tk_arg}
} in 
let plugin f = {let fun f : int = 5 in handle: {f}} in 
tb.tk_arg (f.f)
|};;

let%expect_test "plugin into tb blocked" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;