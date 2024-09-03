let code = 
  {|
    // If returns different trusted blocks -> Type Error
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 
      handle: {checkpwd}
    } in 
    let trust t = { 
        let secret pwd = "pwd" in 
        let fun f : bool = false in 
        handle:{f} 
    } in 
    let b = if (get_int() = 1) then pwd else t
    in b.f
  |}
;;

let%expect_test "TB If - 1" =
let lexbuf = Lexing.from_string code in 
let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
try 
let _ = Trustee.Type_system.type_check code in 
Trustee.Interpreter.eval code |> ignore
with 
| exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
[%expect {| Trustee.Exceptions.Type_Error("If-Rule: branches return different trusted blocks. At Token: (14, 13)-(14, 47)") |}]

let code = 
  {|
    // Field not present -> Type error
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 
      handle: {checkpwd}
    } in 
    let trust t = { 
        let secret pwd = "pwd" in 
        let fun f : bool = false in 
        handle:{f} 
    } in 
    let b = if (get_int() = 1) then pwd else pwd
    in b.f
  |}
;;

let%expect_test "TB If - 2" =
let lexbuf = Lexing.from_string code in 
let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
try 
let _ = Trustee.Type_system.type_check code in 
Trustee.Interpreter.eval code |> ignore
with 
| exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
[%expect {| Trustee.Exceptions.Type_Error("Field f not found or not public in block at: (15, 8)-(15, 9)") |}]
;;

let code = {|
  // It works!
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 
    handle: {checkpwd}
  } in 
  let trust t = { 
      let secret pwd = "pwd" in 
      let fun f : bool = false in 
      handle:{f} 
  } in 
  let b = if (true) then t else t
  in b.f
  |}
;;

let value = Trustee.Syntax.Closure("f","", Trustee.Utils.dummy_value, []);;

let%test "TB If - 3" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value