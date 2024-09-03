(* Closure of checkpwd *)
let code = 
  {|
    let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = declassify(pass = guess) in
      handle: {checkpwd}
    } in pwd.checkpwd
  |}
;;

let value = Trustee.Syntax.Closure("checkpwd","", Trustee.Utils.dummy_value, [])

let%test "Closure of checkpwd" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value

let code = 
  {|
    let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = pass = guess in
      handle: {checkpwd}
    } in pwd.checkpwd "pippo"
  |}
;;

let%expect_test "Closure of checkpwd - secret data" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

(* Failure! secret variable access -> field not found *)
let code = 
  {|
    let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = declassify(pass = guess) in
      handle: {checkpwd}
    } in pwd.pass
  |}
;;

let%expect_test "secret variable access -> field not found" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("Field pass not found or not public in block at: (7, 10)-(7, 13)") |}]

(* field non exists -> field not found *)
let code = 
  {|
    let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = declassify(pass = guess) in
      handle: {checkpwd}
    } in pwd.pas
  |}
;;

let%expect_test "field non exists -> field not found" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("Field pas not found or not public in block at: (7, 10)-(7, 13)") |}]

(* cannot use external var into blocks *)
let code = 
  {|
    let a = "pippo" in 
    let trust pwd = {
      let secret pass = "abcd" in

      let fun checkpwd (guess : string) : bool = a = guess in
      handle: {checkpwd}
    } in pwd.checkpwd "tt"
  |}
;;

let%expect_test "external var use" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Binding_Error("a not found at: (6, 50)-(6, 51)") |}]

(* FAILURE! pass is still secret even if has not keyword (i.e. is not handled) *)
let code = 
  {|
    let trust pwd = {
    let pass = "abcd" in

    let fun checkpwd (guess : string) : bool = guess = pass in
    handle: {checkpwd}
    } in pwd.pass
  |}
;;

let%expect_test "non-handled var" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("Field pass not found or not public in block at: (7, 10)-(7, 13)") |}]

(* more functions into handle *)
let code = 
  {|
  let trust pwd = {
    let secret pass = "abcd" in

    let fun checkpwd (guess : string) : bool = declassify(guess = pass) in
    let fun checkpwd2 : bool = true in
    handle: {checkpwd; checkpwd2}
  } in pwd.checkpwd2
  |}
;;

let value = Trustee.Syntax.Closure("checkpwd2","", Trustee.Utils.dummy_value, []);;

let%test "more handled fns" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value

let code =
  {|
    // access to private member // declassify break the IFA; remove it to prevent leak
    let trust pwd = {
      let secret pass = true in 

      let fun nonhandled : bool = declassify(pass) in 

      let fun call_checkpwd : bool = declassify(pass)
      in
      handle: {call_checkpwd}
    } in
    pwd.nonhandled()
  |}
;;

let%expect_test "private field access 1" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("Field nonhandled not found or not public in block at: (12, 5)-(12, 8)") |}]

let code = {|
  let plugin filter = {
    let fun string_f (predicate : string -> bool) (l : string list) : string list = 
      if l = [] then [] 
      else 
        if predicate (hd l) then (hd l)::(string_f predicate (tl l))
        else (string_f predicate (tl l))
    in
    let fun int_f (predicate : int -> bool) (l : int list) : int list = 
      if l = [] then [] 
      else 
        if predicate (hd l) then (hd l)::(int_f predicate (tl l))
        else (int_f predicate (tl l))
    in handle: {string_f}
  } in filter.int_f (lambda (x:int) : bool -> x % 2 = 0) [1, 2, 3]
|}

let%expect_test "private field access" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("Field int_f not found or not public in block at: (15, 8)-(15, 14)") |}]
;;

let code = {|
  let trust pwd = {
    let secret pass = "abcd" in

    let fun checkpwd (guess : string) : bool = declassify(pass = guess) in
    handle: {checkpwd}
  } in pwd."not var"
|};;

let%expect_test "non-var as field access" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("An identifier was expected in access operation at: (7, 12)-(7, 21)") |}]
;;

let code = {|
  let pippo = "pippo" in pippo.try
|};;

let%expect_test "access to non-block exp" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Type_Error("A block was expected in access operation at: (2, 26)-(2, 31)") |}]
;;
