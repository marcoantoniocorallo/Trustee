let code = {|
  // plugin can be passed as parameter into a trusted block
  // but the interaction between them led to a potential data leak => blocked
  let plugin succ = {
    let s = "s" in
    let fun f (x : int) : int = x+1
    in f
  } in
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 

    let fun take_arg (foo : (int -> int)) : int = 
      foo 7 in 
    handle: {checkpwd; take_arg}
  } in 
  pwd.take_arg succ // data leak (prevented)
|};;

let%expect_test "Plugin as param 1" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;

let code = {|
  // plugin can be passed as parameter into a trusted block
  // but the interaction between them led to a potential data leak => blocked
  let plugin succ = {
    let s = "s" in
    let fun f (x : int) : int = x+1
    in f
  } in
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 

    let fun take_arg (foo : (int -> int)) : int = 
      foo 7 in 
    handle: {checkpwd; take_arg}
  } in 
  pwd.take_arg (lambda (x:int) : int -> x+1) // allowed
|};;

let%test "Plugin as param 2" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  TFhree.Utils.test_cmp_values (TFhree.Interpreter.eval code) (Int(8))
;;

let code = {|
  // more malicious example
  let plugin filter = {
    let fun string_f (predicate : string -> bool) (l : string list) : string list = 
      if l = [] then [] 
      else 
        if predicate (hd l) then (hd l)::(string_f predicate (tl l))
        else (string_f predicate (tl l))
    in string_f
  } in 
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 

    let fun take_arg 
    (foo : ((string -> bool) -> (string list) -> string list)) : string list = 
      foo checkpwd ["pippo", "abcd", "pluto", "abc"]
    in 
    handle: {checkpwd; take_arg}
  } in 
  pwd.take_arg filter // not allowed -> data leak (prevented)!
|};;

let%expect_test "Plugin as param 3" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;

let code = {|
  let plugin filter = {
    let fun string_f (predicate : string -> bool) (l : string list) : string list = 
      if l = [] then [] 
      else 
        if predicate (hd l) then (hd l)::(string_f predicate (tl l))
        else (string_f predicate (tl l))
    in string_f
  } in 
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 

    let fun take_arg 
    (foo : ((string -> bool) -> (string list) -> string list)) : string list = 
      foo checkpwd ["pippo", "abcd", "pluto", "abc"]
    in 
    handle: {checkpwd; take_arg}
  } in 

  // allowed because string_f is std code, not a plugin
  let fun string_f (predicate : string -> bool) (l : string list) : string list = 
    if l = [] then [] 
    else 
      if predicate (hd l) then (hd l)::(string_f predicate (tl l))
      else (string_f predicate (tl l))
    in pwd.take_arg string_f
|};;

let%test "Plugin as param 4" =
let lexbuf = Lexing.from_string code in 
let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
let _ = TFhree.Type_system.type_check code in 
TFhree.Utils.test_cmp_values (TFhree.Interpreter.eval code) (ListV([String("abcd")]))
;;

let code = {|
  // what if a function that returns a plugin is passed as parameter, 
  // and then the returned plugin is used into a tb?
  // --> data leak identified!
  let plugin succ = {
    let s = "s" in
    let fun f (x : int) : int = x+1
    in f
  } in
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 

    let fun take_arg (foo : unit -> (int -> int)) : int = 
      foo() 7

    in 
    handle: {checkpwd; take_arg}
  } in 
  pwd.take_arg (lambda : (int->int) -> succ )
  //pwd.take_arg (lambda : (int->int) -> lambda (x:int) : int -> x+1 )
|};;

let%expect_test "Plugin as param 5" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;

let code = {|
  // now it works because it doesn't use a plugin
  let plugin succ = {
    let s = "s" in
    let fun f (x : int) : int = x+1
    in f
  } in
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 

    let fun take_arg (foo : unit -> (int -> int)) : int = 
      foo() 7

    in 
    handle: {checkpwd; take_arg}
  } in 
  pwd.take_arg (lambda : (int->int) -> lambda (x:int) : int -> x+1 )
|};;

let%test "Plugin as param 6" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  TFhree.Utils.test_cmp_values (TFhree.Interpreter.eval code) (Int(8))
;;

let code = {|
  // plugin blocked
  let plugin filter = {
    let fun string_f (predicate : string -> bool) (l : string list) : string list = 
      if l = [] then [] 
      else 
        if predicate (hd l) then (hd l)::(string_f predicate (tl l))
        else (string_f predicate (tl l))
    in string_f
  } in 
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 

    let fun take_arg 
    (foo : unit -> ((string -> bool) -> (string list) -> string list)) : string list = 
      foo() checkpwd ["pippo", "abcd", "pluto", "abc"]
    in 
    handle: {checkpwd; take_arg}
  } in 

  // allowed
  let fun string_f (predicate : string -> bool) (l : string list) : string list = 
    if l = [] then [] 
    else 
      if predicate (hd l) then (hd l)::(string_f predicate (tl l))
      else (string_f predicate (tl l))

  // not allowed -> data leak (prevented)!
  in pwd.take_arg (lambda : ((string -> bool) -> (string list) -> string list) -> filter )
|};;

let%expect_test "Plugin as param 7" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
;;

let code = {|
  // now it works because it doesn't use a plugin
  let plugin filter = {
    let fun string_f (predicate : string -> bool) (l : string list) : string list = 
      if l = [] then [] 
      else 
        if predicate (hd l) then (hd l)::(string_f predicate (tl l))
        else (string_f predicate (tl l))
    in string_f
  } in 
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = declassify(pass = guess) in 

    let fun take_arg 
    (foo : unit -> ((string -> bool) -> (string list) -> string list)) : string list = 
      foo() checkpwd ["pippo", "abcd", "pluto", "abc"]
    in 
    handle: {checkpwd; take_arg}
  } in 

  // allowed
  let fun string_f (predicate : string -> bool) (l : string list) : string list = 
    if l = [] then [] 
    else 
      if predicate (hd l) then (hd l)::(string_f predicate (tl l))
      else (string_f predicate (tl l))
  in pwd.take_arg (lambda : ((string -> bool) -> (string list) -> string list) -> string_f)
|};;

let%test "Plugin as param 8" =
let lexbuf = Lexing.from_string code in 
let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
let _ = TFhree.Type_system.type_check code in 
TFhree.Utils.test_cmp_values (TFhree.Interpreter.eval code) (ListV([String("abcd")]))
;;