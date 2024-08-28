let code = {| 
  // plugin cannot be passed as parameter
  let plugin pwd = {
    let pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = 
      pass = guess in
    handle: {checkpwd}
  } in

  // plugin is not imported here for simplicity: automated test env doesn't find the file
  let plugin filter = {
    let fun string_f (predicate : string -> bool) (l : string list) : string list = 
      if l = [] then [] 
      else 
        if predicate (hd l) then (hd l)::(string_f predicate (tl l))
        else (string_f predicate (tl l))
    in handle: {string_f}
  } in 
  filter.string_f pwd.checkpwd ["pippo", "abc", "abcd", "pluto", "paperino"]
|};;

let%expect_test "Parse plugin 1" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let res = TFhree.Interpreter.eval code in
  print_endline (TFhree.Utils.string_of_value res);
  [%expect {| 
    Warning: the computed value can be tainted 
    [ abcd ]
  |}]