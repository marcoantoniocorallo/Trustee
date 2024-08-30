(* Output: Security Error: The program could contain a Data leakage. 
   A plugin is used for filtering pwd - given scenario *)
let code = 
  {|
  // Type System prevents data leakage
  let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = 
      declassify(pass = guess) in

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
|}
;;

let%expect_test "Prevent filter data leak 1" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]


let code = 
  {|
    let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = 
      declassify(pass = guess) in
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
  if (let _ = (filter.string_f (pwd.checkpwd) ["pippo", "abc", "abcd", "pluto", "paperino"]) in true) 
    then print_string "hello" 
  else print_string "hi"
  |}
;;

let%expect_test "Prevent filter data leak 2" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

let code = 
  {|
    // try to print 
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = 
        declassify(pass = guess) in
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
    let l = filter.string_f pwd.checkpwd ["pippo", "abc", "abcd", "pluto", "paperino"] in 
    let fun print_list (ls : string list) : unit = 
      if ls <> [] then 
        let _ = print_string (hd ls) in 
        print_list (tl ls)
      else print_string "finished"
    in print_list l
  |}
;;

let%expect_test "Prevent filter data leak 3" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]