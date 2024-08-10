let code = 
  {|
    // Type System prevents data leakage
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = 
        declassify(pass = guess) in

      handle: {checkpwd}
    } in
    let filter = <"filter"> in
    filter pwd.checkpwd ["pippo", "abc", "abcd", "pluto", "paperino"]
  |}
;;

let%expect_test "Prevent filter data leak 1" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

let code = 
  {|
    let trust pwd = {
    let secret pass = "abcd" in 

    let fun checkpwd (guess : string) : bool = 
      declassify(pass = guess) in
    handle: {checkpwd}
  } in
  let filter = <"filter"> in
  if (let _ = (filter (pwd.checkpwd) ["pippo", "abc", "abcd", "pluto", "paperino"]) in true) 
    then print_string "hello" 
  else print_string "hi"
  |}
;;

let%expect_test "Prevent filter data leak 2" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

let code = 
  {|
    // try to print 
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = 
        declassify(pass = guess) in
      handle: {checkpwd}
    } in
    let filter = <"filter"> in
    let l = filter pwd.checkpwd ["pippo", "abc", "abcd", "pluto", "paperino"] in 
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
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]


(*
// In a version without confidential data, it provides pass but alert a possible tainted value
let trust pwd = {
  let pass = "abcd" in 

  let fun checkpwd (guess : string) : bool = 
    pass = guess in
  handle: {checkpwd}
} in

let filter = include <filter> in
filter pwd.checkpwd ["pippo", "abc", "abcd", "pluto", "paperino"]
*)