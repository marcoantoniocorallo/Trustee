let code = 
  {|
    // note: if the value returned by the program was v2, there should be data leakage!
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = 
        declassify(pass = guess) in
      handle: {checkpwd}
    } in

    // note: the following tb has not secret-level data! 
    // But public/normal is at the same level of secret in the lattice so their join is Top
    let trust filter = {
      let fun string_f (predicate : string -> bool) (l : string list) : string list = 
        if l = [] then [] 
        else 
          if predicate (hd l) then (hd l)::(string_f predicate (tl l))
          else (string_f predicate (tl l))
        in handle: {string_f}
    } in

    let v1 = pwd.checkpwd "pippo" in 
    let v2 = filter.string_f (lambda (x: string) : bool -> x = "abcd") ["pippo", "abc", "abcd", "pluto", "paperino"] in
    print_string "Two different secret levels in the same program but it works as it should do"
  |}
;;

let%expect_test "Two different secret levels" =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  TFhree.Interpreter.eval code |> ignore;
  [%expect {| Two different secret levels in the same program but it works as it should do |}]

(* same scenario, but the value filtered is returned *)
let code = 
  {|
    // note: if the value returned by the program was v2, there should be data leakage!
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = 
        declassify(pass = guess) in
      handle: {checkpwd}
    } in

    // note: the following tb has not secret-level data! 
    // But public/normal is at the same level of secret in the lattice so their join is Top
    let trust filter = {
      let fun string_f (predicate : string -> bool) (l : string list) : string list = 
        if l = [] then [] 
        else 
          if predicate (hd l) then (hd l)::(string_f predicate (tl l))
          else (string_f predicate (tl l))
        in handle: {string_f}
    } in

    let v1 = pwd.checkpwd "pippo" in 
    filter.string_f pwd.checkpwd ["pippo", "abc", "abcd", "pluto", "paperino"]
  |}
;;
  
let%expect_test "Two different secret levels - returned value " =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]

(* same scenario, but the value filtered is jut printed *)
let code = 
  {|
    // note: if the value returned by the program was v2, there should be data leakage!
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = 
        declassify(pass = guess) in
      handle: {checkpwd}
    } in

    // note: the following tb has not secret-level data! 
    // But public/normal is at the same level of secret in the lattice so their join is Top
    let trust filter = {
      let fun string_f (predicate : string -> bool) (l : string list) : string list = 
        if l = [] then [] 
        else 
          if predicate (hd l) then (hd l)::(string_f predicate (tl l))
          else (string_f predicate (tl l))
        in handle: {string_f}
    } in

    let v1 = pwd.checkpwd "pippo" in 
    (hd (filter.string_f pwd.checkpwd ["pippo", "abc", "abcd", "pluto", "paperino"]) )
    |> print_string
  |}
;;
  
let%expect_test "Two different secret levels - printed value " =
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  try 
    let _ = TFhree.Type_system.type_check code in 
    TFhree.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| TFhree.Exceptions.Security_Error("The program could contain a Data leakage.") |}]