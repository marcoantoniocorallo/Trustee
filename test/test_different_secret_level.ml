(* Note: these tests allow the data leak because the "filter" function is implemented
    by means of a trusted block (and the comparison fn is declassified); 
    the composition of trusted blocks is allowed by the language 
    and the safety of the composed program is demanded to the programmer
 *)

let code = 
  {|
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = 
        declassify(pass = guess) in
      handle: {checkpwd}
    } in
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
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  Trustee.Interpreter.eval code |> ignore;
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
  
let value = Trustee.Syntax.ListV([String("abcd")]);;  

let%test "Two different secret levels - returned value " =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value



(* same scenario, but the value filtered is jut printed *)
let code = 
  {|
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = 
        declassify(pass = guess) in
      handle: {checkpwd}
    } in
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
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  Trustee.Interpreter.eval code |> ignore;
  [%expect {| abcd |}]


(* same scenario as the first one, but without declassify => recognize and prevent leak *)
let code = 
  {|
    let trust pwd = {
      let secret pass = "abcd" in 

      let fun checkpwd (guess : string) : bool = 
        pass = guess in
      handle: {checkpwd}
    } in
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

let%expect_test "Two different secret levels - no declassify" =
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  try 
    let _ = Trustee.Type_system.type_check code in 
    Trustee.Interpreter.eval code |> ignore
  with 
  | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn);
  [%expect {| Trustee.Exceptions.Security_Error("The program could contain a Data leakage.") |}]
