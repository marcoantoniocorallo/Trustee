open Trustee.Syntax;;

let code = 
  {|
    let fun double(x : int) : int =
    x * 2
    in
    let fun twice(f : int -> int) : int -> int =
      let fun app(x : int) : int =
        f (f x)
      in
      app
    in
    twice double 4
  |}
;;

let value = Int(16);;

let%test "test_twice" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value

