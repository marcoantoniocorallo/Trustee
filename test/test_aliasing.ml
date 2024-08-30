open Trustee.Syntax;;

let code = 
  {|
  let sum2 = 
        lambda ( x : int ) : int -> x + 2
    in sum2 3
  |}
;;

let value = Int(5);;

let%test "sum2_1" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value


let code =
  {|
  let sum2 = 
        fun _ ( x : int ) : int = x + 2
    in sum2 3
  |}
;;

let value = Int(5);;

let%test "sum2_2" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value


let code = 
  {|
  let fun sum2 (x : int) : int = x + 2
    in sum2 3
  |}
;;

let value = Int(5);;

let%test "sum2_3" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value


let code = 
  {|
  let fact =
      fun f (n : int) : int = 
          if n = 0 then 1
          else n * f (n - 1)
  in fact 5
  |}
;;

let value = Int(120);;

let%test "fact1" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 
  vval @@ value


let code = 
  {|
  let fun fact (n : int) : int =
        if n = 0 then 1
        else n * fact (n - 1)
    in fact 5
  |}
;;

let value = Int(120);;

let%test "fact2" =
  let (@@) v1 v2 = Trustee.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = Trustee.Parser.main Trustee.Lexer.tokenize lexbuf in 
  let _ = Trustee.Type_system.type_check code in 
  let vval = Trustee.Interpreter.eval code in 

  vval @@ value