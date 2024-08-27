open TFhree.Syntax;;

let code = 
  {|
    // [ 2, 3, 4 ]
    let fun map (f : int -> int) (l : int list) : int list = 
        if l = [] then []
        else ((f (hd l)) :: (map f (tl l)))
    in map (lambda (x : int) : int -> x+1 ) [1, 2, 3]
  |}
;;

let value = ListV([ Int(2); Int(3); Int(4) ]);;

let%test "test_map1" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value
    
let code = 
  {|
    // [ Ehi cucù, Wooo cucù ]
    let fun map (f : string -> string) (l : string list) : string list = 
        if l=[] then []
        else f (hd l) :: map f (tl l)
    in map (lambda (x : string) : string -> x^" cucù" ) ["Ehi", "Wooo"]
  |}
;;

let value = ListV([ String("Ehi cucù"); String("Wooo cucù") ]);;

let%test "test_map2" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value
    
let code = 
  {|
    // [ 1, 2, 3, not a number ]
    let fun to_string (x : int) : string = 
        if x = 1 then "1"
        else if x = 2 then "2"
        else if x = 3 then "3"
        else "not a number" in 
    let fun map (f : int -> string) (l : int list) : string list = 
        if l=[] then []
        else f (hd l) :: map f (tl l)
    in map to_string [1, 2, 3, 4]
  |}
;;

let value = ListV([ String("1"); String("2"); String("3"); String("not a number")]);;

let%test "test_map3" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value