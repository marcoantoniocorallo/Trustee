let code = 
  {|
    // different
    let plugin filter1 = {
      let fun string_f (predicate : string -> bool) (l : string list) : string list = 
        if l = [] then [] 
        else 
          if predicate (hd l) then (hd l)::(string_f predicate (tl l))
          else (string_f predicate (tl l))
      in string_f
    } in 
    let plugin filter2 = {
      let fun int_f (predicate : int -> bool) (l : int list) : int list = 
        if l = [] then [] 
        else 
          if predicate (hd l) then (hd l)::(int_f predicate (tl l))
          else (int_f predicate (tl l))
      in int_f
    } in 
    filter1 = filter2
  |}
;;

let value = TFhree.Syntax.Bool(false)

let%test "Plugin equality - 1" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value
;;

let code = 
  {|
    // different
    let plugin filter1 = {
      let fun string_f (predicate : string -> bool) (l : string list) : string list = 
        if l = [] then [] 
        else 
          if predicate (hd l) then (hd l)::(string_f predicate (tl l))
          else (string_f predicate (tl l))
      in string_f
    } in 
    let plugin filter2 = {
      let fun string_f (predicate : string -> bool) (l : string list) : string list = 
        if l = [] then [] 
        else 
          if predicate (hd l) then (hd l)::(string_f predicate (tl l))
          else (string_f predicate (tl l))
      in string_f
    } in 
    filter1 = filter2
  |}
;;

let value = TFhree.Syntax.Bool(false)

let%test "Plugin equality - 2" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value
;;

let code = 
  {|
    // same
    let plugin filter1 = {
      let fun string_f (predicate : string -> bool) (l : string list) : string list = 
        if l = [] then [] 
        else 
          if predicate (hd l) then (hd l)::(string_f predicate (tl l))
          else (string_f predicate (tl l))
      in string_f
    } in 
    let plugin filter2 = {
      let fun string_f (predicate : string -> bool) (l : string list) : string list = 
        if l = [] then [] 
        else 
          if predicate (hd l) then (hd l)::(string_f predicate (tl l))
          else (string_f predicate (tl l))
      in string_f
    } in 
    filter1 = filter1
  |}
;;

let value = TFhree.Syntax.Bool(true)

let%test "Plugin equality - 3" =
  let (@@) v1 v2 = TFhree.Utils.test_cmp_values v1 v2 in 
  let lexbuf = Lexing.from_string code in 
  let code = TFhree.Parser.main TFhree.Lexer.tokenize lexbuf in 
  let _ = TFhree.Type_system.type_check code in 
  let vval = TFhree.Interpreter.eval code in 
  vval @@ value