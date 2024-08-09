(* The following tests must be done manually because there is input *)

// get_int()
// get_string()
// get_string() |> (lambda (x : string) : string -> x^"+1")
// get_int() |> (lambda (x : int) : int -> x+1)
// get_int() |> (lambda (x : string) : int -> 1) // Type Error!
// get_int() |> (lambda (x : int) : int -> x+1) // IO Type Error if non-int value
// get_char()
// get_bool()
// get_float() // note: int input is coerced into float !

let fun fact(n : int) : int = 
  let _ = print_int n in 
  if n = 0 then 
    1
  else
    n * fact (n - 1)
in get_int () |> fact
