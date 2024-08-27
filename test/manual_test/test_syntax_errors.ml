(* handle list cannot be empty -> syntax error *)
let trust pwd = {
  let pass = "abcd" in

  let fun checkpwd (guess : string) : bool = guess = pass in
  handle: {}
} in pwd


(*
// Not Empty! -> syntax error
let plugin p = {

} in p 
*)

(* 
// miss "in" -> syntax error
let plugin p = {
    let s = "s"
} in p 
*)

(*
// secret not allowed! -> syntax error
let plugin p = {
    let secret s = "s"  in
} in p
*)

(*
(* Syntax error (handle expected) *)
let fun f (x : int) : int = x + 1 in 
let trust pwd = {
  let secret pass = "abcd" in

  let fun checkpwd (guess : string) : bool = pass = guess in
} in pwd
*)