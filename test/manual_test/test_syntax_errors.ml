(*
(* handle list cannot be empty -> syntax error *)
let trust pwd = {
  let pass = "abcd" in

  let fun checkpwd (guess : string) : bool = guess = pass in
  handle: {}
} in pwd
*)

(*
(* handle list cannot be empty -> syntax error *)
let plugin pwd = {
  let pass = "abcd" in

  let fun checkpwd (guess : string) : bool = guess = pass in
  handle: {}
} in pwd
*)

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
// secret not allowed! -> type error (raised by parser)
let plugin p = {
    let secret s = "s"  in handle:{s}
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

(*
let fun f (x : int) : int = x + 1 in 
let plugin pwd = {
  let pass = "abcd" in

  let fun checkpwd (guess : string) : bool = pass = guess in
} in pwd
*)

(*
// try to declare a method public in a plugin interface
let plugin pwd = {
    let pass = "abcd" in

    let fun checkpwd (guess : string) : bool = guess = pass in

    handle: {checkpwd}
} in 
let fun f (b: plugin{ public checkpwd: (string -> bool)}) (s : string) : bool = (b.checkpwd) s in
f pwd "abcd"
*)

(*
(* Type Error! Only data can be secret (Parser exception!) *)
let trust pwd = {
  let secret pass = "abcd" in 

  let secret fun checkpwd (guess : string) : bool = declassify(pass = guess) in 
  handle: {[1,2]}
} in pwd
*)