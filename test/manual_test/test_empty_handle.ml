(* handle list cannot be empty -> syntax error *)
let trust pwd = {
  let pass = "abcd" in

  let fun checkpwd (guess : string) : bool = guess = pass in
  handle: {}
} in pwd