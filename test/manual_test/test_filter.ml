(* Output: Security Error: The program could contain a Data leakage. 
   A plugin is used for filtering pwd - given scenario *)
// Type System prevents data leakage
let trust pwd = {
  let secret pass = "abcd" in 

  let fun checkpwd (guess : string) : bool = 
    declassify(pass = guess) in

  handle: {checkpwd}
} in
let filter = <"filter"> in
filter.string_f pwd.checkpwd ["pippo", "abc", "abcd", "pluto", "paperino"]
