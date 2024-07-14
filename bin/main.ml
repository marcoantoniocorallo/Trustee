open Exceptions;;
open Utils;;
open Type_system;;

let eval code = 
  let _ = type_check code in 
  print_endline (string_of_value (Interpreter.eval code)) 

let print_usage () = 
  print_endline "Usage: TFhree <filename>";
  print_endline "<filename> expected."
  
let () =
  let argvLength = Array.length Sys.argv in 
  if argvLength < 2 then print_usage()
  else
    let filename = Sys.argv.(argvLength-1) in 
    let lexbuf = ref None in  
    try
      lexbuf := Some (Lexing.from_channel (open_in filename));
      let code = Parser.main Lexer.tokenize (Option.get !lexbuf) in 
      eval code
    with
      | Sys_error(s) -> Printf.fprintf stderr "%s\n" s
      (* Character that doesn't match any case in lexer (i.e. '&')*)
      | Lexing_Error(s) -> Printf.fprintf stderr "%s\n" s
      (* A malformed sequence of tokens (i.e. "let x + 5" ) *)
      | Parser.Error ->  Printf.fprintf stderr "Syntax error at %s.\n%!" 
                        (string_of_position (Lexing.lexeme_start_p (Option.get !lexbuf)))
      (* Type Error *)
      | Type_Error(s) -> Printf.fprintf stderr "Type Error: %s\n" s
      (* Security Error *)
      | Security_Error(s) -> Printf.fprintf stderr "Security Error: %s\n" s
      (* Assertions *)
      | Assertion_Failure(s) -> Printf.fprintf stderr "Assertion Failure: %s\n" s
      (* Other exceptions raised by the interpreter *)
      | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn)