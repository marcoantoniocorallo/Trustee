(** Header: define tokens, keywords and utilities for strings *)
{
	open Exceptions
  open Parser
  open Utils

	let create_hashtable size init =
		let tbl = Hashtbl.create size in
		List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
		tbl

	let keyword_table =
		create_hashtable 30 [
			("if", 	IF);
			("then",THEN);
			("else",ELSE);
			("let",	LET);
			("in",	IN);
			("fun",	FUN);
      ("lambda",LAMBDA);
      ("not", NOT);
      ("and", AND);
      ("or", OR);
	  	("proj", PROJ);
	  	("hd", HEAD);
		  ("tl", TAIL);
      ("int", TINT);
      ("char", TCHAR);
      ("float", TFLOAT);
      ("bool", TBOOL);
      ("string", TSTRING);
      ("list", TLIST);
      ("unit", TUNIT);
      ("trust", TRUST);
      ("secret",  SECRET);
      ("handle",  HANDLE);
      ("plugin", PLUGIN);
      ("assert", ASSERT);
      ("taint", TAINT);
      ("declassify", DECLASSIFY);
		]

}

(** Definition section *)
let digit 	= ['0'-'9']
let integer = digit+ 
let sign    = ('+' | '-')
let float 	= digit+ '.'? digit* (('e'|'E') sign? digit+)? |
							digit* '.'? digit+ (('e'|'E') sign? digit+)?
let bool 		= ("true"|"false")
let char		= "'" [^ '''] "'"
let string  = "\"" [^ '"']* "\""
let id 			= ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let white   = [' ' '\t']

rule tokenize = parse
	| integer as inum		{ INT(int_of_string inum)}
  | float as fnum 		{ FLOAT(float_of_string fnum) }
	| bool as b					{ BOOL (bool_of_string b) }
	| char as c					{ CHAR (c.[1]) }
	| string as s				{ STRING (String.sub s 1 ((String.length s)-2)) }
	| id as word        {
												try Hashtbl.find keyword_table word
												with Not_found -> ID word
											}
  | "<" string as s ">"
                      { (* bad but quick solution to menhir dependency cycle! *)
                        (* can include only files into the plugin dir! *)
                        let filename = String.sub s 2 ((String.length s)-3) in 
                        let substr = String.split_on_char '/' filename in 
                        match substr with 
                        | [x] -> 
                          let lb = Lexing.from_channel (open_in ("plugin/"^x)) in
                          let p = Parser.main tokenize lb in
                          PARSED p
                        | x::_::[] when x = "plugin" -> 
                          let lb = Lexing.from_channel (open_in filename) in
                          let p = Parser.main tokenize lb in
                          PARSED p
                        | _ -> raise(Access_Control_Error("Include: you can only specify a file into the plugin dir."))
                      }
  | '!'               { NOT }
  | "&&"              { AND }
  | "||"              { OR }
  | '.'               { DOT }
  | ','               { COMMA }
  | ';'               { SEMICOLON }
  | '^'               { CONCAT }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIV }
  | '%'               { MOD }
  | "+."              { FPLUS }
  | "-."              { FMINUS }
  | "*."              { FTIMES }
  | "/."              { FDIV }
  | '='               { EQ }
  | "<>"              { NEQ }
  | '<'               { LESS }
  | '>'               { GREATER }
  | "<="              { LEQ }
  | ">="              { GEQ }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | '['               { LBRACKET }
  | ']'               { RBRACKET }
  | '{'               { LCURLY }
  | '}'               { RCURLY }
  | "::"              { CONS_OP }
  | ":"               { COLON }
  | "->"              { ARROW }
  | "|>"              { PIPE }
	| "(*"							{ comments 0 lexbuf }
  | "//" [^ '\n']*    (* eat up one-line comments *)
  | white             (* eat up whitespace *)
											{ tokenize lexbuf }
  | '\n'              { Lexing.new_line lexbuf; tokenize lexbuf }
  | eof               { EOF }
	| _ 			          { raise (Lexing_Error("Unexpected character: "^(Lexing.lexeme lexbuf)^" at "^
												(string_of_position (Lexing.lexeme_start_p lexbuf))))
											}

and comments level = parse
	| "*)"  		        { if level = 0 then tokenize lexbuf else comments (level-1) lexbuf }
  | "(*"    	        { comments (level+1) lexbuf }
	|'\n'      	        { Lexing.new_line lexbuf; comments level lexbuf }
  | _					        { comments level lexbuf }
  | eof               { raise (Lexing_Error ("Non-closed comment !!!")) }
