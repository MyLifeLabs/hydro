{
  type token =
    | Chars of string
    | Colon     (* Unescaped occurrence of : *)
    | At        (* Unescaped occurrence of @ *)
    | Slash     (* Unescaped occurrence of / *)
    | Sep

  let octal_char s =
    let s' = String.sub s 1 (String.length s - 1) in
    let n = int_of_string ("0o" ^ s') in
    if n > 255 then failwith "Bad octal sequence";
    Chars (String.make 1 (Char.chr n))
}


(* Rule for splitting a stringified proxy, endpoint, or identity into
   words. split_words returns a list of tokens. Word splits are marked
   by Sep.
 *)

rule split_words = parse
    | '\\' [ '/' '\'' '"' '\\' ]
	(* The documented backslashed chars *)
	{ let tok = Chars (String.sub (Lexing.lexeme lexbuf) 1 1) in
	  tok :: split_words lexbuf
	}
    | '\\' 'n'
	{ Chars "\010" :: split_words lexbuf }
    | '\\' 'b'
	{ Chars "\008" :: split_words lexbuf }
    | '\\' 'f'
	{ Chars "\012" :: split_words lexbuf }
    | '\\' 'r'
	{ Chars "\013" :: split_words lexbuf }
    | '\\' 't'
	{ Chars "\009" :: split_words lexbuf }
    | '\\' [ '0'-'7' ] [ '0'-'7' ]? [ '0'-'7' ]?
	{ let tok = octal_char (Lexing.lexeme lexbuf) in
	  tok :: split_words lexbuf
	}
    | '\\' _
	(* Undocumented *)
	{ let tok = Chars (String.sub (Lexing.lexeme lexbuf) 1 1) in
	  tok :: split_words lexbuf }
    | '"'
	{ let qs = quoted_string '"' lexbuf in
	  qs @ split_words lexbuf
	}
    | '\''
	{ let qs = quoted_string '\'' lexbuf in
	  qs @ split_words lexbuf
	}
    | ':'
	{ Colon :: split_words lexbuf }
    | '@'
	{ At :: split_words lexbuf }
    | '/'
	{ Slash :: split_words lexbuf }
    | [' ' '\t' '\r' '\n' ]+
	{ Sep :: split_words lexbuf }
    | [^ '\000'-'\032' '\127'-'\255' '\\' '"' '\'' ':' '@' '/' ]+
	{ let tok = Chars (Lexing.lexeme lexbuf) in
	  tok :: split_words lexbuf
	}
    | _
	{ failwith "bad character found" }
    | eof
	{ [ ] }

and quoted_string delim = parse
    | '\\' [ '/' '\'' '"' '\\' ]
	(* The documented backslashed chars *)
	{ let tok = Chars (String.sub (Lexing.lexeme lexbuf) 1 1) in
	  tok :: quoted_string delim lexbuf
	}
    | '\\' 'n'
	{ Chars "\010" :: quoted_string delim lexbuf }
    | '\\' 'b'
	{ Chars "\008" :: quoted_string delim lexbuf }
    | '\\' 'f'
	{ Chars "\012" :: quoted_string delim lexbuf }
    | '\\' 'r'
	{ Chars "\013" :: quoted_string delim lexbuf }
    | '\\' 't'
	{ Chars "\009" :: quoted_string delim lexbuf }
    | '\\' [ '0'-'7' ] [ '0'-'7' ]? [ '0'-'7' ]?
	{ let tok = octal_char (Lexing.lexeme lexbuf) in
	  tok :: quoted_string delim lexbuf
	}
    | '\\' _
	(* Undocumented *)
	{ let tok = Chars (String.sub (Lexing.lexeme lexbuf) 1 1) in
	  tok :: quoted_string delim lexbuf
	}
    | '"'
	{ if delim = '"' then
	    []
	  else
	    Chars "\"" :: quoted_string delim lexbuf
	}
    | '\''
	{ if delim = '\'' then
	    []
	  else
	    Chars "'" :: quoted_string delim lexbuf
	}
    | [^ '\000'-'\031' '\127'-'\255' '\\' '"' '\'' ':' '@' ]+
	{ let tok = Chars (Lexing.lexeme lexbuf) in
	  tok :: quoted_string delim lexbuf
	}
    | _
	{ failwith "bad character found" }
    | eof
	{ failwith "unfinished string literal" }

