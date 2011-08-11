{
  open Hgen_types
  open Hgen_parser
  open Hgen_util
}

let identifier = [ 'a'-'z' 'A'-'Z' ] [ 'a'-'z' 'A'-'Z' '0'-'9' ]*
  (* No underscore! *)

let oct_literal = [ '+' '-' ]? '0' [ '0'-'7']*
let hex_literal = [ '+' '-' ]? "0x" [ '0'-'9' 'A'-'F' 'a'-'f' ]+
let dec_literal = [ '+' '-' ]? [ '1'-'9'] [ '0'-'9' ]*

let digit = [ '0'-'9' ]
let exponent = ['e' 'E'] [ '+' '-' ]? digit+
let dot_float_mantisse = ( digit* '.' digit+ ) | ( digit+ '.' )
let dot_float_literal = dot_float_mantisse exponent?
let exp_float_literal = digit+ exponent
let float_literal =
    [ '+' '-' ]? ( dot_float_literal | exp_float_literal )

let after_ast =
    [ ^ '*' '/' ] [ ^ '*' ]*

let c_comment =
    "/*" '*'* after_ast? ('*'+ after_ast)* '*'* "*/"

let blank = [ ' ' '\t' '\r' ]


rule scan bolloc =
  (* bolloc: location of the beginning of the line *)
  parse
    | identifier as id
        { let loc = location !bolloc lexbuf in
	  match id with
	    | "bool"        -> K_BOOL loc
	    | "enum"        -> K_ENUM loc
	    | "implements"  -> K_IMPLEMENTS loc
	    | "module"      -> K_MODULE loc
	    | "struct"      -> K_STRUCT loc
	    | "byte"        -> K_BYTE loc
	    | "exception"   -> K_EXCEPTION loc
	    | "throws"      -> K_THROWS loc
	    | "class"       -> K_CLASS loc
	    | "extends"     -> K_EXTENDS loc
	    | "interface"   -> K_INTERFACE loc
	    | "out"         -> K_OUT loc
	    | "true"        -> K_TRUE loc
	    | "const"       -> K_CONST loc
	    | "false"       -> K_FALSE loc
	    | "local"       -> K_LOCAL loc
	    | "sequence"    -> K_SEQUENCE loc
	    | "void"        -> K_VOID loc
	    | "dictionary"  -> K_DICTIONARY loc
	    | "short"       -> K_SHORT loc
	    | "double"      -> K_DOUBLE loc
	    | "idempotent"  -> K_IDEMPOTENT loc
	    | "long"        -> K_LONG loc
	    | "string"      -> K_STRING loc
	    | "int"         -> K_INT loc
	    | "float"       -> K_FLOAT loc
	    | _             -> IDENT(id, loc)
	}
    | '\\' (identifier as id)
	{ IDENT(id, location !bolloc lexbuf) }
    | '"'
        { let loc1 = location !bolloc lexbuf in
	  let s = String.concat "" (scan_string bolloc lexbuf) in
	  let loc2 = location !bolloc lexbuf in
          STRING_LITERAL(s, loc1, loc2) }
    | "//" [^ '\n']* '\n'
        { let bolloc' = bolloc_nl !bolloc lexbuf in
	  bolloc := bolloc';
          scan bolloc lexbuf
	}
    | c_comment as s
        { let n, m = count_lf s in
	  let bolloc' = bolloc_multi_nl !bolloc lexbuf n m in
	  bolloc := bolloc';
	  scan bolloc lexbuf
	}
    | oct_literal as lit
        { INT_LITERAL(Int64.of_string ("0o" ^ lit),
		      location !bolloc lexbuf) }
    | hex_literal as lit
        { INT_LITERAL(Int64.of_string lit,
		      location !bolloc lexbuf) }
    | dec_literal as lit
        { INT_LITERAL(Int64.of_string lit,
		      location !bolloc lexbuf) }
    | (float_literal as lit) [ 'f' 'F' ]?
        { FLOAT_LITERAL(float_of_string lit,
			location !bolloc lexbuf) }
    | '{'
	{ LBRACE (location !bolloc lexbuf) }
    | '}'
	{ RBRACE (location !bolloc lexbuf) }
    | ';'
	{ SEMI (location !bolloc lexbuf) }
    | ','
	{ COMMA (location !bolloc lexbuf) }
    | '<'
	{ LANGLE (location !bolloc lexbuf) }
    | '>'
	{ RANGLE (location !bolloc lexbuf) }
    | '('
	{ LPAREN (location !bolloc lexbuf) }
    | ')'
	{ RPAREN (location !bolloc lexbuf) }
    | '*'
	{ ASTERISK (location !bolloc lexbuf) }
    | '='
	{ EQUAL (location !bolloc lexbuf) }
    | "::"
	{ DCOLON (location !bolloc lexbuf) }
    | "["
	{ LBRACK (location !bolloc lexbuf) }
    | "]"
	{ RBRACK (location !bolloc lexbuf) }
    | "[["
	{ LDBRACK (location !bolloc lexbuf) }
    | "]]"
	{ RDBRACK (location !bolloc lexbuf) }
    | '#' blank* ("line" blank+)? (digit+ as d) blank* '\n'
        { if at_bol !bolloc lexbuf then (
            let bolloc' =
              { (bolloc_nl !bolloc lexbuf) with line = int_of_string d } in
	    bolloc := bolloc';
            scan bolloc lexbuf
	  )
	  else
	    let loc = location !bolloc lexbuf in
	    raise(Lexical_error(loc, "Unexpected character"))
        }
    | '#' blank* ("line" blank+)? (digit+ as d) blank+
                 '"' ([^ '"' ]* as name) '"' [^ '\n' ]* '\n'
        { if at_bol !bolloc lexbuf then (
	    let bolloc' =
              { (bolloc_nl !bolloc lexbuf) with
                  file = name;
                  line = int_of_string d
              } in
	    bolloc := bolloc';
            scan bolloc lexbuf
	  )
	  else
	    let loc = location !bolloc lexbuf in
	    raise(Lexical_error(loc,"Unexpected character"))
        }
    | blank +
       { scan bolloc lexbuf }
    | '\n'
       { let bolloc' = bolloc_nl !bolloc lexbuf in
	 bolloc := bolloc';
	 scan bolloc lexbuf }
    | eof
       { EOF (location !bolloc lexbuf) }
    | _
       {
	 let loc = location !bolloc lexbuf in
	 raise(Lexical_error(loc,"Unexpected character"))
       }

and scan_string bolloc =
  parse
    | "\n"
       { let loc = location !bolloc lexbuf in
	 raise(Lexical_error(loc,"Newline in string"))
       }
    | eof
       { let loc = location !bolloc lexbuf in
	 raise(Lexical_error(loc,"EOF in string"))
       }
    | '"'
       { [] }
    | '\\' ( [ '\\' '"' '\'' ] as c)
       { String.make 1 c :: scan_string bolloc lexbuf }
    | "\\n"
       { "\n" :: scan_string bolloc lexbuf }
    | "\\r"
       { "\r" :: scan_string bolloc lexbuf }
    | "\\t"
       { "\t" :: scan_string bolloc lexbuf }
    | "\\v"
       { "\011" :: scan_string bolloc lexbuf }
    | "\\f"
       { "\012" :: scan_string bolloc lexbuf }
    | "\\a"
       { "\007" :: scan_string bolloc lexbuf }
    | "\\b"
       { "\b" :: scan_string bolloc lexbuf }
    | "\\?"
       { "\063" :: scan_string bolloc lexbuf }
    | '\\' ( ['0'-'3']  ['0'-'7']?  ['0'-'7']? as s)
       { let n = int_of_string ("0o" ^ s) in
	 String.make 1 (Char.chr n) :: scan_string bolloc lexbuf
        }
    | '\\' 'x' (['0'-'9' 'A'-'F' 'a'-'f']+ as s)
       { let n = int_of_string ("0x" ^ s) in
	 let loc = location !bolloc lexbuf in
	 if n > 255 then raise(Lexical_error(loc,"Character out of range"));
	 String.make 1 (Char.chr n) :: scan_string bolloc lexbuf
       }
    | '\\' '\n'
       { let bolloc' = bolloc_nl !bolloc lexbuf in
         bolloc := bolloc';
	 "\n" :: scan_string bolloc lexbuf
       }
    | '\\' (_ as c)
       { String.make 1 c :: scan_string bolloc lexbuf }
    | (_ as c)
       { String.make 1 c :: scan_string bolloc lexbuf }


{
}
