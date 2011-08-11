# 3 "hydro_lexer.mll"
 
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

# 17 "hydro_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\239\255\240\255\094\000\212\000\243\255\244\255\245\255\
    \246\255\247\255\198\000\248\255\209\000\250\255\251\255\252\255\
    \253\255\254\255\255\255\217\000\249\255\027\001\243\255\244\255\
    \134\001\246\255\247\255\227\001\248\255\225\000\250\255\251\255\
    \252\255\253\255\254\255\255\255\233\000\249\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\014\000\013\000\255\255\255\255\255\255\
    \255\255\255\255\015\000\255\255\006\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\006\000\255\255\255\255\255\255\255\255\
    \010\000\255\255\255\255\011\000\255\255\006\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\006\000\255\255";
  Lexing.lex_default = 
   "\002\000\000\000\000\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\011\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\023\000\000\000\000\000\
    \255\255\000\000\000\000\028\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\004\000\004\000\000\000\000\000\004\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \004\000\003\000\009\000\003\000\003\000\003\000\003\000\008\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\005\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\007\000\003\000\003\000\003\000\003\000\003\000\
    \006\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\010\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\004\000\004\000\000\000\
    \000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \018\000\000\000\000\000\000\000\000\000\018\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\004\000\018\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\000\000\000\000\
    \001\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\000\000\018\000\000\000\000\000\000\000\000\000\000\000\
    \016\000\000\000\000\000\000\000\015\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\017\000\000\000\000\000\000\000\
    \014\000\000\000\013\000\024\000\024\000\026\000\024\000\024\000\
    \024\000\024\000\025\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\000\000\024\000\024\000\
    \024\000\024\000\024\000\000\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\027\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\024\000\024\000\
    \000\000\024\000\024\000\024\000\024\000\000\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \000\000\024\000\024\000\024\000\024\000\024\000\255\255\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\000\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\035\000\000\000\000\000\
    \000\000\000\000\035\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\035\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\022\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\035\000\
    \000\000\000\000\000\000\000\000\000\000\033\000\000\000\000\000\
    \000\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\034\000\000\000\000\000\000\000\031\000\000\000\030\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \255\255\003\000\003\000\003\000\003\000\255\255\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\255\255\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \255\255\003\000\003\000\003\000\003\000\003\000\255\255\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\255\255\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\004\000\004\000\255\255\
    \255\255\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \010\000\255\255\255\255\255\255\255\255\010\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\004\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\255\255\255\255\
    \000\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\255\255\010\000\255\255\255\255\255\255\255\255\255\255\
    \010\000\255\255\255\255\255\255\010\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\010\000\255\255\255\255\255\255\
    \010\000\255\255\010\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\255\255\021\000\021\000\
    \021\000\021\000\021\000\255\255\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\024\000\024\000\
    \255\255\024\000\024\000\024\000\024\000\255\255\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \255\255\024\000\024\000\024\000\024\000\024\000\010\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\255\255\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\027\000\255\255\255\255\
    \255\255\255\255\027\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\021\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\027\000\
    \255\255\255\255\255\255\255\255\255\255\027\000\255\255\255\255\
    \255\255\027\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\027\000\255\255\255\255\255\255\027\000\255\255\027\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\027\000";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec split_words lexbuf =
    __ocaml_lex_split_words_rec lexbuf 0
and __ocaml_lex_split_words_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 27 "hydro_lexer.mll"
 ( let tok = Chars (String.sub (Lexing.lexeme lexbuf) 1 1) in
	  tok :: split_words lexbuf
	)
# 248 "hydro_lexer.ml"

  | 1 ->
# 31 "hydro_lexer.mll"
 ( Chars "\010" :: split_words lexbuf )
# 253 "hydro_lexer.ml"

  | 2 ->
# 33 "hydro_lexer.mll"
 ( Chars "\008" :: split_words lexbuf )
# 258 "hydro_lexer.ml"

  | 3 ->
# 35 "hydro_lexer.mll"
 ( Chars "\012" :: split_words lexbuf )
# 263 "hydro_lexer.ml"

  | 4 ->
# 37 "hydro_lexer.mll"
 ( Chars "\013" :: split_words lexbuf )
# 268 "hydro_lexer.ml"

  | 5 ->
# 39 "hydro_lexer.mll"
 ( Chars "\009" :: split_words lexbuf )
# 273 "hydro_lexer.ml"

  | 6 ->
# 41 "hydro_lexer.mll"
 ( let tok = octal_char (Lexing.lexeme lexbuf) in
	  tok :: split_words lexbuf 
	)
# 280 "hydro_lexer.ml"

  | 7 ->
# 46 "hydro_lexer.mll"
 ( let tok = Chars (String.sub (Lexing.lexeme lexbuf) 1 1) in
	  tok :: split_words lexbuf )
# 286 "hydro_lexer.ml"

  | 8 ->
# 49 "hydro_lexer.mll"
 ( let qs = quoted_string '"' lexbuf in
	  qs @ split_words lexbuf
	)
# 293 "hydro_lexer.ml"

  | 9 ->
# 53 "hydro_lexer.mll"
 ( let qs = quoted_string '\'' lexbuf in
	  qs @ split_words lexbuf 
	)
# 300 "hydro_lexer.ml"

  | 10 ->
# 57 "hydro_lexer.mll"
 ( Colon :: split_words lexbuf )
# 305 "hydro_lexer.ml"

  | 11 ->
# 59 "hydro_lexer.mll"
 ( At :: split_words lexbuf )
# 310 "hydro_lexer.ml"

  | 12 ->
# 61 "hydro_lexer.mll"
 ( Slash :: split_words lexbuf )
# 315 "hydro_lexer.ml"

  | 13 ->
# 63 "hydro_lexer.mll"
 ( Sep :: split_words lexbuf )
# 320 "hydro_lexer.ml"

  | 14 ->
# 65 "hydro_lexer.mll"
 ( let tok = Chars (Lexing.lexeme lexbuf) in
	  tok :: split_words lexbuf
	)
# 327 "hydro_lexer.ml"

  | 15 ->
# 69 "hydro_lexer.mll"
 ( failwith "bad character found" )
# 332 "hydro_lexer.ml"

  | 16 ->
# 71 "hydro_lexer.mll"
 ( [ ] )
# 337 "hydro_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_split_words_rec lexbuf __ocaml_lex_state

and quoted_string delim lexbuf =
    __ocaml_lex_quoted_string_rec delim lexbuf 21
and __ocaml_lex_quoted_string_rec delim lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 76 "hydro_lexer.mll"
 ( let tok = Chars (String.sub (Lexing.lexeme lexbuf) 1 1) in
	  tok :: quoted_string delim lexbuf
	)
# 350 "hydro_lexer.ml"

  | 1 ->
# 80 "hydro_lexer.mll"
 ( Chars "\010" :: quoted_string delim lexbuf )
# 355 "hydro_lexer.ml"

  | 2 ->
# 82 "hydro_lexer.mll"
 ( Chars "\008" :: quoted_string delim lexbuf )
# 360 "hydro_lexer.ml"

  | 3 ->
# 84 "hydro_lexer.mll"
 ( Chars "\012" :: quoted_string delim lexbuf )
# 365 "hydro_lexer.ml"

  | 4 ->
# 86 "hydro_lexer.mll"
 ( Chars "\013" :: quoted_string delim lexbuf )
# 370 "hydro_lexer.ml"

  | 5 ->
# 88 "hydro_lexer.mll"
 ( Chars "\009" :: quoted_string delim lexbuf )
# 375 "hydro_lexer.ml"

  | 6 ->
# 90 "hydro_lexer.mll"
 ( let tok = octal_char (Lexing.lexeme lexbuf) in
	  tok :: quoted_string delim lexbuf
	)
# 382 "hydro_lexer.ml"

  | 7 ->
# 95 "hydro_lexer.mll"
 ( let tok = Chars (String.sub (Lexing.lexeme lexbuf) 1 1) in
	  tok :: quoted_string delim lexbuf
	)
# 389 "hydro_lexer.ml"

  | 8 ->
# 99 "hydro_lexer.mll"
 ( if delim = '"' then
	    []
	  else
	    Chars "\"" :: quoted_string delim lexbuf
	)
# 398 "hydro_lexer.ml"

  | 9 ->
# 105 "hydro_lexer.mll"
 ( if delim = '\'' then
	    []
	  else
	    Chars "'" :: quoted_string delim lexbuf
	)
# 407 "hydro_lexer.ml"

  | 10 ->
# 111 "hydro_lexer.mll"
 ( let tok = Chars (Lexing.lexeme lexbuf) in
	  tok :: quoted_string delim lexbuf
	)
# 414 "hydro_lexer.ml"

  | 11 ->
# 115 "hydro_lexer.mll"
 ( failwith "bad character found" )
# 419 "hydro_lexer.ml"

  | 12 ->
# 117 "hydro_lexer.mll"
 ( failwith "unfinished string literal" )
# 424 "hydro_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_quoted_string_rec delim lexbuf __ocaml_lex_state

;;
