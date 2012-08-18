{
  open MapParser

  let inc_lnum lexbuf =
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- { pos with
          Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
          Lexing.pos_bol = pos.Lexing.pos_cnum;
        }
      ;;
} 

let digit        = ['0'-'9']
let negative     = ['-']
let sym          = ['_']
let letter       = ['a'-'z' 'A'-'Z']
let box          = letter letter ((letter | sym | negative |digit)+)
let intval       = (negative)?(digit+)
let nl           = ['\n']
let ws           = [' ' '\t'] 

rule token = parse
  | 'S'                 {SCOMP ('S')}
  | 'P'                 {PCOMP ('P')}
  | 'R'                 {SREPL ('R')}
  | 'I'                 {PREPL ('I')}
  | ':'                 {COLON}
  | '<'                 {SMALLER}
  | '>'                 {GREATER}
  | '#'                 {inc_lnum lexbuf; HASH}
  | ws+                 {token lexbuf}
  | nl+                 {token lexbuf}
  | intval as num       {NUM(int_of_string num)}   
  | box as b             {BOXNAME (b) }
  | eof                 {EOF}
  | _                   {failwith((Lexing.lexeme lexbuf)  
                        ^ ": syntax error at " 
                        ^ (string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum)
                        ^ ","
                        ^ (string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_bol))
                   }

{
}
