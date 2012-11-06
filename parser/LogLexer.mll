{

  open LogParser

  let inc_lnum lexbuf =
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- { pos with
          Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
          Lexing.pos_bol = pos.Lexing.pos_cnum;
        }
      ;;
}

let digit        = ['0'-'9']
let intval       = digit+
let int         = '-'?digit+   
let nl           = ['\n']
let ws           = [' ' '\t'] 
 
rule token = parse
  | 'r'                   {MDREAD ('r')}
  | 'w'                   {MDWRITE ('w')}
  | 'A'                   {BLKANY ('A')}
  | 'Z'                   {ZOMBIE}
  | "Log format version"  {LOGVERSION}  
  | '('                   {OBRACKET}
  | "since"               {SINCE}
  | '/'                   {DIV}
  | ')'                   {CBRACKET}
  | '-'                   {FLGNILL ('-')}
  | '?'                   {FLGFIRST ('?')}
  | '!'                   {FLGSECOND ('!')}
  | '*'                   {FLGTHIRD ('*')}
  | '.'                   {DOT}
  | ','                   {COMA}
  | ';'                   {SMC}
  | ':'                   {COLON}
  | 'S'                   {STARTCHAR}
  | 'W'                   {WAITCHAR}
  | 'E'                   {ENDCHAR}
  | 'T'                   {TOTALWAIT}
  | 'C'                   {STSCLOSE ('C')}
  | 'I'                   {CHARI ('I')}
  | 'O'                   {CHARO ('O')}
  | 'R'                   {CHARR ('R')}
  | '|'                   {NDCHOICE}
  | '#'                   {inc_lnum lexbuf; HASH}
  | ws+                   {token lexbuf}
  | nl+                   {token lexbuf}
  | intval as bignum      {BIGNUM(Big_int.big_int_of_string bignum)}   
  | eof                   {EOF}
  | _                     {failwith((Lexing.lexeme lexbuf)  
                    ^ ": syntax error at " 
                    ^ (string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum)
                    ^ ","
                    ^ (string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_bol))
                   }
 
{
}
