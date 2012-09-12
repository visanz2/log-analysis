{

  open ArgParser

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
let dot          = ['.']
let sym          = ['_']
let letter       = ['a'-'z' 'A'-'Z']
let box          = letter letter ((letter | sym | negative |digit)+)
let filename     = ((letter | sym | negative |digit)+) dot (letter+)
let intval       = (negative)?(digit+)
let nl           = ['\n']
let ws           = [' ' '\t'] 

 
rule token = parse
  | "--exec"                 {EXEC}
  | "--listtime"             {LISTTIME}
  | "--stream"               {STREAM}
  | "--save"                 {SAVE}
  | "ended"|"e"|"E"    as e  {ENDED (e)}
  | "blocked"|"b"|"B"  as b  {BLOCK (b)}
  | "read"|"r"|"R"     as r  {READ  (r)}
  | "write"|"w"|"W"    as w  {WRITE (w)}
	| '{' 										 {OARG}
	| '}'											 {CARG}
	| '\\'										 {BAR}
  | ws+                      {token lexbuf}
  | nl+                      {token lexbuf}
  | intval as num            {NUM(Int64.of_string num)}   
  | box as b                 {BOXNAME (b) }
  | filename as f            {FILENAME (f)}
  | eof                      {EOF}
  | _                        {failwith((Lexing.lexeme lexbuf)  
                    ^ ": syntax error at " 
                    ^ (string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum)
                    ^ ","
                    ^ (string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_bol))
                   }
 
{
}