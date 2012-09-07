%{
open LogAst
%}

%token  ZOMBIE
		DOT
		SMC
		COMA
		COLON
		OBRACKET
		DIV
		CBRACKET
		LOGVERSION
		SINCE
		ZOMBIE
		WAITCHAR
		STARTCHAR
		ENDCHAR
		TOTALWAIT
		STSCLOSE
        NDCHOICE
        HASH
        EOF

%token <int64>   NUM

%token <char>  CHARI
               CHARO
               CHARR
			   BLKANY 
			   STSCLOSE      
               MDREAD
               MDWRITE
	           FLGNILL
	           FLGFIRST
	           FLGSECOND
	           FLGTHIRD
                
%token WS
%token NL

%start src
%type <LogAst.sn_ast> src

%%

src: 
| LOGVERSION NUM DOT NUM OBRACKET SINCE NUM DIV NUM DIV NUM CBRACKET HASH src { $14 }
| NUM STARTCHAR HASH src { WorkStarted ($1, $4) }
| NUM WAITCHAR NUM HASH src { WorkWaited ( $1, $3, $5 ) }
| NUM ENDCHAR HASH src { WorkEnded ($1, $4) }
| NUM blockedby NUM NUM streamtrace HASH src { TaskBlocked ($1, $2, $3, $4, $5, $7) }
| NUM ZOMBIE NUM NUM NUM streamtrace HASH src { TaskEnded ( $1, $3, $4, $5, $6, $8) }
| WAITCHAR STSCLOSE NUM WAITCHAR TOTALWAIT NUM src { Information ($3, $6, $7) }
| EOF {Empty}
;

blockedby:
| CHARI  { $1 }
| CHARO  { $1 }
| BLKANY { $1 }
;

streamtrace:
| streamentry streamtrace { StreamTrace $1::$2 }
| messtrace streamtrace { ST_MessTrace $1::$2 }
| {[]}
;

streamentry:
| NUM mode state NUM firstflag secondflag thirdflag { StreamEntry ( $1, $2, $3, $4, $5, $6, $7) }
;

mode:
| MDREAD { $1 }
| MDWRITE { $1 }
| CHARI {$1}
;

state:
| STSCLOSE { $1 }
| CHARI { $1 }
| CHARO { $1 }
| CHARR { $1 }
;

firstflag:
| FLGNILL { $1 }  
| FLGFIRST { $1 }
;

secondflag:
| FLGNILL { $1 }  
| FLGSECOND { $1 }
;

thirdflag:
| FLGNILL { $1 }  
| FLGTHIRD { $1 }
;

messtrace:
| messentry moreinformation SMC { MessTrace ($1, $2) }
| messentry moreinformation COMA { MessTrace ($1, $2) }
;

moreinformation:
| NDCHOICE COLON NUM COLON listtagvalue NDCHOICE { MoreInformation ($3, $5) }
;

listtagvalue:
| NUM SMC listtagvalue {TagValue $1::$3}
| NUM COMA listtagvalue {TagValue $1::$3}
| NUM COLON listtagvalue {TagValue $1::$3}
| NUM  listtagvalue {TagValue $1::$2}
| {[]} 
;


messentry:
| NUM io NUM DOT NUM  { MessEntry ( $1, $2, $3, $5) }
;

io:
| CHARI { $1 }
| CHARO { $1 }
;

%%
