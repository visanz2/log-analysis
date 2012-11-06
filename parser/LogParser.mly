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

%token <Big_int.big_int>   BIGNUM

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
| LOGVERSION BIGNUM DOT BIGNUM OBRACKET SINCE BIGNUM DIV BIGNUM DIV BIGNUM CBRACKET HASH src { $14 }
| BIGNUM STARTCHAR HASH src { WorkStarted ($1, $4) }
| BIGNUM WAITCHAR BIGNUM HASH src { WorkWaited ( $1, $3, $5 ) }
| BIGNUM ENDCHAR HASH src { WorkEnded ($1, $4) }
| BIGNUM blockedby BIGNUM BIGNUM streamtrace HASH src { TaskBlocked ($1, $2, (Big_int.int_of_big_int $3), $4, $5, $7) }
| BIGNUM ZOMBIE BIGNUM BIGNUM BIGNUM streamtrace HASH src { TaskEnded ( $1, (Big_int.int_of_big_int $3), $4, $5, $6, $8) }
| WAITCHAR STSCLOSE BIGNUM WAITCHAR TOTALWAIT BIGNUM src { Information ((Big_int.int_of_big_int $3), $6, $7) }
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
| BIGNUM mode state BIGNUM firstflag secondflag thirdflag { StreamEntry ( (Big_int.int_of_big_int $1), $2, $3, (Big_int.int_of_big_int $4), $5, $6, $7) }
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
| messentry moreinformation SMC { MessTraceWithInfo ($1, $2) }
| messentry moreinformation COMA { MessTraceWithInfo ($1, $2) }
| messentry SMC { MessTrace ($1) }
| messentry COMA { MessTrace ($1) }
;
 
moreinformation:
| NDCHOICE COLON BIGNUM COLON listtagvalue NDCHOICE { MoreInformation ($3, $5) }
;

listtagvalue:
| BIGNUM SMC listtagvalue {TagValue $1::$3}
| BIGNUM COMA listtagvalue {TagValue $1::$3}
| BIGNUM COLON listtagvalue {TagValue $1::$3}
| BIGNUM  listtagvalue {TagValue  $1::$2}
| {[]} 
;


messentry:
| BIGNUM io BIGNUM DOT BIGNUM  { MessEntry ( $1, $2, $3, $5) }
;

io:
| CHARI { $1 }
| CHARO { $1 }
;

%%
