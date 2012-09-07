%{
open MapAst
%}

%token  SMALLER
		GREATER
		DOT
        OBRACKET
        DIV
		CBRACKET  
		LOGVERSION
        SINCE 
        COLON     
        HASH
        EOF

%token <int64>   NUM

%token <char>  SCOMP
			   PCOMP
			   SREPL
			   PREPL

%token <string> BOXNAME 
                
%token WS
%token NL

%start src
%type <MapAst.sn_ast> src


%%
  
src: 
| LOGVERSION NUM DOT NUM OBRACKET SINCE NUM DIV NUM DIV NUM CBRACKET HASH src { $14 }
| NUM netpath boxnames NUM HASH src {MappingEntries ( $1, $2, $3, $4, $6) }
| EOF {Empty}
;

netpath:
| COLON pos netpath { $2::$3 }
| {[]}
;
 
pos:
| SCOMP NUM { PositionWithNum( $1, $2) }
| PCOMP     { PositionWithoutNum( $1 ) }
| PCOMP NUM { PositionWithNum( $1, $2) }
| SREPL     { PositionWithoutNum( $1 ) }
| SREPL NUM { PositionWithNum( $1, $2) }
| PREPL     { PositionWithoutNum( $1 ) }
| PREPL NUM { PositionWithNum( $1, $2) }
; 
 boxnames:
| SMALLER BOXNAME GREATER { $2 }
| BOXNAME  { $1 }
