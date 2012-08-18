%{
open MapAst
%}

%token  SMALLER
        GREATER   
        COLON     
        HASH
        EOF

%token <int>   NUM

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
