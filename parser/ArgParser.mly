%{
open ArgAst
%}

%token  EXEC
		LISTTIME
		STREAM
		SAVE
		HELP
		EOF

%token <int64>   NUM


%token <string> BOXNAME 
                FILENAME
                ENDED
                BLOCK
                READ
                WRITE
%token WS
%token NL

%start src
%type <ArgAst.sn_ast> src

%%

src: 
| EXEC execoption boxnameorid src         { ExecTime ( $2, $3, $4 ) }
| LISTTIME listtimeoption boxnameorid src { ListTime ( $2, $3, $4 ) }
| STREAM NUM src                          { Stream ( $2, $3 ) }
| SAVE FILENAME src                       { Save ( $2, $3 )}
| EOF {Empty}
;

execoption:
| ENDED { $1 }
| BLOCK { $1 }
;
listtimeoption:
| READ  { $1 }
| WRITE { $1 }
;

boxnameorid:
| BOXNAME  { BoxName ( $1 ) }
| NUM      { ID ( $1 ) }
;


