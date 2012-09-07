
exception Node_Mismatch
exception Compilation_Error

let trav_error s = 
  let _ = Printf.printf "\n%s\n" s
in raise Compilation_Error

(* - AST - *)

type boxnameorid = BoxName of string (* Box Name *) 
                 | ID      of int64  (* ID *)

type sn_ast = ExecTime of string (* Execution Option *)
                        * boxnameorid (*Box Name or ID*)
                        * sn_ast
            | ListTime of string (* List Time Option *)
                        * boxnameorid (*Box Name or ID*)
                        * sn_ast
            | Stream   of int64 (* Stream Id *)
                        * sn_ast        
            | Save     of string (* Filename *)
                        * sn_ast
            | Empty


(* Function for see all the ways*)
let read_arguments node listexec listtime liststream filenamesave = 
  let rec codegen_ ind node listexec listtime liststream filenamesave =
    match node with
      | ExecTime(option,boxname, _) -> 
              let item = 
                match boxname with
                  | BoxName (name) -> (option ^ "_" ^ (name) )
                  | ID ( id )-> (option^(Int64.to_string id))  
              in (codegen_ (ind+1) node (item::listexec) listtime liststream filenamesave)
      | ListTime  ( option, boxname, _ ) -> 
              let item = 
                match boxname with
                  | BoxName (name) -> (option ^ "_" ^ (name) )
                  | ID ( id )-> ( option  ^ "_" ^ (Int64.to_string id))  
                in (codegen_ (ind+1) node listexec (item::listtime) liststream filenamesave)
      | Stream (streamid, _) -> codegen_ (ind+1) node listexec listtime (streamid::liststream) filenamesave
      | Save (filename, _) -> codegen_ (ind+1) node listexec listtime liststream filename
      | Empty       _ -> "empty"
  in codegen_ 0 node  





