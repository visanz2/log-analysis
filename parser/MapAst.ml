
exception Node_Mismatch
exception Compilation_Error

let trav_error s = 
  let _ = Printf.printf "\n%s\n" s
in raise Compilation_Error

(* - AST - *)

type net_path = PositionWithoutNum of char (* letter of position (S, P, R or I) *) 
              | PositionWithNum    of char (* letter of position (S, P, R or I) *)
                                    * int  (* Number of composition *)
              | Empty


type sn_ast = MappingEntries of int          (* Task ID *)
                              * net_path list (* list of NET-PATH *)
                              * string        (* Box Name *)
                              * int           (* Worker ID *)
                              * sn_ast
            | Empty


(* Mapping Entries Parameters*)
let mappingentries_succs =
function MappingEntries (_, _, _, _, succs) -> succs   | _ -> raise Node_Mismatch

let codegen node = 
  let rec codegen_ ind node =
    match node with
      | MappingEntries _ -> let _ = codegen_ (ind+1) (mappingentries_succs node) in "workerStarted" 
      | Empty       _ -> "empty"
  in codegen_ 0 node

(* Print all the options *)
let printlistoptions listoptions = 
  Printf.printf "\nThe boxname searched exist a lot of time in the map file.\n";
  Printf.printf "Please selected one of this options:\n";
  let rec loop() = 
    for i = 0 to (List.length listoptions)-1 do
      Printf.printf "%d.- TaskID: %d \n" (i+1) (List.nth listoptions i)
    done;
    let position = read_int() in
      if (position > (List.length listoptions)) || ( position <= 0 ) then 
      (
        Printf.printf "It's not an option. Please select one good option.\n" ;
        loop()
      )
      else
      (
        Printf.printf "Selected: option: %d with taskID: %d\n" position (List.nth listoptions (position-1));
        List.nth listoptions (position-1)
      )
in   loop()


(* Search the box name, if not return null*)
let t_search_boxname node searchname listsolutions = 
  let rec codegen_ ind node listsolutions =
    match node with
      | MappingEntries (taskid, _, boxname, workerid,_) ->
                  if ( String.compare searchname boxname ) == 0 then 
                  ( 
                    codegen_ (ind+1) (mappingentries_succs node) (taskid::listsolutions); 
                   )
                  else
                  (
                    codegen_ (ind+1) (mappingentries_succs node) listsolutions; 
                  );

      | Empty       _ -> 
                  if ((List.length listsolutions) > 1) then
                  (
                     flush stdout;
                     printlistoptions listsolutions
                  )
                  else 
                  (
                    if ((List.length listsolutions) == 1) then
                    (
                      Printf.printf "1.- %d\n" (List.hd listsolutions);
                      (List.hd listsolutions)
                    )
                    else
                    (
                      Printf.printf "\nDon't exist a box name with this name.\n"; 
                      (-1)
                    )
                  )
                
  in codegen_ 0 node []




(* Return all the id possibles for one boxname, if not return null*)
let t_search_boxname_all node searchname listsolutions = 
  let rec codegen_ ind node listsolutions =
    match node with
      | MappingEntries (taskid, _, boxname, workerid,_) ->
                  if ( String.compare searchname boxname ) == 0 then 
                  ( 
                    codegen_ (ind+1) (mappingentries_succs node) (taskid::listsolutions);
                   )
                  else
                  (
                    codegen_ (ind+1) (mappingentries_succs node) listsolutions; 
                  );

      | Empty       _ -> 
                  if ((List.length listsolutions) == 0) then
                  (
                    Printf.printf "\nDon't exist a box name with this name.\n"; 
                    [(-1)]
                  )
                  else
                  (
                    listsolutions
                  )
                  
                
  in codegen_ 0 node []

