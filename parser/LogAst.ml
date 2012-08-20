
exception Node_Mismatch
exception Compilation_Error

let trav_error s = 
  let _ = Printf.printf "\n%s\n" s
in raise Compilation_Error

(* - AST - *)

type mess_entry = MessEntry of int (* TimeStamp *)
                           * char (* IO *)
                           * int (* Message ID  -> Node ID  *)
                           * int (* Message ID  -> Local ID *)

type list_tag_value = TagValue of int (* Tag Value *)

type more_information = MoreInformation of int (* Tag Id *)
                      * list_tag_value list (* List of simbolic name*)

type mess_trace  = MessTrace of mess_entry (* Message Entry *) 
                 * more_information (* Mori Information *)
                 | Empty

type stream_entry = StreamEntry of int (* Stream ID *)
                                 * char (* Mode *)
                                 * char (* State *)
                                 * int (* Items *)
                                 * char (* Firrst Flags *)
                                 * char (* Second Flags *)
                                 * char (* Third  Flags *)

type stream_trace = StreamTrace of stream_entry (* Stream Entry *)
                  | ST_MessTrace of mess_trace (*Message Trace *)
                  | Empty


type sn_ast = WorkStarted of int (* TimeStamp *)
                           * sn_ast
            | WorkWaited  of int (* TimeStamp *)
                           * int (* Waiting Stamp *)
                           * sn_ast
            | WorkEnded   of int (* TimeStamp *)
                           * sn_ast
            | TaskBlocked of int (* TimeStamp *)
                           * char (* Blocked By *)
                           * int (* Task ID *)
                           * int (*Execution Time*)
                           * stream_trace list(* Stream Trace *)
                           * sn_ast             
            | TaskEnded   of int (* TimeStamp *)
                           * int (* Task ID *)
                           * int (* Execution Time *)
                           * int (* Create Time *)
                           * stream_trace list(* Stream Trace *)
                           * sn_ast
            | Information of int (* Waiting count *)
                           * int (* Total Waiting Time *)
                           * sn_ast
            | Empty


(* Worker Started Parameters*)
let workerstarted_time = 
  function WorkStarted (time_stamp, _) -> time_stamp   | _ -> raise Node_Mismatch
let workerstarted_succs =
  function (WorkStarted (_, succs)) -> succs | _ -> raise Node_Mismatch

(* Worker Waited Parameters*)
let workerwaited_time = 
  function WorkWaited (time_stamp, _, _) -> time_stamp   | _ -> raise Node_Mismatch
let workerwaited_wait = 
  function WorkWaited (_, wait_stamp, _) -> wait_stamp   | _ -> raise Node_Mismatch
let workerwaited_succs =
  function WorkWaited (_, _, succs) -> succs | _ -> raise Node_Mismatch


(* Worker Ended Parameters*)
let workernded_time = 
  function WorkEnded (time_stamp, _) -> time_stamp   | _ -> raise Node_Mismatch
let workernded_succs =
  function WorkEnded (_, succs) -> succs | _ -> raise Node_Mismatch

(* Task Blocked Parameters*)
let taskblocked_node ?(sttrace=[]) tims_stamp blocked_by task_id exec_time sn_type= TaskBlocked (tims_stamp, blocked_by, task_id, exec_time, sttrace, sn_type)
let taskblocked_time = 
  function TaskBlocked (time_stamp, _, _, _, _, _) -> time_stamp   | _ -> raise Node_Mismatch
let taskblocked_blockedby = 
  function TaskBlocked (_, blocked_by, _, _, _, _) -> blocked_by | _ -> raise Node_Mismatch
let taskblocked_taskid =
  function TaskBlocked (_, _, taskid , _, _, _) -> taskid  | _ -> raise Node_Mismatch
let taskblocked_exectime =
  function TaskBlocked (_, _, _, exec_time, _, _) -> exec_time  | _ -> raise Node_Mismatch
let taskblocked_stlist =
  function TaskBlocked (_, _, _, _, stlist, _) -> stlist | _ -> raise Node_Mismatch
let taskblocked_succs =
  function TaskBlocked (_, _, _, _, _, succs) -> succs | _ -> raise Node_Mismatch

(* Task Ended Parameters*)
let taskended_node ?(sttrace=[]) tims_stamp task_id exec_time creat_time sn_type = TaskEnded (tims_stamp, task_id, exec_time, creat_time, sttrace, sn_type)
let taskended_time = 
  function TaskEnded (tims_stamp, _, _, _, _, _) -> tims_stamp   | _ -> raise Node_Mismatch
let taskended_taskid = 
  function TaskEnded (_, taskid, _, _, _, _) -> taskid | _ -> raise Node_Mismatch
let taskended_exectime =
  function TaskEnded (_, _, exec_time , _, _, _) -> exec_time  | _ -> raise Node_Mismatch
let taskended_creattime=
  function TaskEnded (_, _, _, creat_time, _, _) -> creat_time  | _ -> raise Node_Mismatch
let taskended_stlist =
  function TaskEnded (_, _, _, _, stlist, _) -> stlist | _ -> raise Node_Mismatch
let taskended_succs =
  function TaskEnded (_, _, _, _, _, succs) -> succs | _ -> raise Node_Mismatch

(* Information Parameters*)
let information_waittime =
  function Information (wait_count, _, _) -> wait_count | _ -> raise Node_Mismatch
let information_totalwaittime =
  function Information (_, total_wait_time, _) -> total_wait_time | _ -> raise Node_Mismatch
let information_succs =
  function Information (_, _, succs) -> succs | _ -> raise Node_Mismatch

(* Stream Entry Parameters *)
let streamentry_streanid =
  function StreamEntry (stream_id, _, _, _, _, _, _) -> stream_id | _ -> raise Node_Mismatch
let streamentry_mode =
  function StreamEntry (_, mode, _, _, _, _, _) -> mode | _ -> raise Node_Mismatch
let streamentry_state =
  function StreamEntry (_, _, state, _, _, _, _) -> state | _ -> raise Node_Mismatch
let streamentry_items =
  function StreamEntry (_, _, _, items, _, _, _) -> items | _ -> raise Node_Mismatch
let streamentry_firstflag =
  function StreamEntry (_, _, _, _, firstflag, _, _) -> firstflag | _ -> raise Node_Mismatch
let streamentry_secondflag =
  function StreamEntry (_, _, _, _, _, secondflag, _) -> secondflag | _ -> raise Node_Mismatch
let streamentry_thirdflag =
function StreamEntry (_, _, _, _, _, _, thirdflag) -> thirdflag | _ -> raise Node_Mismatch


(* Function with all the nodes *)
let node_succs node = match node with
  | WorkStarted _ -> workerstarted_succs node 
  | WorkWaited  _ -> workerwaited_succs node
  | WorkEnded   _ -> workernded_succs node
  | TaskBlocked _ -> taskblocked_succs node             
  | TaskEnded   _ -> taskended_succs node
  | Information _ -> information_succs node
  | _             -> Empty

(* Function for see all the ways*)
let codegen node = 
  let rec codegen_ ind node =
    match node with
      | WorkStarted _ -> codegen_ (ind+1) (node_succs node); "workerStarted" 
      | WorkWaited  _ -> codegen_ (ind+1) (node_succs node); "workerWaited" 
      | WorkEnded   _ -> codegen_ (ind+1) (node_succs node); "workerWaited"
      | TaskBlocked _ -> codegen_ (ind+1) (node_succs node); "TaskBlocked"              
      | TaskEnded   _ -> codegen_ (ind+1) (node_succs node); "TaskEndend" 
      | Information _ -> codegen_ (ind+1) (node_succs node); "Information" 
      | Empty       _ -> "empty"
  in codegen_ 0 node

(* This function needs the Task ID and it returns the execution time of endend Task*)
let t_exectime node taskidsearch = 
  let rec codegen_ ind node =
    match node with
      | TaskEnded (tims_stamp, task_id, exec_time, creation_time, _, _) ->  
          if (task_id == taskidsearch) then
            (
              Printf.printf "Ended Task id: %d with real execution time: %d  \n" taskidsearch (tims_stamp-creation_time);
              Printf.printf "Diference between the real execution time and exectuion time: %d - %d = %d  \n" (tims_stamp-creation_time) exec_time ((tims_stamp-creation_time)-exec_time);
              true
            )
            else
            (
              codegen_ (ind+1) (node_succs node);            )
      | Empty       _ -> false 
      | _       ->  codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node

(* This function needs the Task ID and it returns the execution time of blocked Task*)
let t_blocktime node taskidsearch =   
  let rec codegen_ ind node =
    match node with
      | TaskBlocked (tims_stamp, _, task_id, exec_time, _, _) ->  
          if (task_id == taskidsearch) then
            (
              Printf.printf "Blocked Task id: %d with real execution time in the moment when the task is blocked is: %d  \n" taskidsearch (tims_stamp-exec_time); 
              codegen_ (ind+1) (node_succs node); true
            )
          else 
            codegen_ (ind+1) (node_succs node)              
      | Empty       _ -> Printf.eprintf "\n"; false 
      | _       ->  codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node


(* Convert the state's character to string*)
let state_to_str state =
  match state with
    | 'O' -> "Open"
    | 'C' -> "Closed"
    | 'I' -> "In used"
    | _   -> ""

(* Convert the mode's character to string*)
let mode_to_str state =
  match state with
    | 'r' -> "Read"
    | 'w' -> "Write"
    | _   -> ""


(* Read the information about stream of the read's packets *)
let list_of_read_packets l = 
  match l with
    | StreamTrace (streamentry) -> 
        if ( (streamentry_mode streamentry) == 'r') then
          Printf.printf " StreamID: %d - State: %s - items: %d\n" (streamentry_streanid streamentry) 
          (state_to_str(streamentry_state streamentry)) (streamentry_items streamentry)
        else ()
    | ST_MessTrace _ -> ()
    | _ -> ()

(* Read the information about stream of the write's packets *)
let list_of_write_packets l = 
  match l with
    | StreamTrace (streamentry) -> 
        if ( (streamentry_mode streamentry) == 'w') then
          Printf.printf " StreamID: %d - State: %s - items: %d\n" (streamentry_streanid streamentry) 
          (state_to_str(streamentry_state streamentry)) (streamentry_items streamentry)
        else ()
    | ST_MessTrace _ -> ()
    | _ -> ()



(* Read the information about stream of the write's packets *)
let list_of_specified_packets l idsearch= 
  match l with
    | StreamTrace (streamentry) -> 
        if ( (streamentry_streanid streamentry) == idsearch) then
        (
          "\n Mode: " ^ (mode_to_str (streamentry_mode streamentry)) ^ " - State: " ^ (state_to_str(streamentry_state streamentry)) ^ 
          " - items: " ^ (string_of_int (streamentry_items streamentry)) ^" - Flags: " ^ Char.escaped (streamentry_firstflag streamentry)
          ^ Char.escaped (streamentry_secondflag streamentry) ^ Char.escaped (streamentry_thirdflag streamentry) ^ "\n"  
        )
        else ("")
| ST_MessTrace _ -> ("")
| _ -> ("")




(* This function needs the Task ID and it returns the list of time when the task read in the stream *)
let t_list_rec_read node taskidsearch = 
  let rec codegen_ ind node =
    match node with
      | TaskBlocked (tims_stamp, _, task_id, _, sttrace, _) ->
          if (task_id == taskidsearch) then
          (
            if ((List.length sttrace) > 0) then
            (
              Printf.printf "The task ID: %d with this time execution: %d made this reads from stream:\n" 
              taskidsearch tims_stamp;
              Printf.printf "--------------------------------------------------------------------------\n";
              List.map list_of_read_packets sttrace;
              Printf.printf "\n";
              codegen_ (ind+1) (node_succs node);
              true
            )
            else
            (
              codegen_ (ind+1) (node_succs node)
            )
          )
          else 
          (
            codegen_ (ind+1) (node_succs node)
          )  
      | TaskEnded (tims_stamp, task_id, _, _, sttrace, _) -> 
          if (task_id == taskidsearch) then
          (
            if ((List.length sttrace) > 0) then
            (
              Printf.printf "The task ID: %d with this time execution: %d made this reads from stream:\n" 
              taskidsearch tims_stamp;
              Printf.printf "--------------------------------------------------------------------------\n";
              List.map list_of_read_packets sttrace;
              Printf.printf "\n";
              codegen_ (ind+1) (node_succs node);
              true
            )
            else
            (
            codegen_ (ind+1) (node_succs node)
            )
          )
          else 
          (
            codegen_ (ind+1) (node_succs node)
          )  
      | Empty  _ -> Printf.printf "\n";false 
      | _        -> codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node



(* This function needs the Task ID and it returns the list of time when the task read in the stream *)
let t_list_rec_write node taskidsearch = 
  let rec codegen_ ind node = 
    match node with
      | TaskBlocked (tims_stamp, _, task_id, _, sttrace, _) ->
          if (task_id == taskidsearch) then
          (
            if ((List.length sttrace) > 0) then
            (
              Printf.printf "The task ID: %d with this time execution: %d made this write from stream:\n" 
              taskidsearch tims_stamp;
              Printf.printf "--------------------------------------------------------------------------\n";
              List.map list_of_write_packets sttrace;
              Printf.printf "\n";
              codegen_ (ind+1) (node_succs node);
              true
            )
            else
            (
              codegen_ (ind+1) (node_succs node)
            )
          )
          else 
          (
            codegen_ (ind+1) (node_succs node)
          )  
      | TaskEnded (tims_stamp, task_id, _, _, sttrace, _) -> 
          if (task_id == taskidsearch) then
          (
            if ((List.length sttrace) > 0) then
            (
              Printf.printf "The task ID: %d with this time execution: %d made this write from stream:\n" 
              taskidsearch tims_stamp;
              Printf.printf "--------------------------------------------------------------------------\n";
              List.map list_of_write_packets sttrace;
              Printf.printf "\n";
              codegen_ (ind+1) (node_succs node);
              true
            )
            else
            (
              codegen_ (ind+1) (node_succs node)
            )
          )
          else 
          (
            codegen_ (ind+1) (node_succs node)
          )  
      | Empty  _ -> Printf.printf "\n"; false 
      | _        -> codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node




