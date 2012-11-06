
exception Node_Mismatch
exception Compilation_Error

let searchingnext = ref 0
let vartimestamp = ref Big_int.zero_big_int

let trav_error s = 
  let _ = Printf.printf "\n%s\n" s
in raise Compilation_Error

(* - AST - *)

type mess_entry = MessEntry of Big_int.big_int (* TimeStamp *)
                           * char (* IO *)
                           * Big_int.big_int  (* Message ID  -> Node ID  *)
                           * Big_int.big_int  (* Message ID  -> Local ID *)

type list_tag_value = TagValue of Big_int.big_int (* Tag Value *)

type more_information = MoreInformation of Big_int.big_int (* Tag Id *)
                      * list_tag_value list (* List of simbolic name*)

type mess_trace  = MessTraceWithInfo of mess_entry (* Message Entry *) 
                 * more_information (* Mori Information *)
								 | MessTrace of mess_entry (* Message Entry *) 
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


type sn_ast = WorkStarted of Big_int.big_int (* TimeStamp *)
                           * sn_ast
            | WorkWaited  of Big_int.big_int (* TimeStamp *)
                           * Big_int.big_int (* Waiting Stamp *)
                           * sn_ast
            | WorkEnded   of Big_int.big_int (* TimeStamp *)
                           * sn_ast
            | TaskBlocked of Big_int.big_int (* TimeStamp *)
                           * char (* Blocked By *)
                           * int (* Task ID *)
                           * Big_int.big_int (*Execution Time*)
                           * stream_trace list(* Stream Trace *)
                           * sn_ast             
            | TaskEnded   of Big_int.big_int (* TimeStamp *)
                           * int (* Task ID *)
                           * Big_int.big_int (* Execution Time *)
                           * Big_int.big_int (* Create Time *)
                           * stream_trace list(* Stream Trace *)
                           * sn_ast
            | Information of int (* Waiting count *)
                           * Big_int.big_int (* Total Waiting Time *)
                           * sn_ast
            | Empty


(* Worker Started Parameters*)
let workerstarted_succs =
  function (WorkStarted (_, succs)) -> succs | _ -> raise Node_Mismatch

(* Worker Waited Parameters*)
let workerwaited_succs =
  function WorkWaited (_, _, succs) -> succs | _ -> raise Node_Mismatch

(* Worker Ended Parameters*)
let workernded_succs =
  function WorkEnded (_, succs) -> succs | _ -> raise Node_Mismatch

(* Task Blocked Parameters*)
let taskblocked_succs =
  function TaskBlocked (_, _, _, _, _, succs) -> succs | _ -> raise Node_Mismatch

(* Task Ended Parameters*)
let taskended_succs =
  function TaskEnded (_, _, _, _, _, succs) -> succs | _ -> raise Node_Mismatch

(* Information Parameters*)
let information_succs =
  function Information (_, _, succs) -> succs | _ -> raise Node_Mismatch

(* Stream Entry Parameters *)
let streamentry_streanid =
  function StreamEntry (stream_id, _, _, _, _, _, _) -> stream_id (*| _ -> raise Node_Mismatch *)
let streamentry_mode =
  function StreamEntry (_, mode, _, _, _, _, _) -> mode (*| _ -> raise Node_Mismatch *)
let streamentry_state =
  function StreamEntry (_, _, state, _, _, _, _) -> state (*| _ -> raise Node_Mismatch *)
let streamentry_items =
  function StreamEntry (_, _, _, items, _, _, _) -> items (*| _ -> raise Node_Mismatch *)
let streamentry_firstflag =
  function StreamEntry (_, _, _, _, firstflag, _, _) -> firstflag (*| _ -> raise Node_Mismatch *)
let streamentry_secondflag =
  function StreamEntry (_, _, _, _, _, secondflag, _) -> secondflag (*| _ -> raise Node_Mismatch *)
let streamentry_thirdflag =
	function StreamEntry (_, _, _, _, _, _, thirdflag) -> thirdflag (*| _ -> raise Node_Mismatch *)


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
      | WorkStarted _ -> codegen_ (ind+1) (node_succs node) 
      | WorkWaited  _ -> codegen_ (ind+1) (node_succs node) 
      | WorkEnded   _ -> codegen_ (ind+1) (node_succs node)
      | TaskBlocked _ -> codegen_ (ind+1) (node_succs node)              
      | TaskEnded   _ -> codegen_ (ind+1) (node_succs node) 
      | Information _ -> codegen_ (ind+1) (node_succs node) 
      | Empty       _ -> "empty"
  in codegen_ 0 node

(* This function needs the Task ID and it returns the execution time of endend Task*)
let t_exectime node taskidsearch = 
  let rec codegen_ ind node =
    match node with
      | TaskEnded (tims_stamp, task_id, exec_time, creation_time, _, _) ->  
            if (task_id == taskidsearch) then
            (
							codegen_ (ind+1) (node_succs node)  ^ "EndedTaskID: " ^ (string_of_int taskidsearch) 
							^ "; RealExecTime: " ^ (Big_int.string_of_big_int(Big_int.sub_big_int tims_stamp creation_time))
              ^ "; RealExecTime-Exectime: " ^ (Big_int.string_of_big_int((Big_int.sub_big_int (Big_int.sub_big_int tims_stamp creation_time) exec_time))) ^ ";\n\n" 
            )
            else
            (
              codegen_ (ind+1) (node_succs node);            )
      | Empty       _ -> "" 
      | _       ->  codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node

(* This function needs the Task ID and it returns the execution time of blocked Task*)
let t_blocktime node taskidsearch =   
  let rec codegen_ ind node =
    match node with
      | TaskBlocked (tims_stamp, _, task_id, exec_time, _, _) ->  
          	if (task_id == taskidsearch) then
            (
                codegen_ (ind+1) (node_succs node) 
                ^ "BlockedTaskID: " ^ (string_of_int taskidsearch) ^ "; RealExecTime: " 
                ^ (Big_int.string_of_big_int((Big_int.sub_big_int tims_stamp exec_time))) ^ ";\n\n"
            )
          	else 
            	codegen_ (ind+1) (node_succs node)              
      | Empty       _ -> "" 
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
          " StreamID: " ^ (string_of_int (streamentry_streanid streamentry)) ^ "; State: " 
          ^ (state_to_str(streamentry_state streamentry)) ^ "; items: " ^ (string_of_int (streamentry_items streamentry)) ^ "\n\n"  
        else ("")
    | _ -> ("")

(* Read the information about stream of the write's packets *)
let list_of_write_packets l = 
  match l with
    | StreamTrace (streamentry) -> 
        if ( (streamentry_mode streamentry) == 'w') then
          " StreamID: " ^ (string_of_int (streamentry_streanid streamentry)) ^ "; State: " 
          ^ (state_to_str(streamentry_state streamentry)) ^ "; items: "
          ^ (string_of_int (streamentry_items streamentry)) ^ "\n\n" 
        else ("")
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
                codegen_ (ind+1) (node_succs node)
                ^ "TaskID: " ^ (string_of_int taskidsearch) ^ "; TimeExec: " 
                ^ (Big_int.string_of_big_int tims_stamp) ^ "; ->:\n" 
                ^ String.concat "" (List.map list_of_read_packets sttrace)   
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
                codegen_ (ind+1) (node_succs node)
                ^ "TaskID: " ^ (string_of_int taskidsearch) ^ "; TimeExec: " 
                ^ (Big_int.string_of_big_int tims_stamp) ^ "; ->:\n" 
                ^ String.concat "" (List.map list_of_read_packets sttrace)   
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
      | Empty  _ -> ""
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
              let _ = codegen_ (ind+1) (node_succs node) in
                "TaskID: " ^ (string_of_int taskidsearch) ^ "; TimeExec: " 
                ^ (Big_int.string_of_big_int tims_stamp) ^ "; ->:\n" 
                ^ String.concat "" (List.map list_of_write_packets sttrace)             
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
              let _ = codegen_ (ind+1) (node_succs node) in
                "TaskID: " ^ (string_of_int taskidsearch) ^ "; TimeExec: " 
                ^ (Big_int.string_of_big_int tims_stamp) ^ "; ->:\n" 
                ^ String.concat "" (List.map list_of_write_packets sttrace)   
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
      | Empty  _ -> ""
      | _        -> codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node



(* This function needs the Stream ID and it returns the stream trace if it is exist *)
let t_search_stream node streamidsearch workerid = 
  let rec codegen_ ind node =
    match node with
      | TaskBlocked (tims_stamp, _, task_id, _, sttrace, _) ->          
              String.concat "" (List.map  (* Read the information about stream of the write's packets *)
                                  (
                                  function l -> 
                                      match l with
                                          | StreamTrace (streamentry) -> 
                                              if ( ((streamentry_streanid streamentry) == streamidsearch) ) then
                                              (
                                                "WorkerID: " ^ (string_of_int workerid) ^ "; Mode: " ^ (mode_to_str (streamentry_mode streamentry)) ^ "; State: "
                                                ^ (state_to_str(streamentry_state streamentry)) ^ "; Items: " ^ (string_of_int (streamentry_items streamentry)) 
                                                ^ "; Flags: " ^(Char.escaped (streamentry_firstflag streamentry)) ^ (Char.escaped (streamentry_secondflag streamentry))
                                                ^ (Char.escaped (streamentry_thirdflag streamentry)) ^ "\n\n"  
                                              )
                                              else ("")
                                          | _ -> ("")
                                  )
                              sttrace ) ^ codegen_ (ind+1) (node_succs node)
      | TaskEnded (tims_stamp, task_id, _, _, sttrace, _) -> 
              String.concat "" (List.map  (* Read the information about stream of the write's packets *)
                                  (
                                  function l ->
                                      match l with
                                          | StreamTrace (streamentry) -> 
                                              if ( ((streamentry_streanid streamentry) == streamidsearch) ) then
                                              (
                                                "WorkerID: " ^ (string_of_int workerid) ^ "; Mode: " ^ (mode_to_str (streamentry_mode streamentry)) ^ "; State:"
                                                ^ (state_to_str(streamentry_state streamentry)) ^ "; Items:" ^ (string_of_int (streamentry_items streamentry)) 
                                                ^ "; Flags: " ^(Char.escaped (streamentry_firstflag streamentry)) ^ (Char.escaped (streamentry_secondflag streamentry))
                                                ^ (Char.escaped (streamentry_thirdflag streamentry)) ^ "\n\n"  
                                              )
                                              else ("")
                                          | _ -> ("")
                                  )
                              sttrace ) ^ codegen_ (ind+1) (node_succs node)
      | Empty  _ -> ("")   
      | _        -> codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node



let t_vector_stream_write node workerid = 
  let rec codegen_ ind node liststream = 
    match node with
      | TaskBlocked (_, _, _, _, sttrace, _) ->     
					let rec loop var =
						if (var < (List.length sttrace)) then
        		(
              	(
									match (List.nth sttrace var) with
                  | StreamTrace (streamentry) -> 
    								if ((streamentry_mode streamentry) == 'w') then
                      	(
													liststream @ [( (streamentry_streanid streamentry), workerid)]
												)
    								else(liststream)
                  | _ -> (liststream)
								) @ loop (var+1)
						)
						else
						(
								codegen_ (ind+1) (node_succs node) liststream
						)
						in loop 0
		  | TaskEnded (_, _, _, _, sttrace, _) ->  
					let rec loop var =
						if (var < (List.length sttrace)) then
        		(
              	(
									match (List.nth sttrace var) with
                  | StreamTrace (streamentry) -> 
    								if ((streamentry_mode streamentry) == 'w') then
                      	(
													
													liststream @ [( (streamentry_streanid streamentry), workerid)]
												)
    								else(liststream)
                  | _ -> (liststream)
								) @ loop (var+1)
						)
						else
						(
								codegen_ (ind+1) (node_succs node) liststream
						)
						in loop 0  
      | Empty  _ -> (liststream)   
      | _        -> codegen_ (ind+1) (node_succs node) liststream
  in codegen_ 0 node []
	

	
let t_vector_stream_read node workerid = 
  let rec codegen_ ind node liststream = 
    match node with
      | TaskBlocked (_, _, _, _, sttrace, _) ->     
					let rec loop var =
						if (var < (List.length sttrace)) then
        		(
              	(
									match (List.nth sttrace var) with
                  | StreamTrace (streamentry) -> 
    								if ((streamentry_mode streamentry) == 'r') then
                      	(
													liststream @ [( (streamentry_streanid streamentry), workerid)]
												)
    								else(liststream)
                  | _ -> (liststream)
								) @ loop (var+1)
						)
						else
						(
								codegen_ (ind+1) (node_succs node) liststream
						)
						in loop 0
		  | TaskEnded (_, _, _, _, sttrace, _) ->  
					let rec loop var =
						if (var < (List.length sttrace)) then
        		(
              	(
									match (List.nth sttrace var) with
                  | StreamTrace (streamentry) -> 
    								if ((streamentry_mode streamentry) == 'r') then
                      	(
													liststream @ [( (streamentry_streanid streamentry), workerid)]
												)
    								else(liststream)
                  | _ -> (liststream)
								) @ loop (var+1)
						)
						else
						(
								codegen_ (ind+1) (node_succs node) liststream
						)
						in loop 0  
      | Empty  _ -> (liststream)   
      | _        -> codegen_ (ind+1) (node_succs node) liststream
  in codegen_ 0 node []
		
	
(* Return the last number of stream *)
let t_last_number_stream node = 
  let rec codegen_ ind node  = 
    match node with
      | TaskBlocked (_, _, _, _, sttrace, _) ->          
            (List.map  (* Read the information about stream of the write's packets *)
                                  (
                                  function l ->
                                      match l with
                                          | StreamTrace (streamentry) -> 
                                              	streamentry_streanid streamentry
                                          | _ -> (0)
                                  )
                              sttrace 
											 )  @ codegen_ (ind+1) (node_succs node);
       | TaskEnded (_, _, _, _, sttrace, _) ->    
            (List.map  (* Read the information about stream of the write's packets *)
                                  (
                                  function l ->
                                      match l with
                                          | StreamTrace (streamentry) -> 
                                                streamentry_streanid streamentry
                                          | _ -> (0)
                                  )
                              sttrace ) @ codegen_ (ind+1) (node_succs node);
      | Empty  _ -> ([])   
      | _        -> codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node


	
(* This function needs the Task ID and it returns the execution time of blocked Task*)
let t_create_graph node taskidsearch workerid =   
  let rec codegen_ ind node =
    match node with
			| TaskEnded (tims_stamp, task_id, exec_time, creation_time, _, _) ->  
            if (task_id == taskidsearch) then
            (
								[( (Big_int.add_big_int creation_time exec_time), task_id, workerid)]; 
            )
            else
            (
              codegen_ (ind+1) (node_succs node);
            )        
      | Empty       _ -> [] 
      | _       ->  codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node


(* Function look up, necessary task id *)
let t_look_up_message node taskidsearch = 
  let rec codegen_ ind node = 
    match node with
      | TaskBlocked (_, _, task_id, _, sttrace, _) -> 
    				if (task_id == taskidsearch) then
    				(
                (List.iter  (* Read the information about stream of the write's packets *)
                                  (
                                  function l -> 
                                      match l with
																					| ST_MessTrace (mess_trace) ->
																							(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												Printf.printf "TimeStamp: %s; io: %c; MsgID: %s.%s\n"  (Big_int.string_of_big_int timestamp) io  
																												(Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid);
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												Printf.printf "TimeStamp: %s; io: %c; MsgID: %s.%s\n"  (Big_int.string_of_big_int timestamp) io 
																												 (Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid);
																										)
																									| _ -> ()
																							)
                                      		| _ -> ()
                                  )
                              sttrace ); codegen_ (ind+1) (node_succs node);
						)
						else
						(
							codegen_ (ind+1) (node_succs node);
						)
				| TaskEnded (_, task_id, _, _, sttrace, _) ->  
    				if ( task_id == taskidsearch) then
    				(
                (List.iter  (* Read the information about stream of the write's packets *)
                                  (
                                  function l -> 
                                      match l with
																					| ST_MessTrace (mess_trace) ->
																							(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												Printf.printf "TimeStamp: %s; io: %c; MsgID: %s.%s\n"  (Big_int.string_of_big_int timestamp) io  
																												(Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid);
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												Printf.printf "TimeStamp: %s; io: %c; MsgID: %s.%s\n"  (Big_int.string_of_big_int timestamp) io 
																												 (Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid);
																										)
																									| _ -> ()
																							)
                                      		| _ -> ()
                                  )
                              sttrace ); codegen_ (ind+1) (node_succs node);
						)
						else
						(
							codegen_ (ind+1) (node_succs node);
						)
      | Empty  _ -> ()   
      | _        -> codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node
														
																					

(* Function latency, necessary messageId and taskId *)
let t_latency_info node nodeidsearch localidsearch = 
  let rec codegen_ ind node = 
    match node with
					| TaskBlocked (_, _, task_id, _, sttrace, _) -> 
										let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												(
													match (List.nth sttrace var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												(
																													if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 ) then
																														(timestamp)
																													else (loop (var+1))
																												)
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												(
																													if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 ) then
																														(timestamp)
																													else (loop (var+1))	
																												)
																										)
																									| _ -> (loop (var+1))
																							)
													 | _ -> (loop (var+1))
												)
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node)
											)
                 		in loop 0
			| TaskEnded (_, task_id, _, _, sttrace, _) ->  
										let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												(
													match (List.nth sttrace var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												(
																													if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 ) then
																														(timestamp)
																													else (loop (var+1))
																												)
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												(
																													if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 ) then
																														(timestamp)
																													else (loop (var+1))	
																												)
																										)
																									| _ -> (loop (var+1))
																							)
													 | _ -> (loop (var+1))
												)
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node)
											)
                 		in loop 0
	    | Empty  _ -> (Big_int.zero_big_int)   
      | _        -> codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node
	

(* Function latency, necessary messageId and taskId *)
let t_latency_next node nodeidsearch localidsearch = 
  let rec codegen_ ind node listinfo = 
    match node with
		  | TaskBlocked (_, _, task_id, _, sttrace, _) ->
				 					let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												 (
													match (List.nth sttrace var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'I') then
																													(
																														searchingnext := 1;
																														loop (var+1)
																													)	
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='O') then
																													(
																														listinfo @ [(task_id, timestamp, nodeid, localid)] @ loop (var+1) 	
																													)
																													else 
																													(
																														searchingnext:=0;
																													  listinfo
																													)
																												)	
																																
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'I') then
																													(
																														searchingnext := 1;	
																														loop (var+1)
																													)
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='O') then
																													(
																														listinfo @ [(task_id, timestamp, nodeid, localid)] @ loop (var+1) 	
																													)
																													else
																													(
																														searchingnext := 0;
																														listinfo
																													)
																												)	
																										)
																									| _ -> (listinfo)
																							)
													| _ -> (loop (var+1))
												)   
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node) listinfo
											)
                 		in loop 0
			| TaskEnded (_, task_id, _, _, sttrace, _) ->  
										let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												 (
													match (List.nth sttrace var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'I') then
																													(
																														searchingnext := 1;
																														loop (var+1)
																													)	
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='O') then
																													(
																														listinfo @ [(task_id, timestamp, nodeid, localid)] @ loop (var+1) 	
																													)
																													else 
																													(
																														searchingnext:=0;
																													  listinfo
																													)
																												)	
																																
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'I') then
																													(
																														searchingnext := 1;	
																														loop (var+1)
																													)
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='O') then
																													(
																														listinfo @ [(task_id, timestamp, nodeid, localid)] @ loop (var+1) 	
																													)
																													else
																													(
																														searchingnext := 0;
																														listinfo
																													)
																												)	
																										)
																									| _ -> (listinfo)
																							)
													| _ -> (loop (var+1))
												)   
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node) listinfo
											)
                 		in loop 0
	    | Empty  _ -> (listinfo)   
      | _        -> codegen_ (ind+1) (node_succs node) listinfo
  in codegen_ 0 node []
	
	

(* Function latency, necessary messageId and taskId *)
let t_latency_before node nodeidsearch localidsearch = 
  let rec codegen_ ind node listinfo = 
    match node with
		  | TaskBlocked (_, _, task_id, _, sttrace, _) ->
				 					let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												 (
													match (List.nth (List.rev sttrace) var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'O') then
																													(
																														searchingnext := 1;
																														loop (var+1)
																													)	
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='I') then
																													(
																														searchingnext:=0;
																														listinfo @ [(task_id, timestamp, nodeid, localid)] 	
																													)
																													else 
																													(
																													  loop (var+1) 
																													)
																												)	
																																
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'O') then
																													(
																														searchingnext := 1;
																														loop (var+1)
																													)
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='I') then
																													(
																														searchingnext:=0;
																														listinfo @ [(task_id, timestamp, nodeid, localid)] 	
																													)
																													else 
																													(
																													  loop (var+1) 
																													)
																												)	
																										)
																									| _ -> (listinfo)
																							)
													| _ -> (loop (var+1))
												)   
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node) listinfo
											)
                 		in loop 0
			| TaskEnded (_, task_id, _, _, sttrace, _) ->  
										let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												 (
													match (List.nth (List.rev sttrace) var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'O') then
																													(
																														searchingnext := 1;
																														loop (var+1)
																													)	
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='I') then
																													(
																														searchingnext:=0;
																														listinfo @ [(task_id, timestamp, nodeid, localid)] 	
																													)
																													else 
																													(
																													  loop (var+1) 
																													)
																												)	
																																
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'O') then
																													(
																														searchingnext := 1;	
																														loop (var+1)
																													)
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='I') then
																													(
																														searchingnext:=0;
																														listinfo @ [(task_id, timestamp, nodeid, localid)] 	
																													)
																													else 
																													(
																													  loop (var+1) 
																													)
																												)	
																										)
																									| _ -> (listinfo)
																							)
													| _ -> (loop (var+1))
												)   
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node) listinfo
											)
                 		in loop 0
	    | Empty  _ -> (listinfo)   
      | _        -> codegen_ (ind+1) (node_succs node) listinfo
  in codegen_ 0 node []
	
	
 
(* Function latency, necessary messageId and taskId *)
let t_latency_trace node nodeidsearch localidsearch = 
  let rec codegen_ ind node listinfo = 
    match node with
		  | TaskBlocked (_, _, task_id, _, sttrace, _) ->
				 					let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												 (
													match (List.nth sttrace var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'I') then
																													(
																														searchingnext := 1;
																														vartimestamp := timestamp;
																														loop (var+1)
																													)	
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='O') then
																													(
																														Printf.printf "%s.%s: %s -> %s.%s: %s\n" (Big_int.string_of_big_int nodeidsearch) (Big_int.string_of_big_int localidsearch)
                                														(Big_int.string_of_big_int !vartimestamp) (Big_int.string_of_big_int nodeid) 
                                														(Big_int.string_of_big_int localid) (Big_int.string_of_big_int timestamp);
																														
																														listinfo @ [(task_id, timestamp, nodeid, localid)] @ loop (var+1) 	
																													)
																													else 
																													(
																														searchingnext := 0;
																														vartimestamp := Big_int.zero_big_int;
																													  listinfo
																													)
																												)	
																																
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'I') then
																													(
																														searchingnext := 1;	
																														vartimestamp := timestamp;
																														loop (var+1)
																													)
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='O') then
																													(
																														Printf.printf "%s.%s: %s -> %s.%s: %s\n" (Big_int.string_of_big_int nodeidsearch) (Big_int.string_of_big_int localidsearch)
                                														(Big_int.string_of_big_int !vartimestamp) (Big_int.string_of_big_int nodeid) 
                                														(Big_int.string_of_big_int localid) (Big_int.string_of_big_int timestamp);
																														
																														listinfo @ [(task_id, timestamp, nodeid, localid)] @ loop (var+1) 	
																													)
																													else
																													(
																														searchingnext := 0;
																														vartimestamp := Big_int.zero_big_int;
																														listinfo
																													)
																												)	
																										)
																									| _ -> (listinfo)
																							)
													| _ -> (loop (var+1))
												)   
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node) listinfo
											)
                 		in loop 0
			| TaskEnded (_, task_id, _, _, sttrace, _) ->  
										let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												 (
													match (List.nth sttrace var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'I') then
																													(
																														searchingnext := 1;
																														vartimestamp := timestamp;
																														loop (var+1)
																													)	
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='O') then
																													(
																														Printf.printf "%s.%s: %s -> %s.%s: %s\n" (Big_int.string_of_big_int nodeidsearch) (Big_int.string_of_big_int localidsearch)
                                														(Big_int.string_of_big_int !vartimestamp) (Big_int.string_of_big_int nodeid) 
                                														(Big_int.string_of_big_int localid) (Big_int.string_of_big_int timestamp);
																														
																														listinfo @ [(task_id, timestamp, nodeid, localid)] @ loop (var+1) 	
																													)
																													else 
																													(
																														searchingnext := 0;
																														vartimestamp := Big_int.zero_big_int;
																													  listinfo
																													)
																												)	
																																
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'I') then
																													(
																														vartimestamp := timestamp;
																														searchingnext := 1;	
																														loop (var+1)
																													)
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='O') then
																													(
																														Printf.printf "%s.%s: %s -> %s.%s: %s\n" (Big_int.string_of_big_int nodeidsearch) (Big_int.string_of_big_int localidsearch)
                                														(Big_int.string_of_big_int !vartimestamp) (Big_int.string_of_big_int nodeid) 
                                														(Big_int.string_of_big_int localid) (Big_int.string_of_big_int timestamp);
																														
																														listinfo @ [(task_id, timestamp, nodeid, localid)] @ loop (var+1) 	
																													)
																													else
																													(
																														searchingnext := 0;
																														vartimestamp := Big_int.zero_big_int;
																														listinfo
																													)
																												)	
																										)
																									| _ -> (listinfo)
																							)
													| _ -> (loop (var+1))
												)   
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node) listinfo
											)
                 		in loop 0
	    | Empty  _ -> (listinfo)   
      | _        -> codegen_ (ind+1) (node_succs node) listinfo
  in codegen_ 0 node []
          

(* Function latency, necessary messageId and taskId *)
let t_latency_rtrace node nodeidsearch localidsearch = 
  let rec codegen_ ind node listinfo = 
    match node with
		  | TaskBlocked (_, _, task_id, _, sttrace, _) ->
				 					let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												 (
													match (List.nth (List.rev sttrace) var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'O') then
																													(
																														searchingnext := 1;
																														vartimestamp := timestamp;
																														loop (var+1)
																													)	
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='I') then
																													(
																														searchingnext:=0;
																														Printf.printf "%s.%s: %s -> %s.%s: %s\n" (Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid) 
                                														(Big_int.string_of_big_int timestamp) (Big_int.string_of_big_int nodeidsearch) 
                                														(Big_int.string_of_big_int localidsearch) (Big_int.string_of_big_int !vartimestamp);
																														
																														listinfo @ [(task_id, timestamp, nodeid, localid)] 	
																													)
																													else 
																													(
																													  loop (var+1) 
																													)
																												)	
																																
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'O') then
																													(
																														searchingnext := 1;
																														vartimestamp := timestamp;	
																														loop (var+1)
																													)
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='I') then
																													(
																														searchingnext:=0;
																														Printf.printf "%s.%s: %s -> %s.%s: %s\n" (Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid) 
                                														(Big_int.string_of_big_int timestamp) (Big_int.string_of_big_int nodeidsearch) 
                                														(Big_int.string_of_big_int localidsearch) (Big_int.string_of_big_int !vartimestamp);
																														
																														listinfo @ [(task_id, timestamp, nodeid, localid)] 	
																													)
																													else 
																													(
																													  loop (var+1) 
																													)
																												)	
																										)
																									| _ -> (listinfo)
																							)
													| _ -> (loop (var+1))
												)   
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node) listinfo
											)
                 		in loop 0
			| TaskEnded (_, task_id, _, _, sttrace, _) ->  
										let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												 (
													match (List.nth (List.rev sttrace) var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'O') then
																													(
																														searchingnext := 1;
																														vartimestamp := timestamp;
																														loop (var+1)
																													)	
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='I') then
																													(
																														searchingnext:=0;
																														Printf.printf "%s.%s: %s -> %s.%s: %s\n" (Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid) 
                                														(Big_int.string_of_big_int timestamp) (Big_int.string_of_big_int nodeidsearch) 
                                														(Big_int.string_of_big_int localidsearch) (Big_int.string_of_big_int !vartimestamp);
																														listinfo @ [(task_id, timestamp, nodeid, localid)] 	
																													)
																													else 
																													(
																													  loop (var+1) 
																													)
																												)	
																																
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												if ( !searchingnext == 0) then
																												(
																												 if ((Big_int.compare_big_int nodeidsearch nodeid) == 0 && (Big_int.compare_big_int localidsearch localid) == 0 
																														  && io == 'O') then
																													(
																														searchingnext := 1;	
																														vartimestamp := timestamp;
																														loop (var+1)
																													)
																													else (loop (var+1))
																												)
																												else
																												(
																													if (!searchingnext == 1 && io =='I') then
																													(
																														searchingnext:=0;
																														Printf.printf "%s.%s: %s -> %s.%s: %s\n" (Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid) 
                                														(Big_int.string_of_big_int timestamp) (Big_int.string_of_big_int nodeidsearch) 
                                														(Big_int.string_of_big_int localidsearch) (Big_int.string_of_big_int !vartimestamp);
																														listinfo @ [(task_id, timestamp, nodeid, localid)] 	
																													)
																													else 
																													(
																													  loop (var+1) 
																													)
																												)	
																										)
																									| _ -> (listinfo)
																							)
													| _ -> (loop (var+1))
												)   
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node) listinfo
											)
                 		in loop 0
	    | Empty  _ -> (listinfo)   
      | _        -> codegen_ (ind+1) (node_succs node) listinfo
  in codegen_ 0 node []
	
											

(* Function look up, necessary task id *)
let t_all_message node = 
  let rec codegen_ ind node = 
    match node with
      | TaskBlocked (_, _, task_id, _, sttrace, _) -> 
                let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												 (
													match (List.nth sttrace var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												Printf.printf "TaskID: %d; Time: %s; io: %c; MsgID: %s.%s\n" task_id 
																												(Big_int.string_of_big_int timestamp) io (Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid);
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												Printf.printf "TaskID: %d; Time: %s; io: %c; MsgID: %s.%s\n" task_id 
																												(Big_int.string_of_big_int timestamp) io (Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid);	
																										)
																									| _ -> ()
																							)
													| _ -> ()
												); loop (var+1) ; 
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node)
											)
                 		in loop 0
				| TaskEnded (_, task_id, _, _, sttrace, _) ->  
										let rec loop var =
											if (var < (List.length sttrace)) then
                  		(
												 (
													match (List.nth sttrace var) with
														| ST_MessTrace (mess_trace) ->
																						(
																							match mess_trace with
																									| MessTrace (mess_entry) -> 
																										( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												Printf.printf "TaskID: %d; Time: %s; io: %c; MsgID: %s.%s\n" task_id 
																												(Big_int.string_of_big_int timestamp) io (Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid);
																										)
																									| MessTraceWithInfo (mess_entry, _) -> ( 
																											match mess_entry with
																											| MessEntry (timestamp, io, nodeid, localid) ->
																												Printf.printf "TaskID: %d; Time: %s; io: %c; MsgID: %s.%s\n" task_id
																												(Big_int.string_of_big_int timestamp) io (Big_int.string_of_big_int nodeid) (Big_int.string_of_big_int localid);	
																										)
																									| _ -> ()
																							)
													| _ -> ()
												) ; loop (var+1);  
                      )
                      else
                      (
												codegen_ (ind+1) (node_succs node)
											)
                 		in loop 0
      | Empty  _ -> ()   
      | _        -> codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node
	
	
	
	
(* Return the last number of stream *)
let t_last_number_taskid node = 
  let rec codegen_ ind node  = 
    match node with
      | TaskBlocked (_, _, task_id, _, sttrace, _) ->          
            task_id :: codegen_ (ind+1) (node_succs node);
       | TaskEnded (_, task_id, _, _, sttrace, _) ->    
            task_id :: codegen_ (ind+1) (node_succs node);
      | Empty  _ -> ([])   
      | _        -> codegen_ (ind+1) (node_succs node)
  in codegen_ 0 node

