open LogAst
open MapAst
open ArgAst
 


type args = {mutable filename: string}

(* Variables for use the arguments *)
let varvecall = ref []
let varvecfather1 = ref []
let varvecfather2 = ref []
let varvecfather3 = ref []
let varvecson1 = ref []
let varvecson2 = ref []
let varvecson3 = ref []
let varvecalltask = ref []
let varcountfile = ref 0
let varcount = ref (-1)
let savename = ref ""
let father = ref ""
let relationfatherson = ref ""
let nodeidsearch = ref Big_int.zero_big_int
let localidsearch = ref Big_int.zero_big_int
let latency = ref Big_int.zero_big_int
let varlatency = ref []
let vartaskidfinded = ref false
let messageoptions = ref false
let streamidmax = ref 0
let taskidmax = ref 0
let resultsdotfile = ref "digraph G {\n"
let resultgraphfile = ref ""
let varresultgraphfile = ref ""
let commandmessageid = ref ""
let optionexec     = ref false
let optionlisttime = ref false
let execoption   = [("e", true); ("ended", true); ("b", false); ("blocked", false)]
let listtimeoption = [("r", true); ("read", true); ("w", false); ("write", false)]

let print_tag t s = print_string t; print_endline s

let contains s1 s2 =
  let re = Str.regexp_string s2
  in
    try ignore (Str.search_forward re s1 0); true
    with Not_found -> false
;;

let my_max = function
    [] -> invalid_arg "empty list"
  | x::xs -> List.fold_left max x xs
;;

(* Remove the first element to produce the list *)
let rem_first l = 
  match l with            
  | [] -> []
  | h::t -> t 
;;


(* Sort the list *)
let rec sort lst =
   match lst with
     [] -> []
   | head :: tail -> insert head (sort tail)
 and insert elt lst =
   match lst with
     [] -> [elt]
   | head :: tail -> 
			let (timestamp, taskid, workerid) = elt in
			let (timestamphead, taskidhead, workeridhead) = head in
				if (Big_int.compare_big_int timestamp timestamphead) == (-1) 
				then elt :: lst 
				else head :: insert elt tail
;;

(* Search the log files and the map file*)
let searchfilesasd mapfile =
  let numbernode = (String.sub mapfile ((String.length mapfile) -12) 2) in 
    let directory = (String.sub mapfile 0 ((String.length mapfile) -13)) in 
      let children = Sys.readdir directory in
        let rec getlogfile ind listfile =
          if (ind < (Array.length children)) then
          (
            let filename = Array.get children ind in
              if (contains filename ("mon_n" ^ numbernode ^ "_worker" ) ) then
              (
                getlogfile (ind+1) ((directory ^ filename)::listfile)
              )
              else 
              (
                getlogfile (ind+1) listfile
              )
          )
          else 
          (
            listfile
          )
  in getlogfile 0 [mapfile]
;;



(* PRINCIPAL FUNCTION OF THIS PROGRAM  *)
let main () = 
  if ((Array.length Sys.argv) == 1 || (String.length Sys.argv.(Array.length Sys.argv-1))<14 ) then
  (
    Printf.printf "\nIt's necessary to give 1 arguments, more arguments will be ignored:\n";
    Printf.printf "1.- The direction of the mapfile\n";
    Printf.printf "if you want more information, write --help mapfile \n";
    exit 0
  )
  else();

  (* Read the log files and the map files *)
  let listfile = searchfilesasd Sys.argv.(Array.length Sys.argv-1)  in
    if ((List.length listfile) == 1 ) then (* Because always insert the map file *) 
    (
      Printf.printf "No such file. put a correct file.\n";
      exit 0
    )
    else();
		 
    let rec readfile ind listmap listlog =
			Printf.printf "%d/%d\n" ind (List.length listfile);
			flush stdout;
      if (ind < (List.length listfile) && (List.length listfile) != 1) then
      (        let extension = String.sub (List.nth listfile ind) ((String.length (List.nth listfile ind))-3) 3 in 
          if (String.compare extension "log") == 0 then 
          (
            let the_file = Pervasives.open_in (List.nth listfile ind) in
              let lexbuf = Lexing.from_channel the_file in      
                let the_ast = 
                  try LogParser.src LogLexer.token lexbuf
                    with exn -> (* fix this *)
                      begin
                      let curr = lexbuf.Lexing.lex_curr_p in
                        let line = curr.Lexing.pos_lnum in
                          let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
                            let tok = Lexing.lexeme lexbuf in
                              let _ = Printf.printf "\nError: line %d pos %d %s %s\n\n" line cnum tok (List.nth listfile ind) in
                        raise LogAst.Compilation_Error
                    end in 
                  let _ = LogAst.codegen the_ast in
                    readfile (ind+1) listmap (the_ast::listlog) 
          )
          else 
          (
            if (String.compare extension "map") == 0 then 
            (
              let the_file = Pervasives.open_in (List.nth listfile ind) in
                let lexbuf = Lexing.from_channel the_file in
                  let the_ast = 
                  try MapParser.src MapLexer.token lexbuf
                  with exn -> (* fix this *)
                  begin
                    let curr = lexbuf.Lexing.lex_curr_p in
                      let line = curr.Lexing.pos_lnum in
                        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
                          let tok = Lexing.lexeme lexbuf in
                            let _ = Printf.printf "\nError: line %d pos %d %s in Map File\n\n" line cnum tok in
                      raise MapAst.Compilation_Error
              end in
                  readfile (ind+1) (the_ast::listmap) listlog 
              )
              else 
              (
                Printf.printf "" 
              )
           )
      )
      else 
      (
      );

      Printf.printf "\nThe programm read: %d LOG file(s). \n" (List.length listlog);

(* Command line options *)

Arg.parse 

  (Arg.align
    [
      ("--exec",Arg.Tuple 
        [ Arg.Symbol (["e"; "ended"; "b"; "blocked"], 
          (  
            fun key -> 
              optionexec := (List.assoc key execoption)
          )
        ); 
          Arg.String 
          (
            fun key -> 
              Printf.printf "\n\nEXECUTION TIME FOR TASKS\n------------------------\n";
              let rec searchexecutiontime var =
                if (var < (List.length listlog)) then
                (
                  let _ = if ( !optionexec == true ) then
                          (
                            try  Printf.printf "%s" (t_exectime (List.nth listlog var) (int_of_string (key))); 
                            with Failure _ -> 
                            (
                              let listtaskidsearch = t_search_boxname_all (List.nth listmap 0) key []  in
                                let rec searchealltask ind =
                                  if (ind < (List.length listtaskidsearch)) then
                                  (
                                    Printf.printf "%s" (t_exectime (List.nth listlog var) (List.nth listtaskidsearch ind));
                                    searchealltask (ind+1)
                                  )
                                  else
                                  ();
                                in searchealltask 0
                            )
                          )
                          else
                          (
                            try  Printf.printf "%s" (t_blocktime (List.nth listlog var) (int_of_string (key))); 
                            with Failure _ -> 
                            (
                              let listtaskidsearch = t_search_boxname_all (List.nth listmap 0) key []  in
                                let rec searchealltask ind =
                                  if (ind < (List.length listtaskidsearch)) then
                                  (
                                    Printf.printf "%s" (t_blocktime (List.nth listlog var) (List.nth listtaskidsearch ind));
                                    searchealltask (ind+1)
                                  )
                                  else
                                  ();
                                in searchealltask 0
                            )
                          );
                  in
                  searchexecutiontime (var+1)
                )
                else
                ();
              in searchexecutiontime 0; messageoptions:= true;
          )
      ]
, (" For the tasks execution time. You must to put the option for blocked task ('b' or 'blocked') or ended task ('e' or 'ended' ) 
and after put the taskID or the boxname of the task\n"
^ "For example:\n\t --exec e leq1 -> Show the execution ended task time for all the task with boxname 'leq1'"
^ "\n\t --exec b 1 -> Show the execution blocked task time for all the task with ID = 1 "
^ "\n\t --exec blocked 1 --exec blocked 2 -> Show the execution blocked task time for all the task with ID = 1 and 2\n")
);


("--listtime", 
  Arg.Tuple 
    [
      Arg.Symbol 
      (
        ["r"; "read"; "w"; "write"], 
        (
          fun key -> 
            optionlisttime := (List.assoc key listtimeoption)
        )
      ); 

      Arg.String 
      (
        fun key -> 
          Printf.printf "\n\nLIST TIME OF WHEN THE TASK READ/WRITE IN THE STREAM\n---------------------------------------------------\n";
          let rec searchexecutiontime var =
            if (var < (List.length listlog)) then
            (
              if ( !optionlisttime == true ) then
              (
                try  Printf.printf "%s" (t_list_rec_read (List.nth listlog var) (int_of_string (key))); 
                with Failure _ -> 
                (
                  let listtaskidsearch = t_search_boxname_all (List.nth listmap 0) key []  in
                    let rec searchealltask ind =
                      if (ind < (List.length listtaskidsearch)) then
                      (
                        Printf.printf "%s" (t_list_rec_read (List.nth listlog var) (List.nth listtaskidsearch ind));
                        searchealltask (ind+1)
                      )
                      else
                      ();
                    in searchealltask 0
                )
              )
              else
              (
                try  Printf.printf "%s" (t_list_rec_write (List.nth listlog var) (int_of_string (key))); 
                with Failure _ -> 
                (
                  let listtaskidsearch = t_search_boxname_all (List.nth listmap 0) key[]  in
                    let rec searchealltask ind =
                      if (ind < (List.length listtaskidsearch)) then
                      (
                        Printf.printf "%s" (t_list_rec_write (List.nth listlog var) (List.nth listtaskidsearch ind));
                        searchealltask (ind+1)
                      )
                      else
                      ();
                  in searchealltask 0
                )
              );
              searchexecutiontime (var+1)
            )
            else
            ();
          in searchexecutiontime 0; messageoptions:= true;

)], (" Show the time of record time. You must to put the option for read's records ('r' or 'read') or write's records('w' or 'write' ) 
and the taskID or the boxname of the task\n"
^ "For example:\n\t --listtime r leq1 -> Show the time of read's record time for all the task with boxname 'leq1'"
^ "\n\t --listtime w 1 -> Show the time of read's record time for all the task with ID = 1 "
^ "\n\t --listtime write 1 --listtime write 2-> Show the time of read's record time for all the task with ID = 1 and 2 \n"));



("-o", 
  Arg.String
	(  
		fun key ->
  			(Printf.printf "OPTIONS FILE -> %s\n" key;
  			if Sys.file_exists key then 
  			(
          let the_file = Pervasives.open_in (key) in
            let lexbuf = Lexing.from_channel the_file in      
              let the_ast = 
                try ArgParser.src ArgLexer.token lexbuf
                with exn -> (* fix this *)
                    begin
                      let curr = lexbuf.Lexing.lex_curr_p in
                        let line = curr.Lexing.pos_lnum in
                          let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
                            let tok = Lexing.lexeme lexbuf in
                              let _ = Printf.printf "\nError: line %d pos %d %s %s\n\n" line cnum tok key in
                    raise ArgAst.Compilation_Error
          end in 
          let listarg = ArgAst.read_arguments the_ast in
            Printf.printf "Reading %d arguments in the option file:\n" (List.length listarg);
            List.iter 
							(
								fun key ->
									match (String.sub key 0 4)  with 
              			| "exec"-> 
														let listexec =  Str.split (Str.regexp "[%]+") key in
															Printf.printf "%s" ("--exec " ^ (List.nth listexec 1) ^ " " ^ (List.nth listexec 3) ^ " ->"
															^ "\n\nEXECUTION TIME FOR TASKS \n" ^ "------------------------\n");
															let rec searchexecutiontime var =
                                if (var < (List.length listlog)) then
                                (
                									if (List.assoc (List.nth listexec 1) execoption) then
                									( (* INSIDE the ended task*)
                										if ( (String.compare (List.nth listexec 2) "B") == 0) then
                										(
          											   		let listtaskidsearch = t_search_boxname_all (List.nth listmap 0)  (List.nth listexec 3) []  in
                                     		let rec searchealltask ind =
                                          if (ind < (List.length listtaskidsearch)) then
                                          (
                                            Printf.printf "%s" (t_exectime (List.nth listlog var) (List.nth listtaskidsearch ind));
                                            searchealltask (ind+1)
                                          )
                                          else
                                          ();
                                        in searchealltask 0
                										)
                										else
                										(
                											Printf.printf "%s" (t_exectime (List.nth listlog var) (int_of_string ((List.nth listexec 3)))); 
                										)
                									)
                									else
                									((* INSIDE the blocked task*)
																		if ( (String.compare (List.nth listexec 2) "B") == 0) then
                										(
          											   		let listtaskidsearch = t_search_boxname_all (List.nth listmap 0)  (List.nth listexec 3) []  in
                                     		let rec searchealltask ind =
                                          if (ind < (List.length listtaskidsearch)) then
                                          (
                                            Printf.printf "%s" (t_blocktime (List.nth listlog var) (List.nth listtaskidsearch ind));
                                            searchealltask (ind+1)
                                          )
                                          else
                                          ();
                                        in searchealltask 0
                										)
                										else
                										(
                											Printf.printf "%s" (t_blocktime (List.nth listlog var) (int_of_string ((List.nth listexec 3)))); 
                										)
                									);
                  								searchexecutiontime (var+1);
																)
																else ();
																in searchexecutiontime 0;
  									| "list"-> 
														let listtime =  Str.split (Str.regexp "[%]+") key in
															Printf.printf "%s"
															 ( "--listtime " ^ (List.nth listtime 1)  ^ " " ^ (List.nth listtime 3) ^ " ->"
															 ^ "\n\nLIST TIME OF WHEN THE TASK READ/WRITE IN THE STREAM\n" 
															 ^ "---------------------------------------------------\n");	
															let rec searchexecutiontime var =
                                if (var < (List.length listlog)) then
                                (
                									if (List.assoc (List.nth listtime 1) listtimeoption) then
                									( (* INSIDE the read list*)
                										if ( (String.compare (List.nth listtime 2) "B") == 0) then
                										(
          											   		let listtaskidsearch = t_search_boxname_all (List.nth listmap 0)  (List.nth listtime 3) []  in
                                     		let rec searchealltask ind =
                                          if (ind < (List.length listtaskidsearch)) then
                                          (
                                            Printf.printf "%s" (t_list_rec_read (List.nth listlog var) (List.nth listtaskidsearch ind));
                                            searchealltask (ind+1)
                                          )
                                          else
                                          ();
                                        in searchealltask 0
                										)
                										else
                										(
                											Printf.printf "%s" (t_list_rec_read (List.nth listlog var) (int_of_string ((List.nth listtime 3)))); 
                										)
                									)
                									else
                									((* INSIDE the write list*)
																		if ( (String.compare (List.nth listtime 2) "B") == 0) then
                										(
          											   		let listtaskidsearch = t_search_boxname_all (List.nth listmap 0)  (List.nth listtime 3) []  in
                                     		let rec searchealltask ind =
                                          if (ind < (List.length listtaskidsearch)) then
                                          (
                                            Printf.printf "%s" (t_list_rec_write (List.nth listlog var) (List.nth listtaskidsearch ind));
                                            searchealltask (ind+1)
                                          )
                                          else
                                          ();
                                        in searchealltask 0
                										)
                										else
                										(
                											Printf.printf "%s" (t_list_rec_write (List.nth listlog var) (int_of_string (List.nth listtime 3))); 
                										)
                									);
                  								searchexecutiontime (var+1);
																)
																else ();
																in searchexecutiontime 0;
  									| "stre"->  
															let liststream =  Str.split (Str.regexp "[%]+") key in
																Printf.printf "%s"
																( "--stream " ^ (List.nth liststream 1) ^ " ->"
																^ "\n\nLIST OF STREAM TRACE IN THE LOG FILE\n" ^ "------------------------------------\n");
																let rec searchstreams var =
                                  if (var < (List.length listlog)) then
                                  (
                                    Printf.printf "%s" (t_search_stream (List.nth listlog var) (int_of_string (List.nth liststream 1)) var);
                                    searchstreams (var+1) 
                                  )
                                  else
                                	();
                    					in searchstreams 0
  									| "save"-> let listsave =  Str.split (Str.regexp "[%]+") key in
																savename := (List.nth listsave 1); 
  									| _ -> ()
								
							) listarg;
	  			)
  			else 
  			(
  				print_endline "Option file don't exists.";
  			);
			)
	), (" With this argument you can put the file with all the arguments. The program read the file similar a if you put the arguments in the console. The file must be .log. \n"));


(* Lookup Command Parameter *)
("--lookup", 

      Arg.String 
      (
       fun key -> 
			 (
          let taskidsearch = try int_of_string (key) 
          	with Failure _ -> Printf.printf "\n That's not a number \n"; (-1)
          	in
  						let rec graphtask var =
  							if (var < (List.length listlog)) then
            		(
  								t_look_up_message (List.nth listlog var) taskidsearch; 
        					graphtask (var+1);  
                )
                else
                (
									Printf.printf "\n";
  							)
           	in graphtask 0; messageoptions:= true;
			 )
			)
, 

("this argument is for show all the Message for one Task ID. It's necessary put the Task ID.  \n"));


(* Latency Command Parameter *)
("--latency", 
	Arg.Tuple 
    [
      Arg.String (* Message ID *) 
      (
        (
          fun key -> 
            let varnode = try Big_int.big_int_of_string (String.sub key 0 (String.index_from key 0 '.'))  
					 		with Failure _ -> Printf.printf "\n That's not a number \n"; Big_int.minus_big_int (Big_int.unit_big_int)
								in nodeidsearch := varnode;
								let varlocal =  try Big_int.big_int_of_string (String.sub key ((String.index_from key 0 '.')+1) ((String.length key)-(String.index_from key 0 '.')-1)) 
									with Failure _ -> Printf.printf "\n That's not a number \n"; Big_int.minus_big_int (Big_int.unit_big_int)
									in localidsearch := varlocal;
        )
      ); 

      Arg.String (* Task Id *)
      (
       fun key -> 
			 (
          let taskidsearch = try int_of_string (key)
          	with Failure _ -> Printf.printf "\n That's not a number \n"; (-1)
          	in
						latency := Big_int.zero_big_int;
							let rec graphtask var =
								if (var < (List.length listlog) && List.length !varlatency == 0) then
            		(
									latency := (t_latency_info (List.nth listlog var) !nodeidsearch !localidsearch);
									varlatency := !varlatency @ (t_latency_next (List.nth listlog var) !nodeidsearch !localidsearch);

        					graphtask (var+1);  
                )
                else ();
         			in graphtask 0;
							let rec searching ind =
								Printf.printf "-";
								if ( ind > -1 && (List.length !varlatency > 0)) then
								(
									let  (task_id, timestamp, nodeid, localid) = List.hd !varlatency in
									if (task_id == taskidsearch) then
									(
										latency := Big_int.sub_big_int timestamp !latency;
									)
									else
									(
  									varlatency := List.tl !varlatency;
  									
      							let rec loop var =
      								if (var < (List.length listlog)) then
                  		(
												let lenghtvar = List.length !varlatency in 
      									varlatency := !varlatency @ t_latency_next (List.nth listlog var) nodeid localid;
      									if ( lenghtvar == (List.length !varlatency)) then
      									(
              						loop (var+1);
												)  
												else ()
                      )
                      else();
               			in loop 0;searching (ind+1);
									)
								)else()
							in searching 0; Printf.printf "\nLatency is %s\n" (Big_int.string_of_big_int !latency); messageoptions:= true;
			 )
			)
		]
, 

(" This argument show the time use for arrive the message the one point a another point. The argument needs Message ID [NodeID.LocalID] and TaskId"
  ^ " The Message ID says de begin and the Task ID says the end point. For show all the message until ends, write TaskID = 0. \n"));


(* Trace Command Parameter *)
("--trace", 

  Arg.Tuple 
    [
      Arg.String (* Message ID *) 
      (
        (
          fun key -> 
            let varnode = try Big_int.big_int_of_string (String.sub key 0 (String.index_from key 0 '.'))  
					 		with Failure _ -> Printf.printf "\n That's not a number \n"; Big_int.minus_big_int (Big_int.unit_big_int)
								in nodeidsearch := varnode;
								let varlocal =  try Big_int.big_int_of_string (String.sub key ((String.index_from key 0 '.')+1) ((String.length key)-(String.index_from key 0 '.')-1)) 
									with Failure _ -> Printf.printf "\n That's not a number \n"; Big_int.minus_big_int (Big_int.unit_big_int)
									in localidsearch := varlocal;
        )
      ); 

      Arg.String (* Task Id *)
      (
       fun key -> 
			 (
					let taskidsearch = try int_of_string (key)
          	with Failure _ -> Printf.printf "\n That's not a number \n"; (-1)
          	in
						latency := Big_int.zero_big_int;
							let rec graphtask var =
								if (var < (List.length listlog) && List.length !varlatency == 0) then
            		(
									varlatency := !varlatency @ (t_latency_trace (List.nth listlog var) !nodeidsearch !localidsearch);
        					graphtask (var+1);  
                )
                else ();
         			in graphtask 0;
							let rec searching ind =
								if ( ind > -1 && (List.length !varlatency > 0)) then
								(
									let  (task_id, timestamp, nodeid, localid) = List.hd !varlatency in
									if (task_id == taskidsearch) then
									(
										latency := Big_int.sub_big_int timestamp !latency;
									)
									else
									(
  									varlatency := List.tl !varlatency;
  									
      							let rec loop var =
      								if (var < (List.length listlog)) then
                  		(
												let lenghtvar = List.length !varlatency in 
      									varlatency := !varlatency @ t_latency_trace (List.nth listlog var) nodeid localid;
      									if ( lenghtvar == (List.length !varlatency)) then
      									(
              						loop (var+1);
												)  
												else 
												()
                      )
                      else();
               			in loop 0;searching (ind+1);
									)
								)else()
							in searching 0; messageoptions:= true;
				)
			)
		]
, 

(" Show the trace of one message. This argument needs a MessageID [NodeID.LocalID] and one TaskID. "
  ^ " The Message ID says de begin and the Task ID says the end point. For show all the message until ends, write TaskID = 0.\n"));


(* Latency Command Parameter *)
("--rlatency", 
	Arg.Tuple 
    [
      Arg.String (* Message ID *) 
      (
        (
          fun key -> 
            let varnode = try Big_int.big_int_of_string (String.sub key 0 (String.index_from key 0 '.'))  
					 		with Failure _ -> Printf.printf "\n That's not a number \n"; Big_int.minus_big_int (Big_int.unit_big_int)
								in nodeidsearch := varnode;
								let varlocal =  try Big_int.big_int_of_string (String.sub key ((String.index_from key 0 '.')+1) ((String.length key)-(String.index_from key 0 '.')-1)) 
									with Failure _ -> Printf.printf "\n That's not a number \n"; Big_int.minus_big_int (Big_int.unit_big_int)
									in localidsearch := varlocal;
        )
      ); 

      Arg.String (* Task Id *)
      (
       fun key -> 
			 (
				let taskidsearch = try int_of_string (key)
          	with Failure _ -> Printf.printf "\n That's not a number \n"; (-1)
          	in
						latency := Big_int.zero_big_int;
							let rec graphtask var =
								if (var < (List.length listlog) && List.length !varlatency == 0) then
            		(
									latency := (t_latency_info (List.nth listlog var) !nodeidsearch !localidsearch);
									varlatency := !varlatency @ (t_latency_before (List.nth listlog var) !nodeidsearch !localidsearch);

        					graphtask (var+1);  
                )
                else ();
         			in graphtask 0;
							let rec searching ind =
								Printf.printf "-";
								if ( ind > -1 && (List.length !varlatency > 0)) then
								(
									let  (task_id, timestamp, nodeid, localid) = List.hd !varlatency in
									if (task_id == taskidsearch) then
									(
										latency := Big_int.sub_big_int !latency timestamp ;
									)
									else
									(
  									varlatency := List.tl !varlatency;
  									
      							let rec loop var =
      								if (var < (List.length listlog)) then
                  		(
												let lenghtvar = List.length !varlatency in 
      									varlatency := !varlatency @ t_latency_before (List.nth listlog var) nodeid localid;
      									if ( lenghtvar == (List.length !varlatency)) then
      									(
              						loop (var+1);
												)  
												else ()
                      )
                      else();
               			in loop 0;searching (ind+1);
									)
								)else()
							in searching 0; Printf.printf "\nLatency is %s\n" (Big_int.string_of_big_int !latency); messageoptions:= true;
			 )
			)
		]
, 

(" This argument show the time use for arrive the message the one point a another point, but it's the reverse option." 
  ^ "The argument needs Message ID [NodeID.LocalID] and TaskId"
  ^ " The Message ID says de begin and the Task ID says the end point. For show all the message until ends, write TaskID = 0. \n"));


(* Trace Reverse Command Parameter *)
("--rtrace", 

  Arg.Tuple 
    [
      Arg.String (* Message ID *) 
      (
        (
          fun key -> 
            let varnode = try Big_int.big_int_of_string (String.sub key 0 (String.index_from key 0 '.'))  
					 		with Failure _ -> Printf.printf "\n That's not a number \n"; Big_int.minus_big_int (Big_int.unit_big_int)
								in nodeidsearch := varnode;
								let varlocal =  try Big_int.big_int_of_string (String.sub key ((String.index_from key 0 '.')+1) ((String.length key)-(String.index_from key 0 '.')-1)) 
									with Failure _ -> Printf.printf "\n That's not a number \n"; Big_int.minus_big_int (Big_int.unit_big_int)
									in localidsearch := varlocal;
        )
      ); 

      Arg.String (* Task Id *)
      (
       fun key -> 
			 (
          let taskidsearch = try int_of_string (key)
          	with Failure _ -> Printf.printf "\n That's not a number \n"; (-1)
          	in
						latency := Big_int.zero_big_int;
							let rec graphtask var =
								if (var < (List.length listlog) && List.length !varlatency == 0) then
            		(
									varlatency := !varlatency @ (t_latency_rtrace (List.nth listlog var) !nodeidsearch !localidsearch);
        					graphtask (var+1);  
                )
                else ();
         			in graphtask 0;
							let rec searching ind =
								if ( ind > -1 && (List.length !varlatency > 0)) then
								(
									let  (task_id, timestamp, nodeid, localid) = List.hd !varlatency in
									if (task_id == taskidsearch) then
									(
										latency := Big_int.sub_big_int timestamp !latency;
									)
									else
									(
  									varlatency := List.tl !varlatency;
  									
      							let rec loop var =
      								if (var < (List.length listlog)) then
                  		(
												let lenghtvar = List.length !varlatency in 
      									varlatency := !varlatency @ t_latency_rtrace (List.nth listlog var) nodeid localid;
      									if ( lenghtvar == (List.length !varlatency)) then
      									(
              						loop (var+1);
												)  
												else 
												()
                      )
                      else();
               			in loop 0;searching (ind+1);
									)
								)else()
							in searching 0; messageoptions:= true;
				
				)
			)
		]
, 

(" Show the trace of one message but see who message need before. This argument needs a MessageID [NodeID.LocalID] and one TaskID. "
  ^ " The Message ID says de begin and the Task ID says the end point. For show all the message until ends, write TaskID = 0.\n"));



(* Show all the message Command Parameter *)
("--swallmessage", 
      Arg.Unit (* Task Id *)
      (
         fun key -> 
  			 (
            let rec graphtask var =
							Printf.printf "\nThe WorkerID: %d has the next messages:\n" var;
  							if (var < (List.length listlog)) then
            		(
  								t_all_message (List.nth listlog var); 
        					graphtask (var+1);  
                )
                else
                (	)
           	in graphtask 0; messageoptions:= true;
  				
  			)	
			)
, 

(" Show all the message exist in the log files.\n"));




(* Show all the message Command Parameter *)
("--swmessage", 
      Arg.Int (* Task Id *)
      (
         fun key -> 
  			 (
						Printf.printf "\nThe WorkerID: %d has the next messages:\n" key;
						if (key < (List.length listlog)) then
        		(
  						t_all_message (List.nth listlog key);
            )
            else
            ();messageoptions := true
				 )  
  		)	
, 

(" Show all the message exist in one worker.\n"));



("--stream",
  Arg.String  
  (
    fun key -> 
      Printf.printf "\n\nLIST OF STREAM TRACE IN THE LOG FILE\n------------------------------------\n";
      let streamidsearch = try int_of_string(key)  
        with Failure _ -> Printf.printf "\n That's not a number \n"; (-1)
      in
        let rec searchstreams var =
          if (var < (List.length listlog)) then
          (
            Printf.printf "%s\n" (t_search_stream (List.nth listlog var) streamidsearch var );
            searchstreams (var+1) 
          )
          else
          (
          )
      in searchstreams 0; messageoptions := true

  ),      
(" Show Who read/write in the stream of program. You must to put the Stream ID \n"
^ "For example:\n\t --stream 1 -> Show all the stream message with stream ID = 1\n"
^ "\n\t --stream 1 --stream 2 -> Show all the stream message with stream ID = 1 and 2\n")
);
(*
("--save",Arg.Set_string savename, (" Save the result about the command line in one file. After the program finish." 
^ "\nOnly is possible to save in one file. The programme get the last save command."
^ "\nFor example: \n\t --save filename.txt\n" ));
*)
])

(print_tag " ") 
  (
    " Analysis tools for runtime logs "
    ^ "\nIt's necessary to give 1 arguments, more arguments will be ignored:\n" 
    ^ "1.- The direction of the mapfile\n"
  )
;

(* Message option *)
 	if (!messageoptions) then
		exit 0
  else();


(* If the option save is used *)
 (* if ((String.length !savename) == 0) then
  (
  )
  else
  (
    let channel = open_out !savename in
      output_string channel !resultscommandline;
      close_out channel;
      Printf.printf "The results are saving in %s \n" !savename;
      exit 0
  );

*)

(* Principal menu of the program *)
        let rec menu ind =
          Printf.printf "\nMenu: \n";
          Printf.printf "0.- Finish the program \n";
          Printf.printf "1.- Search execution time of ended task for Boxname\n";
          Printf.printf "2.- Search execution time of ended task for Task ID\n";
          Printf.printf "3.- Search execution time of blocked task for Boxname\n";
          Printf.printf "4.- Search execution time of blocked task for Task ID\n";
          Printf.printf "5.- List of time of read's record time for task searched with Boxname\n";
          Printf.printf "6.- List of time of read's record time for task searched with Task ID\n";
          Printf.printf "7.- List of time of write's record time for task searched with Boxname\n";
          Printf.printf "8.- List of time of write's record time for task searched with Task ID\n";
          Printf.printf "9.- Who read/write the specific stream\n";
					Printf.printf "10.- Save the dependence tree to the program in a dot file (Slow if the program is very big)\n";
					Printf.printf "11.- Save the TaskId tree to the program in a dot file(Slow if the program is very big)\n";
          flush stdout;
          let _ = 
            match (input_line stdin)  with 
              | "0"-> Printf.printf "THE END"; exit 0
              | "1"-> (* Menu 1 - Search execution time of ended task for Boxname *)  
                      Printf.printf "\n What task do you want to search? (write the Boxname) %d \n" (List.length listmap) ;
                      flush stdout;
                      let taskidsearch = t_search_boxname (List.hd listmap) (input_line stdin) []  in
                        let rec searchexecutiontime var =
                          if (var < (List.length listlog)) then
                          (
                            let timeexec = t_exectime (List.nth listlog var) taskidsearch in 
                              if ( (String.compare timeexec "") == 0) then
                              (
                                searchexecutiontime (var+1)
                              )
                              else
                              (
                                Printf.printf "%s" timeexec;
                                menu (ind+1)
                              )                          )
                          else
                          (
                            Printf.printf "Impossible to find the task in the load log files.\n";
                            menu (ind+1)
                          )
                        in searchexecutiontime 0 

              | "2"-> (* Menu 2 - Search execution time of ended task for Task ID *)  
                        Printf.printf "\n What task do you want to search? (write the Task ID)\n"; 
                        flush stdout;
                        let taskidsearch = try int_of_string(input_line stdin) 
                        with Failure _ -> Printf.printf "\n That's not a number \n"; (-1)
                        in
                          let rec searchexecutiontime var =
                            if (var < (List.length listlog)) then
                            (
                              let timeexec = t_exectime (List.nth listlog var) taskidsearch in
                                 if ( (String.compare timeexec "") == 0) then
                                (
                                  searchexecutiontime (var+1)
                                )
                                else
                                (
                                  Printf.printf "%s" timeexec;
                                  menu (ind+1);
                                )
                            )
                            else
                            (
                              Printf.printf "Impossible to find the task in the load log files.\n";
                              menu (ind+1)
                            )
                          in searchexecutiontime 0 
              | "3"-> (* Menu 3 - Search execution time of blocked task for Boxname *)  
                      Printf.printf "\n What task do you want to search? (write the Boxname)\n";
                      flush stdout;
                      let taskidsearch = t_search_boxname (List.nth listmap 0) (input_line stdin) []  in
                        let rec searchexecutiontime var =
                          if (var < (List.length listlog)) then
                          (
                            let blocktime = t_blocktime (List.nth listlog var) taskidsearch in
                              if ( (String.compare blocktime "") == 0) then
                              (
                                searchexecutiontime (var+1)
                              )
                              else
                              (
                                Printf.printf "%s" blocktime;
                                menu (ind+1);
                              )
                          )
                          else
                          (
                            Printf.printf "Impossible to find the task in the load log files.\n";
                            menu (ind+1)
                          )
                        in searchexecutiontime 0 

              | "4"-> (* Menu 4 - Search execution time of blocked task for Task ID *)  
                        Printf.printf "\n What task do you want to search? (write the Task ID)\n"; 
                        flush stdout;
                        let taskidsearch = try int_of_string(input_line stdin)  
                        with Failure _ -> Printf.printf "\n That's not a number \n"; (-1)
                        in
                          let rec searchexecutiontime var =
                            if (var < (List.length listlog)) then
                            (
                              let blocktime = t_blocktime (List.nth listlog var) taskidsearch in
                                if ( (String.compare blocktime "") == 0) then
                                (
                                  searchexecutiontime (var+1)
                                )
                                else
                                (
                                  Printf.printf "%s" blocktime;
                                  menu (ind+1);
                                )
                            )
                            else
                            (
                              Printf.printf "The task is never blocked or it's impossible to find in the load log files.\n";
                              menu (ind+1)
                            )
                          in searchexecutiontime 0 
              | "5"-> (* Menu 5 - List of time of read's record time for task searched with Boxname *)  
                      Printf.printf "\n What task do you want to search? (write the Boxname)\n";
                      flush stdout;
                      let taskidsearch = t_search_boxname (List.nth listmap 0) (input_line stdin) []  in
                        let rec searchexecutiontime var =
                          if (var < (List.length listlog)) then
                          (
                            let listread =  t_list_rec_read (List.nth listlog var) taskidsearch in
                              if ( (String.compare listread "") == 0) then
                              (
                                searchexecutiontime (var+1)
                              )
                              else
                              (
                                Printf.printf "%s" listread;
                                menu (ind+1);
                              )
                          )
                          else
                          (
                            Printf.printf "Impossible to find the task in the load log files.\n";
                            menu (ind+1)
                          )
                        in searchexecutiontime 0 

              | "6"-> (* Menu 6 - List of time of read's record time for task searched with Task ID *)  
                        Printf.printf "\n What task do you want to search? (write the Task ID)\n"; 
                        flush stdout;
                        let taskidsearch = try int_of_string(input_line stdin) 
                        with Failure _ -> Printf.printf "\n That's not a number \n"; (-1)
                        in
                          let rec searchexecutiontime var =
                            if (var < (List.length listlog)) then
                            (
                              let listread = t_list_rec_read (List.nth listlog var) taskidsearch in
                                if ( (String.compare listread "") == 0) then
                                (
                                  searchexecutiontime (var+1)
                                )
                                else
                                (
                                  Printf.printf "%s" listread;
                                  menu (ind+1);
                                )
                            )
                            else
                            (
                              Printf.printf "Impossible to find the task in the load log files.\n";
                              menu (ind+1)
                            )
                          in searchexecutiontime 0 
              | "7"-> (* Menu 7 - List of time of write's record time for task searched with Boxname *)  
                      Printf.printf "\n What task do you want to search? (write the Boxname)\n";
                      flush stdout;
                      let taskidsearch = t_search_boxname (List.nth listmap 0) (input_line stdin) []  in
                        let rec searchexecutiontime var =
                          if (var < (List.length listlog)) then
                          (
                            let listwrite = t_list_rec_write (List.nth listlog var) taskidsearch in 
                              if ( (String.compare listwrite "") == 0) then
                              (
                                searchexecutiontime (var+1)
                              )
                              else
                              (
                                Printf.printf "%s" listwrite;
                                menu (ind+1);
                              )
                          )
                          else
                          (
                            Printf.printf "Impossible to find the task in the load log files.\n";
                            menu (ind+1)
                          )
                        in searchexecutiontime 0 

              | "8"-> (* Menu 8 - List of time of read's record time for task searched with Task ID *)  
                        Printf.printf "\n What task do you want to search? (write the Task ID)\n"; 
                        flush stdout;
                        let taskidsearch = try int_of_string (input_line stdin)  
                        with Failure _ -> Printf.printf "\n That's not a number \n"; (-1)
                        in
                          let rec searchexecutiontime var =
                            if (var < (List.length listlog)) then
                            (
                              let listwrite = t_list_rec_write (List.nth listlog var) taskidsearch in 
                                if ( (String.compare listwrite "") == 0) then
                                (
                                  searchexecutiontime (var+1)
                                )
                                else
                                (
                                  Printf.printf "%s" listwrite;
                                  menu (ind+1);
                                )
                            )
                            else
                            (
                              Printf.printf "Impossible to find the task in the load log files.\n";
                              menu (ind+1)
                            )
                          in searchexecutiontime 0 
              | "9"-> (* Menu 9 - List of Stream trace with Stream ID specific *)  
												Printf.printf "\n What Stream do you want to search? (write the Stream ID)\n"; 
                        flush stdout;
                        let streamidsearch = try int_of_string (input_line stdin)  
                        with Failure _ -> Printf.printf "\n That's not a number \n"; (-1)
                        in
                          let rec searchstreams var =
                            if (var < (List.length listlog)) then
                            (
                              Printf.printf "%s" (t_search_stream (List.nth listlog var) streamidsearch var );
                              searchstreams (var+1) 
                            )
                            else
                            (
                              menu (ind+1)
                            )
                          in searchstreams 0 
              | "10" -> (* Menu 10 - Tree of the programme*)
											let channel = open_out "graph.dot" in
											Printf.printf "Loading all variables:\n";
												let rec searchstreamsparents var =
													
														if ( ( (var * 50) / (List.length listlog) ) > !varcount) then
														(
															Printf.printf " %d%% "  ((var * 50) / (List.length listlog));
															flush stdout;
															varcount := !varcount + 5;
														)
														else
														();
														
  													if (var < (List.length listlog)) then
                          	(
															let varmax = (my_max (t_last_number_stream (List.nth listlog var))) in
																
  															if (!streamidmax < varmax ) then
																(
																	streamidmax := varmax;
																)
																else ();
																
																varvecall :=  (t_vector_stream_write (List.nth listlog var) var);
																let totalvarvecall = List.length !varvecall in
																	let rec partition aux =
																		if ( aux < totalvarvecall) then
																		(
    																	let (streamid, worker) = List.hd !varvecall in
																			
																			varvecall := List.remove_assoc streamid !varvecall;
  																			if ( (streamid mod 3) == 0 ) then
        																(
  																				if ( List.mem_assoc streamid !varvecfather1 == false) then
      																		(
  																					varvecfather1 := !varvecfather1 @ [(streamid, worker)];
      																		)
      																		else();
  																			)
  																			else
  																			(
  																				if ( (streamid mod 3) == 1 ) then
          																(
    																				if ( List.mem_assoc streamid !varvecfather2 == false) then
        																		(
    																					varvecfather2 := !varvecfather2 @ [(streamid, worker)];
        																		)
        																		else();
    																			)
  																			 else
  																			 (
  																					if ( List.mem_assoc streamid !varvecfather3 == false) then
        																		(
    																					varvecfather3 := !varvecfather3 @ [(streamid, worker)];
        																		)
        																		else();
  																			 );
  																			);
																				
																			partition (aux+1);
																		)
																		else()
																	in partition 0;
																	
																varvecall :=  (t_vector_stream_read (List.nth listlog var) var);
																let totalvarvecall = List.length !varvecall in
																	let rec partition aux =
																		if ( aux < totalvarvecall) then
																		(
    																	let (streamid, worker) = List.hd !varvecall in
																			
																			varvecall := List.remove_assoc streamid !varvecall;
  																			if ( (streamid mod 3) == 0 ) then
        																(
  																				if ( List.mem_assoc streamid !varvecson1 == false) then
      																		(
  																					varvecson1 := !varvecson1 @ [(streamid, worker)];
      																		)
      																		else();
  																			)
  																			else
  																			(
  																				if ( (streamid mod 3) == 1 ) then
          																(
    																				if ( List.mem_assoc streamid !varvecson2 == false) then
        																		(
    																					varvecson2 := !varvecson2 @ [(streamid, worker)];
        																		)
        																		else();
    																			)
  																			 else
  																			 (
  																					if ( List.mem_assoc streamid !varvecson3 == false) then
        																		(
    																					varvecson3 := !varvecson3 @ [(streamid, worker)];
        																		)
        																		else();
  																			 );
  																			);
																				
																			partition (aux+1);
																		)
																		else()
																	in partition 0;
																	
																	searchstreamsparents (var+1);
                            )
                          	else
                          	(
														);
                         	in searchstreamsparents 0;

												varcount := -1;
												output_string channel ("digraph asde91 {\n ranksep=.75; size = \"7.5,7.5\";
                                {
                                node [shape=plaintext, fontsize=16];\n StreamID->");
																
  												let rec allstreams var =
														if ( ( (var * 50) / !streamidmax) > !varcount) then
														(
															Printf.printf " %d%% "  (((var * 50) / !streamidmax)+50);
															flush stdout;
															varcount := !varcount + 5;
														)
														else
														();
														
  													if ( !streamidmax > var ) then
                          		(
																output_string channel ((string_of_int var) ^ " -> ");
																allstreams (var+1);
  														)
  													else
														(
																output_string channel ((string_of_int var)  ^ ";");
  													)
                         	in allstreams 0;
													

																Printf.printf "LISTS %d %d %d ; %d %d %d\n" (List.length !varvecfather1) (List.length !varvecfather2)
																(List.length !varvecfather3) (List.length !varvecson1) (List.length !varvecson2) (List.length !varvecson3); 	

													
													varcount := -1;
													
													output_string channel ("\n} { rank = same; \"WorkerId:StreamId\" ; }; node [shape=box];");
													Printf.printf "\nStart Conversion in pdf:\n";
													
  												let rec creategraph var =
														if ( ( (var * 100) / !streamidmax) > !varcount) then
														( 
															Printf.printf "%d%% " ((var * 100) / !streamidmax);
															flush stdout;
															varcount := !varcount + 5;
														)
														else
														();
														if ( !streamidmax > var ) then
                        		(
  														if ( List.mem_assoc var !varvecfather1) then
  														(
  															father := "\"" ^ (string_of_int (List.assoc var !varvecfather1)) ^ ":" ^ (string_of_int var) ^ "\"";
																varvecfather1 := List.remove_assoc var !varvecfather1;
																flush stdout;
  														)
  														else
  														(
  															if ( List.mem_assoc var !varvecfather2) then
  															(
																	father := "\"" ^ (string_of_int (List.assoc var !varvecfather2)) ^ ":" ^ (string_of_int var) ^ "\"";
																	varvecfather2 := List.remove_assoc var !varvecfather2;
  															)
  															else
  															(
  																if (List.mem_assoc var !varvecfather3) then
  																(
																		father := "\"" ^ (string_of_int (List.assoc var !varvecfather3)) ^ ":" ^ (string_of_int var) ^ "\"";
																		varvecfather3 := List.remove_assoc var !varvecfather3;
  																)
  																else
  																(
  																);
  															);
  														);
															
  														(* SON STRUCTURE *)
															
															flush stdout;
															if ((String.length !father) > 0) then
															( 
    														flush stdout;
																if ( List.mem_assoc var !varvecson1) then
    														(
    															flush stdout;
																	output_string channel (!father ^ " -> \"" ^ (string_of_int (List.assoc var !varvecson1)) 
    																	^ ":" ^ (string_of_int var) ^ "\" \n{ rank = same; \"" ^ (string_of_int var) ^ "\";" ^ !father ^ ";" 
    																	^  "\"" ^ (string_of_int (List.assoc var !varvecson1)) ^":"^ (string_of_int var) ^ "\" ; }");
																			varvecson1 := List.remove_assoc var !varvecson1;
    														)
    														else
    														(
    															if ( List.mem_assoc var !varvecson2) then
    															(
																		output_string channel (!father ^ " -> \"" ^ (string_of_int (List.assoc var !varvecson2)) 
    																	^ ":" ^ (string_of_int var) ^ "\" \n{ rank = same; \"" ^ (string_of_int var) ^ "\";" ^ !father ^ ";" 
    																	^  "\"" ^ (string_of_int (List.assoc var !varvecson2)) ^":"^ (string_of_int var) ^ "\" ; }");
																			varvecson2 := List.remove_assoc var !varvecson2;
    															)
    															else
    															(
    																if (List.mem_assoc var !varvecson3) then
    																(
																			output_string channel (!father ^ " -> \"" ^ (string_of_int (List.assoc var !varvecson3)) 
    																	^ ":" ^ (string_of_int var) ^ "\" \n{ rank = same; \"" ^ (string_of_int var) ^ "\";" ^ !father ^ ";" 
    																	^  "\"" ^ (string_of_int (List.assoc var !varvecson3)) ^":"^ (string_of_int var) ^ "\" ; }");
																			varvecson3 := List.remove_assoc var !varvecson3;
    																)	
    																else
    																(
    																);
    															);
    														);
  														)
  														else();
															creategraph (var+1);
														)
														else()
                         	in creategraph 0; 
													
													varcount := -1;
													
									(* Finish the file and convert to pdf *)

                    output_string channel ("\n}");
                    close_out channel;
										varcount := (-1);
										varvecfather1 := [];
										varvecson1 := [];
										varvecfather2 := [];
										varvecson2 := [];
										varvecfather3 := [];
										varvecson3 := [];
                  	let a = Unix.fork () in
                      (
												match a with
                        | 0 -> (
														try
                              Unix.execvp "dot" [|"dot"; "-Tpdf"; "graph.dot"; "-O"|];
                           	with
                              _ -> Printf.printf "The results are saving in graph.dot. For convert the graph, put in the console 'dot -Tpdf graph.dot -O'.";
            										Printf.printf "The graph show one tree with all the stream numbers and the workerId who works with this streamID." ;
            										Printf.printf "All the dependences are marked with arrows. For example, x -> y, means that y wait for x because x write in the stream and y read the stream.\n";
																)
                        | -1 -> Printf.printf "The results are saving in graph.dot. For convert the graph, put in the console 'dot -Tpdf graph.dot -O'.";
            										Printf.printf "The graph show one tree with all the stream numbers and the workerId who works with this streamID." ;
            										Printf.printf "All the dependences are marked with arrows. For example, x -> y, means that y wait for x because x write in the stream and y read the stream.\n";
                        | _ -> ignore (Unix.wait ()); Printf.printf "The results are saving in the file graph.dot.pdf.\n";
                     );
                  	menu (ind+1)
										
							| "11" -> 
										let rec taskmax var =
												if (var < (List.length listlog)) then
                       ( 
													let varmax = (my_max (t_last_number_taskid (List.nth listlog var))) in
													(*compare x y returns 0 if x is equal to y, a negative integer if x is less than y, and a positive integer if x is greater than y.*)
													if (!taskidmax < varmax) then
													(
														taskidmax := varmax;
													)
													else ();
													taskmax (var+1);
    										)
    										else
    										()
										in taskmax 0;
Printf.printf "BEGIN %d\n" !taskidmax;
flush stdout;
										let channel = open_out "graphtask.dot" in
											output_string channel ("digraph asde91 {\n"
 												^ "ranksep=.75; size = \"7.5,7.5\";\n{ \n node [shape=plaintext, fontsize=16];\n TimeStamp when finish ->");
										let rec count taskid =
                   		let rec graphtask var =
													if ( taskid < !taskidmax) then
													(
														if (var < (List.length listlog)) then
                        		(
															let infosearched = (t_create_graph (List.nth listlog var) taskid var) in
															if (( List.length infosearched) > 0) then
															(
																let (timestamp, taskid, workerid) = List.hd infosearched in
															
          											output_string channel ((Big_int.string_of_big_int timestamp) ^ " -> ");
																
																varvecalltask := !varvecalltask @ infosearched;
																Printf.printf "%d  - - var: %d\n" taskid var;
																flush stdout;
																count (taskid+1);
															)
															else
															(
																graphtask (var+1);
															)
														)
                           else
                            (
															count (taskid+1);
														)
													)
													else ()
                       	in graphtask 0;
											in count 0; 
											output_string channel ( "THE END;");
											Printf.printf "Sorting\n";
											flush stdout;
											let vargraph = !varvecalltask in
											flush stdout;
												output_string channel ( "\n} { rank = same; \"TaskId:WorkerId\" ; }; node [shape=box];"); 
												let rec alltimestampson var =
													Printf.printf "+";
													flush stdout;
        									if ((List.length !varvecalltask) > (var) ) then
                        		(
															let (timestamp, taskid, workerid) = List.nth vargraph var in
  															output_string channel ("{ rank = same; " ^ (Big_int.string_of_big_int timestamp)
  																^ ";\"" ^  (string_of_int taskid) ^ ":" ^ (string_of_int workerid) ^ "\"; }");
          											alltimestampson (var+1);
        										)
        									else
        									(
        									)
                       	in alltimestampson 0;
												
													output_string channel ("\n}");
													close_out channel;
 													resultgraphfile := "";
 													varresultgraphfile := "";
													let a = Unix.fork () in
 													(
 														match a with
                         			| 0 -> 
																(
 																	try Unix.execvp "dot" [|"dot"; "-Tpdf"; "graphtask.dot"; "-O"|];
                            			with
                              			_ -> Printf.printf "The results are saving in graphtask.dot. For convert the graph, put in the console 'dot -Tpdf graphtask.dot -O'.";
																				 Printf.printf "The graph show one tree with all the stream numbers and the workerId who works with this streamID." ;
             													   Printf.printf "All the dependences are marked with arrows. For example, x -> y, means that y wait for x because x write in the stream and y read the stream.\n";
 																)
                        			| -1 -> Printf.printf "The results are saving in graphtask.dot. For convert the graph, put in the console 'dot -Tpdf graphtask.dot -O'.";
																			Printf.printf "The graph show one tree with all the stream numbers and the workerId who works with this streamID." ;
           														Printf.printf "All the dependences are marked with arrows. For example, x -> y, means that y wait for x because x write in the stream and y read the stream.\n";
                        			| _ -> ignore (Unix.wait ()); Printf.printf "The results are saving in the file graphtask.dot.pdf.\n";
                     );menu (ind+1)
						
              | _ -> Printf.printf "It's not a good option.\n"; menu (ind+1)
    in Printf.printf "\n"
            in menu 0;
    in readfile 0 [] []; Printf.printf "\n"
  

let _ = Printexc.print main () ;;





