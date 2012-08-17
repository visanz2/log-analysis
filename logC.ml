open LogAst
open MapAst

type args = {mutable filename: string}

let set_filename a s = a.filename <- s

let contains s1 s2 =
  let re = Str.regexp_string s2
  in
    try ignore (Str.search_forward re s1 0); true
    with Not_found -> false

(* Search the log fiales and the map file with one direction *)
let searchfiles direction numbernode =
let children = Sys.readdir direction in
let rec getlogfile ind listfile =
    if (ind < (Array.length children)) then
    (
      let filename = Array.get children ind in
        if (contains filename ("mon_n" ^ numbernode ^ "_worker" ) ) then
        (
          getlogfile (ind+1) ((direction ^ filename)::listfile)
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
in getlogfile 0 [direction ^ "n" ^numbernode ^"_tasks.map"]

(* PRINCIPAL FUNCTION OF THIS PROGRAM  *)
let main () = 
  if ((Array.length Sys.argv) < 3) then
  (
    Printf.printf "It's necessary to give 2 arguments, more arguments will be ignored:\n";
    Printf.printf "1.- Directory where the file is, for example, c:/ \n";
    Printf.printf "2.- Number of nodes (always two digits) for opening the log files and the map file.\n"
  )
  else 
  ( (* Read the log files and the map files *)
    let listfile = searchfiles Sys.argv.(1) Sys.argv.(2) in
      if ((List.length listfile) == 1 ) then (* Because always insert the map file *) 
      (
        Printf.printf "No such file.\n";
        exit 0
      )
      else(); 
      let rec readfile ind listmap listlog =
        if (ind < (List.length listfile)) then
        (          let extension = String.sub (List.nth listfile ind) ((String.length (List.nth listfile ind))-3) 3 in 
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
                              let _ = Printf.printf "\nError: line %d pos %d %s\n\n" line cnum tok in
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
        Printf.printf "\nThe programm read: %d LOG file(s) for the node %s\n" (List.length listlog) Sys.argv.(2);
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
          flush stdout;
          let _ = 
            match (input_line stdin)  with 
              | "0"-> Printf.printf "THE END"; exit 0
              | "1"-> (* Menu 1 - Search execution time of ended task for Boxname *)  
                      Printf.printf "\n What task do you want to search? (write the Boxname)\n";
                      flush stdout;
                      let taskidsearch = t_search_boxname (List.nth listmap 0) (input_line stdin) []  in
                        let rec searchexecutiontime var =
                          if (var < (List.length listlog)) then
                          (
                            if ((t_exectime (List.nth listlog var) taskidsearch) == false) then
                            (
                              searchexecutiontime (var+1)
                            )
                            else
                            (
                              menu (ind+1);
                            )                          )
                          else
                          (
                            Printf.printf "Impossible to find the task in the load log files.\n"
                          )
                        in searchexecutiontime 0 

              | "2"-> (* Menu 2 - Search execution time of ended task for Task ID *)  
                        Printf.printf "\n What task do you want to search? (write the Task ID)\n"; 
                        flush stdout;
                        let taskidsearch = try read_int() 
                        with Failure _ -> Printf.printf "\n That's not a number \n"; -1
                        in
                          let rec searchexecutiontime var =
                            if (var < (List.length listlog)) then
                            (
                              if ((t_exectime (List.nth listlog var) taskidsearch) == false) then
                              (
                                searchexecutiontime (var+1)
                              )
                              else
                              (
                                menu (ind+1);
                              )
                            )
                            else
                            (
                              Printf.printf "Impossible to find the task in the load log files.\n"
                            )
                          in searchexecutiontime 0 
              | "3"-> (* Menu 3 - Search execution time of blocked task for Boxname *)  
                      Printf.printf "\n What task do you want to search? (write the Boxname)\n";
                      flush stdout;
                      let taskidsearch = t_search_boxname (List.nth listmap 0) (input_line stdin) []  in
                        let rec searchexecutiontime var =
                          if (var < (List.length listlog)) then
                          (
                            if ((t_blocktime (List.nth listlog var) taskidsearch) == false) then
                            (
                              searchexecutiontime (var+1)
                            )
                            else
                            (
                              menu (ind+1);
                            )
                          )
                          else
                          (
                            Printf.printf "Impossible to find the task in the load log files.\n"
                          )
                        in searchexecutiontime 0 

              | "4"-> (* Menu 4 - Search execution time of blocked task for Task ID *)  
                        Printf.printf "\n What task do you want to search? (write the Task ID)\n"; 
                        flush stdout;
                        let taskidsearch = try read_int() 
                        with Failure _ -> Printf.printf "\n That's not a number \n"; -1
                        in
                          let rec searchexecutiontime var =
                            if (var < (List.length listlog)) then
                            (
                              if ((t_blocktime (List.nth listlog var) taskidsearch) == false) then
                              (
                                searchexecutiontime (var+1)
                              )
                              else
                              (
                                menu (ind+1);
                              )
                            )
                            else
                            (
                              Printf.printf "The task is never blocked or it's impossible to find in the load log files.\n"
                            )
                          in searchexecutiontime 0 
              | "5"-> (* Menu 5 - List of time of read's record time for task searched with Boxname *)  
                      Printf.printf "\n What task do you want to search? (write the Boxname)\n";
                      flush stdout;
                      let taskidsearch = t_search_boxname (List.nth listmap 0) (input_line stdin) []  in
                        let rec searchexecutiontime var =
                          if (var < (List.length listlog)) then
                          (
                            if ((t_list_rec_read (List.nth listlog var) taskidsearch) == false) then
                            (
                              searchexecutiontime (var+1)
                            )
                            else
                            (
                              menu (ind+1);
                            )
                          )
                          else
                          (
                            Printf.printf "Impossible to find the task in the load log files.\n"
                          )
                        in searchexecutiontime 0 

              | "6"-> (* Menu 6 - List of time of read's record time for task searched with Task ID *)  
                        Printf.printf "\n What task do you want to search? (write the Task ID)\n"; 
                        flush stdout;
                        let taskidsearch = try read_int() 
                        with Failure _ -> Printf.printf "\n That's not a number \n"; -1
                        in
                          let rec searchexecutiontime var =
                            if (var < (List.length listlog)) then
                            (
                              if ((t_list_rec_read (List.nth listlog var) taskidsearch) == false) then
                              (
                                searchexecutiontime (var+1)
                              )
                              else
                              (
                                menu (ind+1);
                              )
                            )
                            else
                            (
                              Printf.printf "Impossible to find the task in the load log files.\n"
                            )
                          in searchexecutiontime 0 
              | "7"-> (* Menu 7 - List of time of write's record time for task searched with Boxname *)  
                      Printf.printf "\n What task do you want to search? (write the Boxname)\n";
                      flush stdout;
                      let taskidsearch = t_search_boxname (List.nth listmap 0) (input_line stdin) []  in
                        let rec searchexecutiontime var =
                          if (var < (List.length listlog)) then
                          (
                            if ((t_list_rec_write (List.nth listlog var) taskidsearch) == false) then
                            (
                              searchexecutiontime (var+1)
                            )
                            else
                            (
                              menu (ind+1);
                            )
                          )
                          else
                          (
                            Printf.printf "Impossible to find the task in the load log files.\n"
                          )
                        in searchexecutiontime 0 

              | "8"-> (* Menu 8 - List of time of read's record time for task searched with Task ID *)  
                        Printf.printf "\n What task do you want to search? (write the Task ID)\n"; 
                        flush stdout;
                        let taskidsearch = try read_int() 
                        with Failure _ -> Printf.printf "\n That's not a number \n"; -1
                        in
                          let rec searchexecutiontime var =
                            if (var < (List.length listlog)) then
                            (
                              if ((t_list_rec_write (List.nth listlog var) taskidsearch) == false) then
                              (
                                searchexecutiontime (var+1)
                              )
                              else
                              (
                                menu (ind+1);
                              )
                            )
                            else
                            (
                              Printf.printf "Impossible to find the task in the load log files.\n"
                            )
                          in searchexecutiontime 0 

              | _ -> Printf.printf "It's not a good option.\n"; menu (ind+1)
    in Printf.printf "\n"
            in menu 0;
    in readfile 0 [] []; Printf.printf "\n"
  )

let _ = Printexc.print main () ;;
