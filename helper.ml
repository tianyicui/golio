let parse_str str =
    let lb = Lexing.from_string str in
    let rec go () =
        match Parser.parse Lexer.tokens lb with
        | None -> []
        | Some x -> x :: go ()
    in go ()
;;

let repl_str str =
    let temp_in = Filename.temp_file "golio-" "" in
    let str_c = open_out temp_in in
    output_string str_c str;
    close_out str_c;

    let in_c = open_in temp_in in
    let temp_out = Filename.temp_file "golio-" "" in
    let out_c = open_out temp_out in
    Repl.repl in_c out_c false;
    close_in in_c;
    close_out out_c;

    let rst_c = open_in temp_out in
    let rec go () =
        try let line = input_line rst_c in line :: go ()
        with End_of_file -> []
    in String.concat "\n" (go ())
;;
