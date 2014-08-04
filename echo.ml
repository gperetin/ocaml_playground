(** Print a list of arguments and *)

let echo () = 
    let len = Array.length Sys.argv in
    if len > 1 then
        begin
            for i = 1 to len - 1 do
                print_char ' ';
                print_string Sys.argv.(i);
            done;
            print_newline ();
        end;;

echo ()
