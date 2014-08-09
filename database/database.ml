type data_card = string array
type data_base = { card_index: string -> int; data: data_card list }

(* Access field n of a card dc of database db *)
let field base name =
    let i = base.card_index name in fun (card: data_card) -> card.(i)


(* Get suffix from string s starting at position i *)
let suffix s i = try String.sub s i ((String.length s) - i)
                 with Invalid_argument("String.sub") -> ""


(* Split the string s by character c *)
let split c s =
    let rec split_from n =
        try let p = String.index_from s n c
            in (String.sub s n (p-n)) :: (split_from (p+1))
        with Not_found -> [ suffix s n ]
    in if s = "" then [] else split_from 0


(* Returns a function which takes a name and returns an index into 
 * names array *)
let mk_index list_names =
    let rec mk_enum a b = if a > b then [] else a::(mk_enum (a+1) b) in
    let list_index = (mk_enum 0 ((List.length list_names) - 1)) in
    let assoc_names = List.combine list_names list_index in
    function name -> List.assoc name assoc_names


let read_base filename =
    let channel = open_in filename in
    let split_line = split ':' in (* Partial function *)
    let list_names = split '|' (input_line channel) in
    let rec read_file () =
        try
            let data = Array.of_list (split_line (input_line channel)) in
                data :: (read_file ())
        with End_of_file -> close_in channel ; []
    in
    { card_index = mk_index list_names ; data = read_file () }
