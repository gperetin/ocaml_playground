(* Graph traversal exercise from Developing applications with OCaml book,
 * Chapter 2 *)

(* Directed graph where each vertex has a list of it's successors *)
type 'a graph = ('a * 'a list) list;;

let insert_vtx g vtx =
    List.append [(vtx, [])] g

let rec insert_edge g v1 v2 =
    let insert curr_v new_v =
        (fst curr_v, new_v::snd curr_v)
    in match g with
    | x::xs -> if fst x = v1 insert v2 else
