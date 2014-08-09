(* Stack example from Developing applications with OCaml book, Chapter 3 *)

type 'a stack = { mutable ind: int; size: int; mutable elts: 'a array }

exception Stack_empty;;
exception Stack_full;;

let init_stack n = { ind=0; size=n; elts=[||]}

let pop p =
    if p.ind = 0 then raise Stack_empty
    else (p.ind <- p.ind - 1; p.elts.(p.ind));;

let push p v =
    if p.elts = [||] then (p.elts <- Array.create p.size v; p.ind <- 1)
    else if p.ind >= p.size then raise Stack_full
    else (p.elts.(p.ind) <- v; p.ind <- p.ind + 1);;
