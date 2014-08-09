(* Merging two lists exercise from Developing applications with OCaml
 * book, Chapter 2.
 *)

(* Take 2 sorted lists and return merged list, also sorted *)
let rec merge_i xs ys =
    match xs, ys with
    | [], _ -> ys
    | _, [] -> xs
    | hx::txs, hy::tys ->
        if hx < hy then hx::merge_i txs ys else hy::merge_i xs tys


let rec merge cmp xs ys =
    match xs, ys with
    | [], _ -> ys
    | _, [] -> xs
    | hx::txs, hy::tys ->
        if cmp xs ys then hx::merge cmp txs ys else hy::merge cmp xs tys
