(** Desktop calculator from Chapter 2 of Developing Applications
 * with OCaml book http://caml.inria.fr/pub/docs/oreilly-book/ 
 *
 * To try it out in utop:
     * run open Core.Std.Caml (to override Core's types)
     * #use "desktop_calculator.ml"
     * let example = [Digit 3; Plus; Digit 2; Equals;] (this will do 3 + 2)
     * transition_list initial_state example;;
 * *)

type key = Plus | Minus | Times | Div | Equals | Digit of int;;

type state = {
    last_computation_done : int;
    last_key_activated : key;
    last_operator_activated : key;
    value_printed : int
}

let is_digit = function x -> (x >= 0) && (x <= 9);;

let is_valid_key k = match k with
    | Digit n -> is_digit n
    | _ -> true;;

let evaluate x y ky = match ky with
    | Plus -> x + y
    | Minus -> x - y
    | Times -> x * y
    | Div -> x / y
    | Equals -> y
    | Digit _ -> failwith "evaluate: no op";;

(** Initial state of the calculator *)
let initial_state = {
    last_computation_done=0;
    last_key_activated=Equals;
    last_operator_activated=Equals;
    value_printed=0
}

let transition st ky =
    (* This is the same as
     * let digit_transition n last_key = 
         match last_key with
         | Digit _ ->...
         ...
     * let a b = function ...  makes a function a which takes in
     * b and another parameter and does matching on second parameter
     *)
    let digit_transition n = function
        | Digit _ -> { st with 
            last_key_activated=ky;
            value_printed=st.value_printed * 10 + n }
        | _ -> { st with last_key_activated=ky; value_printed=n }
    in match ky with
        | Digit p -> (digit_transition p) st.last_key_activated
        | _ -> let res = 
            evaluate st.last_computation_done
                        st.value_printed
                        st.last_operator_activated
        in {
            last_computation_done=res;
            last_key_activated=ky;
            last_operator_activated=ky;
            value_printed=res
        };;

let transition_list st ls = List.fold_left transition st ls;;
