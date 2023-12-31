open OUnit2
open Fsm

let bfut1 = basicfsm [('a', "aaaa"); ('b', "aa")]
let bfut2 = basicfsm [('a', "aa"); ('b', "ab")]

let charopt_to_string (input: char option): string = match input with
    | Some c -> Printf.sprintf "Some %c" c;
    | None -> "None"

let ae_co =
    assert_equal ~printer:charopt_to_string ~msg:"ae_co"

let basicfsm_tests = "basicfsm tests" >::: [
    "state reuse" >:: (fun _ -> assert_equal bfut1.numstates 5);
    "bfut1 accepts what it should" >:: (
        fun _ -> ((assert_equal (fsm_accepts bfut1 "aa") (Some 'b')); assert_equal (fsm_accepts bfut1 "aaaa") (Some 'a'))
    ); 
    "bfut1 rejects some things that it should" >:: (
        fun _ -> (
            (ae_co (fsm_accepts bfut1 "aaa") None);
            (ae_co (fsm_accepts bfut1 "aaaaa") None);
            (ae_co (fsm_accepts bfut1 "a") None);
            (ae_co (fsm_accepts bfut1 "b") None);
            (ae_co (fsm_accepts bfut1 "") None);
            (ae_co (fsm_accepts bfut1 "hello, world") None);
        )
    )
]

let _ = run_test_tt_main basicfsm_tests
