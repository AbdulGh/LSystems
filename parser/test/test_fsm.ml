open OUnit2
open Fsm

let bfut1 = basicfsm [('a', "aaaa"); ('b', "aa")]
let bfut2 = basicfsm [('a', "aa"); ('b', "aaaa")]
let bfut3 = basicfsm [('a', "aa"); ('b', "ab")]

let charopt_to_string (input: char option): string = match input with
    | Some c -> Printf.sprintf "Some %c" c;
    | None -> "None"

let ae_co =
    assert_equal ~printer:charopt_to_string ~msg:"ae_co"

let basicfsm_tests = "basicfsm tests" >::: [
    "state reuse" >:: (fun _ -> assert_equal bfut1.numstates 5);
    "state reuse 2" >:: (fun _ -> assert_equal bfut2.numstates 5);
    "bfut1 accepts aa" >:: (
        fun _ -> (assert_equal (fsm_accepts bfut1 "aa") (Some 'b'))
    );
    "bfut1 accepts aaaa" >:: (
        fun _ -> (assert_equal (fsm_accepts bfut1 "aaaa") (Some 'a'))
    );
    "bfut2 accepts aa" >:: (
        fun _ -> (assert_equal (fsm_accepts bfut2 "aa") (Some 'a'))
    );
    "bfut2 accepts aaaa" >:: (
        fun _ -> (assert_equal (fsm_accepts bfut2 "aaaa") (Some 'b'))
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
    );
    "bfut2 rejects some things that it should" >:: (
        fun _ -> (
            (ae_co (fsm_accepts bfut2 "aaa") None);
            (ae_co (fsm_accepts bfut2 "aaaaa") None);
            (ae_co (fsm_accepts bfut2 "a") None);
            (ae_co (fsm_accepts bfut2 "b") None);
            (ae_co (fsm_accepts bfut2 "") None);
            (ae_co (fsm_accepts bfut2 "hello, world") None);
        )
    );
    "state reuse 3" >:: (fun _ -> assert_equal bfut3.numstates 4);
    "bfut3 accepts aa" >:: (fun _ -> assert_equal (fsm_accepts bfut3 "aa") (Some 'a'));  
    "bfut3 rejects some things that it should" >:: (
        fun _ -> (
            (ae_co (fsm_accepts bfut3 "aaa") None);
            (ae_co (fsm_accepts bfut3 "aaaaa") None);
            (ae_co (fsm_accepts bfut3 "a") None);
            (ae_co (fsm_accepts bfut3 "b") None);
            (ae_co (fsm_accepts bfut3 "") None);
            (ae_co (fsm_accepts bfut3 "hello, world") None);
        )
    );
    "bfut3 accepts ab" >:: (fun _ -> assert_equal (fsm_accepts bfut3 "ab") (Some 'b'))  
]

let failurefun_tests = "failurefun tests" >::: [
    "overlap test 1" >:: (fun _ -> assert_equal (longest_overlap "aa" "bb") 0);
    "overlap test 2" >:: (fun _ -> assert_equal (longest_overlap "abcde" "defg") 2);
    "overlap test 3" >:: (fun _ -> assert_equal (longest_overlap "ijklm" "ijklmnop") 5);
    "failurefun prefix 1" >:: (fun _ -> assert_equal (failurefun_prefix "ijklmnop" ["ijklmnop"]) ("ijklmnop", 8));
    "failurefun prefix 2" >:: (
        fun _ -> assert_equal 
            (failurefun_prefix "ijklmnop" ["abcdi"; "xxxxx"; "dggshijkl"; "lmnopa"])
            ("lmnopa", 5)
    )
]


let _ = run_test_tt_main basicfsm_tests
let _ = run_test_tt_main failurefun_tests
