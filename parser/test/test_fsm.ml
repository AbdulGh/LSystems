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

let bfut4 = basicfsm [('a', "ab"); ('b', "aaaaab")]

let failurefun_tests = "failurefun tests" >::: [
    "overlap test 1" >:: (fun _ -> assert_equal (longest_overlap "aa" "bb") 0);
    "overlap test 2" >:: (fun _ -> assert_equal (longest_overlap "abcde" "defg") 2);
    "overlap test 3" >:: (fun _ -> assert_equal (longest_overlap "ijklm" "ijklmnop") 5);
    "failurefun prefix 1" >:: (fun _ -> assert_equal (failurefun_prefix "ijklmnop" ["ijklmnop"]) ("ijklmnop", 8));
    "failurefun prefix 2" >:: (
        fun _ -> assert_equal 
            (failurefun_prefix "ijklmnop" ["abcdi"; "xxxxx"; "dggshijkl"; "lmnopa"])
            ("lmnopa", 5)
    );
    "failurefun prefix 3" >:: (
        fun _ -> assert_equal
            (failurefun_prefix "aaaab" ["ab"; "aaaaab"])
            ("ab", 2)
    );
    "failurefun test 1" >:: (
        fun _ -> assert_equal
            (failurefun bfut4 "aaaab" ["ab"; "aaaaab"])
            7
    )
]

let fut1 = make_fsm [('a', "aaaa"); ('b', "aa")]
let fut2 = make_fsm [('a', "ab"); ('b', "aaaaab")]
let fut3 = make_fsm [('a', "ba"); ('b', "aab")]

let make_fsm_tests = "makefsm tests" >::: [
    "fut1 accepts aa" >:: (
        fun _ -> (assert_equal (fsm_accepts fut1 "aa") (Some 'b'))
    );
    "fut1 accepts aaaa" >:: (
        fun _ -> (assert_equal (fsm_accepts fut1 "aaaa") (Some 'a'))
    );
    "fut1 rejects some things that it should" >:: (
        fun _ -> (
            (ae_co (fsm_accepts fut1 "a") None);
            (ae_co (fsm_accepts fut1 "b") None);
            (ae_co (fsm_accepts fut1 "") None);
            (ae_co (fsm_accepts fut1 "hello, world") None);
        )
    );
    "fut2 accepts ab" >:: (
        fun _ -> (assert_equal (fsm_accepts fut2 "ab") (Some 'a'))
    );
    "fut2 accepts aaaaab" >:: (
        fun _ -> (assert_equal (fsm_accepts fut2 "aaaaab") (Some 'b'))
    );
    "fut2 accepts aaaab as a" >:: (
        fun _ -> (assert_equal (fsm_accepts fut2 "aaaab") (Some 'a'))
    );
    "fut2 rejects some things that it should" >:: (
        fun _ -> (
            (ae_co (fsm_accepts fut2 "aaa") None);
            (ae_co (fsm_accepts fut2 "aaaaa") None);
            (ae_co (fsm_accepts fut2 "a") None);
            (ae_co (fsm_accepts fut2 "b") None);
            (ae_co (fsm_accepts fut2 "") None);
            (ae_co (fsm_accepts fut2 "hello, world") None);
        )
    );
    "fut3 accepts aaba as a" >:: (
        fun _ -> assert_equal
            (fsm_accepts fut3 "aaba")
            (Some 'a')
    )
]

let ffut1 = flatten_fsm ffut1;
let ffut2 = flatten_fsm ffut2;
let ffut3 = flatten_fsm ffut3;

let flattem_fsm_test = "flatten_fsm tests" >::: [
    "ffut1 accepts aa" >:: (
        fun _ -> (assert_equal (fsm_accepts ffut1 "aa") (Some 'b'))
    );
    "ffut1 accepts aaaa" >:: (
        fun _ -> (assert_equal (fsm_accepts ffut1 "aaaa") (Some 'a'))
    );
    "ffut1 rejects some things that it should" >:: (
        fun _ -> (
            (ae_co (fsm_accepts ffut1 "a") None);
            (ae_co (fsm_accepts ffut1 "b") None);
            (ae_co (fsm_accepts ffut1 "") None);
            (ae_co (fsm_accepts ffut1 "hello, world") None);
        )
    );
    "ffut2 accepts ab" >:: (
        fun _ -> (assert_equal (fsm_accepts ffut2 "ab") (Some 'a'))
    );
    "ffut2 accepts aaaaab" >:: (
        fun _ -> (assert_equal (fsm_accepts ffut2 "aaaaab") (Some 'b'))
    );
    "ffut2 accepts aaaab as a" >:: (
        fun _ -> (assert_equal (fsm_accepts ffut2 "aaaab") (Some 'a'))
    );
    "ffut2 rejects some things that it should" >:: (
        fun _ -> (
            (ae_co (fsm_accepts ffut2 "aaa") None);
            (ae_co (fsm_accepts ffut2 "aaaaa") None);
            (ae_co (fsm_accepts ffut2 "a") None);
            (ae_co (fsm_accepts ffut2 "b") None);
            (ae_co (fsm_accepts ffut2 "") None);
            (ae_co (fsm_accepts ffut2 "hello, world") None);
        )
    );
    "ffut3 accepts aaba as a" >:: (
        fun _ -> assert_equal
            (fsm_accepts ffut3 "aaba")
            (Some 'a')
    )
]

let _ = run_test_tt_main basicfsm_tests
let _ = run_test_tt_main failurefun_tests
let _ = run_test_tt_main make_fsm_tests
