open OUnit2
open Lists

let tests = "test suite for last" >::: [
  "empty" >:: (fun _ -> assert_equal None (last []) );
  "four" >:: (fun _ -> assert_equal (Some "d") (last ["a"; "b"; "c"; "d"]));

  "last2_empty" >:: (fun _ -> assert_equal None (last_two []));
  "last2_four" >:: (fun _ -> assert_equal (Some ("c", "d") ) (last_two ["a"; "b"; "c"; "d"]));

  "listnth" >:: (fun _ -> assert_raises (Failure "nth") (fun() -> listnth 2 ["a"] ));
]

let _ = run_test_tt_main tests
