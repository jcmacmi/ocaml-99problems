open OUnit2
open Lists

let tests = "test suite for lists" >::: [
  "last - empty" >:: (fun _ -> assert_equal None (last []));
  "last - four" >:: (fun _ -> assert_equal (Some "d") (last ["a"; "b"; "c"; "d"]));

  "last_two empty" >:: (fun _ -> assert_equal None (last_two []));
  "last_two four" >:: (fun _ -> assert_equal (Some ("c", "d") ) (last_two ["a"; "b"; "c"; "d"]));

  "listnth - five" >:: (fun _ -> assert_equal "c" (listnth ["a"; "b"; "c"; "d"; "e"] 2 ));
  "listnth if negative raise Failure" >:: (fun _ -> assert_raises (Failure "nth") (fun() -> listnth ["a"] 2));

  "length - three" >:: (fun _ -> assert_equal 3 (length ["a"; "b"; "c"]));
  "length - empty" >:: (fun _ -> assert_equal 0 (length [] ));
    
]

let _ = run_test_tt_main tests
