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

  "reverse - three" >:: (fun _ -> assert_equal ["c"; "b"; "a"] (reverse ["a"; "b"; "c" ]));

  "is_palindrome - five (odd)" >:: (fun _ -> assert_equal true (is_palindrome ["x"; "a"; "m"; "a"; "x"]));
  "is_palindrome - two (even)" >:: (fun _ -> assert_equal false (is_palindrome ["a"; "b" ]));

  "flatten - five" >:: (fun _ -> assert_equal ["a"; "b"; "c"; "d"; "e"] (flatten [One "a"; Many [ One "b"; Many [One "c"; One "d"]; One "e" ]])) ;

  "compress - a bunch" >:: (fun _ -> assert_equal ["a"; "b"; "c"; "a"; "d"; "e" ] (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]));

  "pack - a bunch" >:: (fun _ -> assert_equal [["a";"a";"a";"a"]; ["b"]; ["c"; "c"]; ["a";"a"]; ["d"]; ["e"; "e"; "e"; "e"]] (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]));

  "encode - a bunch" >:: (fun _ -> assert_equal [(4, "a"); (1,"b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]));

  "mencode - a bunch" >:: (fun _ -> assert_equal [Many(4, "a"); One "b"; Many(2, "c"); Many(2, "a"); One "d"; Many(4, "e")] (mencode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]));

  "duplicate - five" >:: (fun _ -> assert_equal ["a"; "a"; "b"; "b"; "c"; "c"; "d"; "d"; "e"; "e" ] (duplicate ["a"; "b"; "c"; "d"; "e" ] ));

  "replicate - three" >:: (fun _ -> assert_equal ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] (replicate ["a"; "b"; "c" ] 3 ));
]

let _ = run_test_tt_main tests
