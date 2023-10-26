let binary_to_gray n =
  n lxor (n lsr 1)

let rec binary_digits_string n m res =
  if m = 0 then res 
  else binary_digits_string (n lsr 1) (m-1) ((string_of_int (n land 1)) ^ res)

let rec gray_code_aux n m res = 
  if m = (1 lsl n) then res
  else gray_code_aux n (m+1) ((binary_digits_string (binary_to_gray m) n "") :: res)  

let gray_code n =
  List.rev (gray_code_aux n 0 [])
