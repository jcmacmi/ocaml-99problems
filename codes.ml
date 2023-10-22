

let rec gray_aux n = 
  if n = 1 then ["0" ; "1"]
  else List.append (gray_aux n-1) ["1"]
