(* 1 *)
let rec last = function 
  | [] -> None 
  | h :: [] -> Some h 
  | _ :: t -> last t

(* 2 *)
let rec last_two = function
  | [] -> None
  | _ :: [] -> None
  | [ h1; h2 ] -> Some (h1, h2)
  | _ :: t -> last_two t

(* 3 *)
let rec listnth n lst =
  if 0 > n then raise (Failure "nth")
  else
    match lst with
    | h :: t -> if n = 0 then h else listnth (n - 1) t
    | [] -> raise (Failure "nth")

(* 4 *)
let rec append lst1 lst2 =
  match lst1 with [] -> lst2 | h :: t -> h :: append t lst2

let rec reverse_aux lst1 lst2 =
  match lst1 with [] -> lst2 | h :: t -> reverse_aux t (h :: lst2)

(* 5 *)
let reverse lst = reverse_aux lst []

(* 6 *)
let rec list_compare lst1 lst2 =
  match (lst1, lst2) with
  | [], [] -> true
  | _, [] -> false
  | [], _ -> false
  | h1 :: t1, h2 :: t2 -> if h1 = h2 then list_compare t1 t2 else false

(* 7 *)
let is_palindrome lst = list_compare lst (reverse lst)

type 'a node = One of 'a | Many of 'a node list

let rec flatten_list (lst1 : 'a node list) lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> (
      match h with
      | One x -> flatten_list t (x :: lst2)
      | Many l -> flatten_list t (flatten_list l lst2))

(* 8 *)
(* It would be nice to be able to avoid the reverse at the end *)
let flatten (lst : 'a node list) = reverse (flatten_list lst [])

let rec compress_aux x lst res =
  match lst with
  | [] -> res
  | h :: t -> if h = x then compress_aux h t res else compress_aux h t (h :: res)

(* 9 *)
let compress lst =
  match lst with [] -> [] | h :: t -> reverse (compress_aux h t (h :: []))

let rec pack_aux x lst cur_res res =
  match lst with
  | [] -> cur_res :: res
  | h :: t ->
      if h = x then pack_aux h t (h :: cur_res) res
      else pack_aux h t (h :: []) (cur_res :: res)

(* 10 *)
let pack lst =
  match lst with [] -> [] | h :: _ -> reverse (pack_aux h lst [] [])

(* 11 *)
let rec encode_aux lst cur_res res =
  match (lst, cur_res) with
  | [], _ -> cur_res :: res
  | h :: t, (a, b) ->
      if h = b then encode_aux t (a + 1, b) res
      else encode_aux t (1, h) (cur_res :: res)

let encode lst =
  match lst with [] -> [] | h :: t -> reverse (encode_aux t (1, h) [])


(* Construction to allow run-lenght encoding of a list
   giving a list of One or Many.

   Uses recursion: mencode_aux, which compares the lst with the current result
   - the current result is the rle being built - ie. the character just seen 
   in the previous iteration along with it's count in the Many case.

   A helper function is used with mutual recursion (mencode_choose) to avoid code 
   duplication. It takes the tail of the current lst (what we need for the next
   iteration); the head of the current list; and the character from the previous iteration
   and it's count. It makes the decision - do we need to add to the count; or do we need
   a new encoding with One; and append the previous one to the results list.
*)

(* 12 *)
type 'a rle = One of 'a | Many of (int * 'a)

let rec mencode_aux lst cur_res res =
  match (lst, cur_res) with 
  | [], _ -> cur_res :: res 
  | h :: t, Many (a, b) -> 
     mencode_choose t h b a cur_res res
  | h :: t, One (b) ->
     mencode_choose t h b 1 cur_res res

and  mencode_choose t h b a cur_res res =
  if h = b then mencode_aux t (Many (a+1, b)) res
  else mencode_aux t (One h) (cur_res :: res)  


let mencode lst =
  match lst with [] -> [] | h :: t -> reverse (mencode_aux t (One h) [])

let rec range_aux fst lst res =
  if fst < lst then range_aux (fst+1) lst (fst :: res)
  else (fst :: res)

let rec duplicate_aux res = function
  | [] -> reverse res
  | h :: t -> duplicate_aux (h :: h :: res) t


(* 15 *)
let duplicate lst =
  duplicate_aux [] lst 


(* 16 *)
let rec replicate_aux res n cnt lst =
  match (lst, cnt) with 
  | [], _ -> reverse res
  | _ :: t, 0 -> replicate_aux res n n t
  | h :: _ , _ -> replicate_aux (h :: res) n (cnt-1) lst

let replicate lst n =
  replicate_aux [] n n lst 

(* 17 *)

let rec drop_aux lst res n cnt = 
  match (lst, cnt) with 
    | [], _ -> reverse res
    | _ :: t, 1 -> drop_aux t res  n n 
    | h :: t, m -> drop_aux t (h::res) n (m-1)

let drop lst n =
  drop_aux lst [] n n

let rec split_aux first last n =
  match (last, n) with
  | _, 0 | [], _ -> (reverse first, last)
  | h :: t, m -> split_aux (h :: first) t (m-1) 

let split lst n =
  split_aux [] lst n

let rec slice_aux res lst m n =
  match (lst, m, n) with
  | [], _, _ -> reverse res
  | h :: _, 0, 0 -> reverse (h :: res) 
  | h :: t, 0, q -> slice_aux (h::res) t 0 (q-1)
  | _ :: t, p, q -> slice_aux res t (p-1) (q-1)

let slice lst m n = 
  slice_aux [] lst m n

(* 23 *)
let range fst lst =
  if fst < lst then reverse (range_aux fst lst [])
  else range_aux lst fst []


