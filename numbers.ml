let rec gcd p q =
  if p == 0 then q
  else gcd (q mod p) p

let coprime p q =
  if (gcd p q) == 1 then true else false

let coprime' p q =
  if (gcd p q) == 1 then 1 else 0

let rec phi_aux n m res =
  if m = 0 then res 
  else phi_aux n (m-1) (res + (coprime' n m) )

let phi n = phi_aux n (n-1) 0

let rec prime_test ?(a=2) p =
  if p = 1 then false 
  else if (a * a) > p then true
  else if (p mod a) = 0 then false
  else prime_test ~a:(a+1) p

let rec primelist l u res =
  if l = (u+1) then res
  else primelist 
    (l+1) u (if prime_test l then l :: res else res)  

let all_primes l u =
  primelist l u []

let goldbach n = 
  let lst = all_primes 2 (n-1) in
  let v = List.find (fun x->prime_test (n-x)) lst in  
  (n-v, v)

let rec factors_aux a n res =
  if n = 1 then List.rev res
  else if (prime_test a) && (n mod a = 0)  then factors_aux a (n/a) (a :: res) 
  else factors_aux (a+1) n res

let factors n = 
  factors_aux 2 n []
