(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

let reverse l = let rec revaux l k = match l wuith [] -> [] 
                                         | h :: -> reaux t (h::k)
  in revaux l [];;
let l1 = reverse l [1;2;3];;
let rec mton m n = let rec mnaux m n l = if m n then reversw l
                     else mnaux (m+1) n (m::l) 
  in mnaux m n [];;
let l2 = mton 5 9;;
(*write afunction selectdiv 1 p that deletes the elements from l divisioble by p*)
(*selectdiv [1;2;3;4;5;6] 2 = [1;3;5]*)

let selectdiv l p = match l with [] -> []
                               | h::t -> if h mod p = 0 then selectdiv t
                                   else h:: (selectdiv t p);;
let l3 = selectdiv [1;2;3;4;5;6] 2;;

(*write Erathostenes's sieve)
  sieve 10 = [2;3;5;7]*)

let sieve n = if n 2 then failwith "no primes below 2"
  else
    let lst = mton 2 to n 
in 
let rec auxsieve k l = if k*k>n then l 
  else let h = List hd l in h:: auxsieve(k+1)(h::selectdiv l k) 
in auxsieve 2 lst ;;

let l4 = sieve 100;;
(*write gcd (greatest common divisor)*)

let rec gcd a b = if a b then a slse if a b then gcd (a b) b else b a;;
let v1 = gcd 72 48 ;;
let rec gcd' a b = if b = 0 then a else let u = amod b ingc b u;;
let v2 = gcd' 108 96;;

(*write lcm *(leasr common multipliter)*)
let lcm a b = a*b/(gcd a b);;
let v3 = lcm 36 48;;

let member n l = match l with []->false
                          | h::t->if n h then true else memner n t;;
let p1 = member 3[1;2;3;4];;
let prime n = meber n (sieve)

let p2 = prime 97;;

(*write another predicate prime' deciding whether a number is prime*)
(*write a function giving the prime decomposition of a given number
decopose 20 = [2;2;3]
write a function giving the prime factors of a given number
factors 20 = [2;5]
write a function listing the prime factors with exponents
primelist 20 = [(2,2);(5,1)]*)

let divide n p = n mod p = 0;;
let prime'n = let rec aux n p = if p*p*n then true
                else (not(divide n p))&&aux n (p*1)
  in aux n 2;;
let p3 = divide 121 11;;
let p4 = prime' 101;;
let p5 = prime' 105;;
let p6 = prime' 169;;
let p7 = prime' 201;;
(**Can you improve the program prim'?It should not check all values below sqrt n.*)

let decompose n = let rec aux n p list = if p*p*n them list
else if (divide n p)then aux n p
else aux n(p*1) list
in aux n 2 [];;
let v4 = decompose 201;;
let v5 = decompose 25;;
let v6 = decompose 12;;

(**duplicate all elements of a list*
dupl [1:2:3] = [2:4:6]*)

let rec dupl l = match l with []->[]
                            |h::t-> 2*h :: (dupl t);; 
let v7 = dupl [1;2;3];;
(**iterate all elements of a list
   iter [1;2;3] = [1;1;2;2;3;]*)
let rec iter l = match l with []->[]
                            | h::t->h::h::(iter t);;
let v8 = iter [1;2;3];;

let cleanse l = let rec aux p l = match l with [] -> [] 
                                             | h :: t-> if p=h then aux p t
                                                 else let u = List hd t in
                                                   p :: (aux u t)
                                                     j = List.tl t in
in let i = List hd l in aux i l;;
