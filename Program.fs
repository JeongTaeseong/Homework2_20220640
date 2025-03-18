module CS220.Program

/// How many different ways can we make change of a given amount of money in
/// Korean coins? Suppose we have 5 different kinds of coins: 500-won coins,
/// 100-won coins, 50-won coins, 10-won coins, and 1-won coins. Write a function
/// prob1 that takes in an amount of money in won, and returns the number of
/// possible combinations. For example, when the given amount is 10, then the
/// function should return 2, because there are 2 different ways to make change
/// for 10 won: (1) one 10-won, and (2) ten 1-won. The function should return -1
/// if an errorneous input is given, e.g., when negative amount is given.
let prob1 amount =
  let rec f amount n =
    if amount<0 then 0
    elif amount=0 then 1
    elif n=1 then 1
    elif n=5 then ((f amount 4)+(f (amount-500) 5))
    elif n=4 then ((f amount 3)+(f (amount-100) 4))
    elif n=3 then ((f amount 2)+(f (amount-50) 3))
    elif n=2 then ((f amount 1)+(f (amount-10) 2))
    else -1
  if amount<0 then -1
  else f amount 5

/// Write a function `prob2` that computes GCD (Greatest Common Divisor) of two
/// given integers. This function should return -1 if both inputs are 0.
let prob2 a b =
  let rec f a b n =
    if a=0 && b=0 then -1
    elif a=0 then b
    elif b=0 then a
    elif a<n || b<n then 1
    elif a%n=0 && b%n=0 then n*(f (a/n) (b/n) 2)
    else (f a b (n+1))
  f a b 2


/// Write a function `prob3` that takes in a string s and an integer n, and
/// returns a string that repeats s for n times. For example, if "abc" and 3 are
/// given, the function should return "abcabcabc". When n is zero, the function
/// returns an empty string. When n is negative, it returns a string that
/// repeats reversed s for -n times. For example, pow "abc" -3 will return
/// "cbacbacba".
let prob3 s n =
  let rev (str: string) =
    let arr = str.ToCharArray()
    System.Array.Reverse(arr)
    System.String(arr)
  let rec rep s n str=
    if n=0 then str
    elif n>0 then rep s (n-1) (str+s)
    else rep s (n+1) (str+s)
  if n=0 then ""
  elif n>0 then rep s n ""
  else rep (rev s) n ""


/// Write a function `prob4` that takes in an unsigned integer n (uint32), and
/// returns the smallest integral divisor of n that is greater than 1. For
/// example, given 45, the function will return 3 (45 % 3 = 0). This function
/// returns 0 for all error cases, e.g., when the given number is 1u.
let prob4 (n: uint32) =
  let rec f (a: uint32) (b: uint32) =
    if a<=1u then 0u
    elif a%b=0u then b
    elif a-2u*b-1u<b*b then a
    else f a (b+1u)
  f n 2u

/// Write a function `prob5` that takes in an unsigned integer as input, and
/// checks if the number is a prime number or not. If the number is prime, then
/// the function returns true. Otherwise, it returns false. Hint: you can use
/// the `prob4` function above.
let prob5 (n: uint32) =
  let rec f (a: uint32) (b: uint32) =
    if a<=1u then 0u
    elif a%b=0u then b
    elif a-2u*b-1u<b*b then a
    else f a (b+1u)
  if n>1u && (f n 2u)=n then true
  else false

[<EntryPoint>]
let main _args =
  0
