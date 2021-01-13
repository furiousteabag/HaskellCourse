factorial n = if n == 0 then 1 else n * factorial (n - 1)

factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

factorial'' 0 = 1
factorial'' n = if n < 0 then error "arg must be >= 0" else n * factorial' (n - 1)

factorial''' 0 = 1
factorial''' n | n < 0 =  error "arg must be >= 0" 
               | n > 0 = n * factorial' (n - 1)

factorial4 :: Integer -> Integer
factorial4 n | n == 0    = 1
             | n > 0     = n * factorial' (n - 1)
             | otherwise =  error "arg must be >= 0" 

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 0 =   fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = - fibonacci (n + 1) + fibonacci (n + 2)

factorial5 n | n >= 0    = helper 1 n
             | otherwise = error "arg must be >= 0" 
helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

fibonacci' :: Integer -> Integer
fibonacci' n  = fib_helper n 0 1

fib_helper 0 n1 n2 = n1
fib_helper it n1 n2 | it > 0 = fib_helper (it - 1) n2 (n1 + n2)
                    | it < 0 = fib_helper (it + 1) n2 (n1 - n2)
