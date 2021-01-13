import Data.Char

roots' a b c = 
    let d = sqrt (b ^ 2 - 4 * a * c) in
    ((-b - d) / (2 * a), (-b + d) / (2 * a))

factorial n
    | n >= 0 = let
        helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)
      in helper 1 n
    | otherwise = error "arg < 0"

seqA :: Integer -> Integer
seqA n
    | n == 0 = 1
    | n == 1 = 2
    | n > 1 = let
        help limit it n1 n2 n3 | it < limit = help limit (it + 1) n2 n3 (n3 + n2 - 2 * n1)
                               | otherwise  = n3
      in help (n - 2) 0 1 2 3
    | otherwise = error "arg < 0"

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
    | x > 0  = (sum' x, count' x)
    | x == 0 = (0, 1)
    | x < 0  = (sum' (abs x), count' (abs x))
    where

        sumd2 0 acc = acc
        sumd2 x acc = sumd2 (x `div` 10) (acc + (x `mod` 10))
        sum' = (flip sumd2) 0

        count2 0 acc = acc
        count2 x acc = count2 (x `div` 10) (acc + 1)
        count' = (flip count2) 0


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper f a b 999 0 
    where
        helper f a b 0 s = s + (hf a b 1000) * ((f a) + (f b)) / 2
        helper f a b n s = helper f a b (n - 1) (s + (hf a b 1000) * (f (b - n * (hf a b 1000))))
        hf a b l = (b - a) / l
