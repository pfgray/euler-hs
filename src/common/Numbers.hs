module Numbers where

withIndex as = zip [0..(length as - 1)] as

-- a stream of natural numbers
nats = 1 : map (+1) nats

-- a stream of triangles
triangles = trianglesI 0 1
trianglesI sum i = 
  let newSum = sum + i in
    newSum : trianglesI newSum (i + 1)

collatz :: Integer -> Integer -> Integer
collatz sum n
  | n <= 1 = sum + 1
  | n `mod` 2 == 0 = collatz (sum + 1) (n `div` 2)
  | otherwise = collatz (sum + 1) (3 * n + 1)
