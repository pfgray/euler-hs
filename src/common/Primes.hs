module Primes where

isPrime :: (Int) -> Bool
isPrime 2 = True
isPrime n
  | n `mod` 2 == 0 = False
  | otherwise = isPrimeRec (n, 3)

isPrimeRec :: (Int, Int) -> Bool
isPrimeRec (2, _) = True
isPrimeRec (3, _) = True
isPrimeRec (n, i)
  | i * i > n = True
  | n `mod` i == 0 = False
  | otherwise = isPrimeRec (n, i + 2)

nats = 1 : map (+1) nats

primes = 2 : filter(isPrime) (drop 2 nats)

primeFact :: Int -> [Int]
primeFact n = primeFactRec (n, primes, [])

primeFactRec :: (Int, [Int], [Int]) -> [Int]
primeFactRec (1, _, acc) = acc
primeFactRec (n, p:ps, acc)
  | n `mod` p == 0 = primeFactRec (n `quot` p, p:ps, p:acc)
  | otherwise = primeFactRec (n, ps, acc)

