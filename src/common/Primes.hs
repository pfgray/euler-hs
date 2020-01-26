module Primes where
-- import Numbers
import Data.List

-- isPrime :: (Integer) -> Bool
isPrime 2 = True
isPrime n
  | n `mod` 2 == 0 = False
  | otherwise = isPrimeRec (n, 3)

-- isPrimeRec :: () -> Bool
isPrimeRec (2, _) = True
isPrimeRec (3, _) = True
isPrimeRec (n, i)
  | i * i > n = True
  | n `mod` i == 0 = False
  | otherwise = isPrimeRec (n, i + 2)

nats = 1 : map (\a -> a+1) nats

primes = 2 : filter(isPrime) (drop 2 nats)

-- primeFact :: Integer -> [Integer]
primeFact n = primeFactRec (n, primes, [])

-- primeFactGroup :: Integer -> [(Integer, Integer)]
primeFactGroup n = map (\ps -> (head ps, length ps)) (groupBy (==) (primeFact n))

-- primeFactRec :: (Integer, [Integer], [Integer]) -> [Integer]
primeFactRec (1, _, acc) = acc
primeFactRec (n, p:ps, acc)
  | n `mod` p == 0 = primeFactRec (n `quot` p, p:ps, p:acc)
  | otherwise = primeFactRec (n, ps, acc)

-- divisorCount :: Integer -> Integer
divisorCount n =
  foldl (*) 1 (map (\(a, b) -> (b + 1)) (primeFactGroup n))

