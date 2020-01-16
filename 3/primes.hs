
isPrime :: (Int) -> Bool
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

primes = 2 : drop 2 (filter(isPrime) nats)