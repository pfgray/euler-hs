

nats = 20 : map (+20) nats

divisors = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

isDivisible :: [Integer] -> Integer -> Bool
isDivisible [] n = True
isDivisible (d:ds) n
  | n `mod` d == 0 = isDivisible ds n
  | otherwise = False


main = putStrLn $ show $ head $ filter (isDivisible divisors) nats


