module Fourteen where

import Numbers

collat n = [n, 1]

mapWith f = map (\a -> (a, f(a)))

maxOfB (a1, b1) (a2, b2) = if length b1 > length b2 then (a1, b1) else (a2, b2)

largestCollatz (len, num) n =
  let maybeLen = collatz 0 n in
        if maybeLen > len then (maybeLen, n)
        else (len, num)

fourteen = putStrLn $ show $ (foldl largestCollatz (0, 0) [0..999999])


