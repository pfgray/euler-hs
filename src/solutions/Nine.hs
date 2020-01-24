module Nine where

orElse :: Maybe a -> Maybe a -> Maybe a
orElse a b = case a of
  Just _ -> a
  Nothing -> b


isPythagoreanTriplet :: Int -> Int -> Int -> Bool
isPythagoreanTriplet a b c = a * a + b * b == c * c

findTriplet :: Maybe (Int, Int, Int, Int)
findTriplet = findA 0 
  -- let a = 0
  -- start a at 0, continue if Nothing

findA :: Int -> Maybe (Int, Int, Int, Int)
findA a 
  | a == 1000 = Nothing 
  | otherwise = (findB a (a + 1)) `orElse` (findA (a + 1))

findB a b 
  | b == 1000 = Nothing
  | otherwise = (findC a b) `orElse` (findB a (b + 1))

findC :: Int -> Int -> Maybe (Int, Int, Int, Int)
findC a b = 
  let c = 1000 - a - b in
    if c <= b then Nothing
    else if isPythagoreanTriplet a b c then Just(a, b, c, a*b*c)
    else Nothing

nine = putStrLn $ show $ findTriplet