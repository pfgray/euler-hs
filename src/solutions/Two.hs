module Two where

sumEvenFibs :: (Int, Int, Int) -> Int
sumEvenFibs (prevPrev, prev, acc) =
  let next = prevPrev + prev in
    if next > 4000000
      then acc
      else if next `mod` 2 == 0
        then sumEvenFibs (prev, next, acc + next)
        else sumEvenFibs (prev, next, acc)

two = putStrLn (show (sumEvenFibs (1, 2, 2)))
