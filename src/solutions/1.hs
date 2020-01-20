sumMultiples :: (Int, Int) -> Int
sumMultiples (0, acc) = acc
sumMultiples (n, acc) = 
  if mod n 3 == 0 || mod n 5 == 0
    then sumMultiples (n - 1, n + acc)
    else sumMultiples (n - 1, acc)

main = putStrLn (show (sumMultiples (999, 0)))
