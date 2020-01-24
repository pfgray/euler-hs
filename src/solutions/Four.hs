module Four where

nums = [100..999]

isPalindrome [] = True
isPalindrome [n] = True
isPalindrome (n:ns)
  | n == last ns = isPalindrome $ init ns
  | otherwise = False

largestPdromeProd :: ([Int], Maybe Int) -> Maybe Int
largestPdromeProd ([], biggest) = biggest
largestPdromeProd ((a:as), biggest) =
  let big = largestPdromeProdIn (a, nums, biggest) in
    largestPdromeProd (as, big `max` biggest)

largestPdromeProdIn :: (Int, [Int], Maybe Int) -> Maybe Int
largestPdromeProdIn (a, [], biggest) = biggest
largestPdromeProdIn (a, (b:bs), biggest) = let big = a * b in
  if isPalindrome $ show $ big
    then Just big `max` biggest 
    else largestPdromeProdIn (a, bs, biggest)

four = putStrLn $ show $ largestPdromeProd (nums, Nothing)
