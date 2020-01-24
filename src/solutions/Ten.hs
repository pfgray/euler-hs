module Ten where
import Primes

ten = putStrLn $ show $ foldl (+) 0 (takeWhile (<2000000) primes)

