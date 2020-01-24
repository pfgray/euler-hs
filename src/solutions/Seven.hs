module Seven where
import Primes 

seven = putStrLn $ show $ head $ (drop 10000 primes)