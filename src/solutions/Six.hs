module Six where
nats = 1 : map (+1) nats

sumOfSquares :: Num t => [t] -> t -> t
sumOfSquares [] acc = acc
sumOfSquares (n:ns) acc = sumOfSquares ns ((n*n) + acc ) 

squareOfSum [] acc = acc^2
squareOfSum (n:ns) acc = squareOfSum ns (n + acc)

hun = take 100 nats
ten = take 10 nats

six = putStrLn $ show $ ((squareOfSum hun 0) - (sumOfSquares hun 0))