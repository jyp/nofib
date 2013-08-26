import Fusion

import System.Environment

suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: MuList Int -> MuList Int
the_filter ns = filterMu (isdivs n) ns
  where n = headMu ns

primes :: NuList Int
primes = fmap headMu (iterateNu the_filter (freeze $ iterateNu suCC 2))

run x = print $ freeze primes `atMu` x
  
main = do
	[arg] <- getArgs
	run $ read arg
