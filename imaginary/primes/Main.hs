import Fusion

import System.Environment

suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: NuList Int -> NuList Int
the_filter ns0 = thaw $ filterMu (isdivs n) $ freeze ns
  where n = headNu ns0
        ns = tailNu ns0

primes :: NuList Int
primes = fmap headNu (iterateNu the_filter (iterateNu suCC 2))

run x = print $ freeze primes `atMu` x
  
main = do
	[arg] <- getArgs
	run $ read arg
