module PrimeFactors (primeFactors) where
import qualified Data.Numbers.Primes as P

--I've done enough Project Euler to not be super interested in this one...
primeFactors :: Integral a => a -> [a]
primeFactors = P.primeFactors

