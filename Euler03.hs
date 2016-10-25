module Euler03 where

import Data.Time.Clock 
    
isPrime :: Integer -> Bool
isPrime n
    | n == 1 = True
    | n == 2 = True
    | mod n 2 == 0 = False
    | null [x | x <- [3,5..getSmallestFactor n] , mod n x == 0, x <= round (sqrt (fromInteger n))] = True
    | otherwise = False

 
isPrimeFactor :: Integer -> Integer -> Bool
isPrimeFactor n x = and [isPrime x, mod n x == 0]


getSmallestFactor :: Integer -> Integer
getSmallestFactor n = until (\x -> mod n x == 0) (\x -> x + 1) 2


getLargestFactor :: Integer -> Integer
getLargestFactor n = until (\x -> mod n x == 0) (\x -> x - 1) $ n - 1


getLargestPrimeFactor :: Integer -> Integer
getLargestPrimeFactor n = if idx  == 0 then n else idx
  where
  start    = if mod n 2 == 0 then div n 2 else div n 2 + 1 	
  idx = until (\x -> or[isPrimeFactor n x, x < 2]) (\x -> x -1 )  start

getLargestPrimeFactor2 :: Integer -> Integer
getLargestPrimeFactor2 n = if idx  == 0 then n else ns !! idx
  where
    ns = primesTill (div n 2 )
    idx = until (\x -> or[mod n (ns!!x)== 0, x < 0]) 
                            (\x -> x - 1) $ length ns - 1
   
primesTill :: Integer -> [Integer]
primesTill n = filter isPrime [2..n]


largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = if null fs then n else maximum fs
  where
    ns = primesTill n
    fs = [x | x <- ns, mod n x == 0]

       
test :: Integer -> IO Int
test n = do
   t1 <- getCurrentTime 
   let f = primesTill n
   t2 <- getCurrentTime 
   let dt =  show $ diffUTCTime t2 t1
   return $ length f

   
main :: IO()
main = do
  r <- test 600851475143
  putStrLn $ show r 
   


  
  

  
 


  
  

  
 
