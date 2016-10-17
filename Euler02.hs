module Euler02 where
import Data.List



generateFibonacci :: Int -> Int
generateFibonacci 0 = 1
generateFibonacci 1 = 1
generateFibonacci x = generateFibonacci (x-1) + generateFibonacci (x-2)


getEvenNumbers :: Int -> Bool
getEvenNumbers n = mod n 2 == 0

sumOfEvenValuedTerms :: Int -> Int
sumOfEvenValuedTerms n = sum $ filter getEvenNumbers $ map generateFibonacci [0..n]


   
  

  
  
  
  
