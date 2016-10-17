module Euler04 where
import Data.List


isAPalindrome :: Int -> Bool
isAPalindrome x = result
  where 
  listOfDigits = numberSplitIntoDigits x
  result = listOfDigits == reverse listOfDigits
  
numberSplitIntoDigits :: Int -> [Int]
numberSplitIntoDigits 0 = []
numberSplitIntoDigits x = numberSplitIntoDigits (x `div` 10) ++ [x `mod` 10]

largestPalindromeFor3DigitFactors = maximum  [x*y | x <- [100..999], y <- [100..999], isAPalindrome (x*y)]