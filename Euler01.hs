module Euler01 where
import Data.List

calculateSumOfAllMultiplesOf3Or5 :: Int -> Int -> Int -> Int
calculateSumOfAllMultiplesOf3Or5 start end stepSize = foldl1 (+) z
  where
  y = createAListOfNumbers start end stepSize
  z = filter multipleOf3Or5 y
  
  
createAListOfNumbers :: Int -> Int -> Int -> [Int]
createAListOfNumbers start end stepSize = [start,start+stepSize..end]

multipleOf3Or5 :: Int -> Bool
multipleOf3Or5 input = mod input 3 == 0 || mod input 5 == 0
 



