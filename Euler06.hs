module Euler06 where
  
sumOfTheSquares :: [Int] -> Int
sumOfTheSquares range = result
  where
  result = sum $ [n^2|n<-range] 
  
squareOfTheSum :: [Int] -> Int
squareOfTheSum range = result
  where
  result = (sum $ [n|n<-range])^2 
  
difference :: [Int] -> Int
difference range = result
  where
  result =  subtract (sumOfTheSquares range) $ squareOfTheSum range
