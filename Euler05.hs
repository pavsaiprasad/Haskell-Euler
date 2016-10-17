module Euler05 where
  
smallestPositiveInteger :: [Int] -> Int
smallestPositiveInteger range = foldl1 lcm $ range