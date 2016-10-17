module Euler02 where
import Data.List



f :: Int -> Int
f 0 = 1
f 1 = 1
f x = f (x-1) + f (x-2)


cond :: Int -> Bool
cond n = mod n 2 == 0

series :: Int -> Int
series n = sum $ filter cond $ map f [0..n]


   
  

  
  
  
  