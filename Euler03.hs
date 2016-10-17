module Euler03 where
import Data.List hiding (foldl1)

--Incomplete. Need to work out the best way to sieve. 
  
multiplesOfANumber :: Int -> Int -> [Int]
multiplesOfANumber inputNumber maxNumber = result
  where
  result = [n|n<-[inputNumber,inputNumber*2..maxNumber]]
  
filterListFromAnotherList :: [Int]->[Int]->[Int]
filterListFromAnotherList originalList listOfNumbersToBeEliminated = result
  where
  result = originalList \\ listOfNumbersToBeEliminated
  
eliminateIndivisibleNumbersFromTheList :: Int -> Int -> [Int]
eliminateIndivisibleNumbersFromTheList maxNumber k = result
  where
  result = filterListFromAnotherList [1..maxNumber] (multiplesOfANumber k maxNumber) 
  
getPrimeFactors :: Int -> [Int]
getPrimeFactors input = list2
  where
  list = [1..input]
  list2 = eliminateIndivisibleNumbersFromTheList input 2
  list3 = eliminateIndivisibleNumbersFromTheList (last list2) 3


  
  

  
 