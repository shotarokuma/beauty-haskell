import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort a
  | len == 0 = a
  | len == 1 = a
  | otherwise  = merge (mergeSort l) (mergeSort r)
  where 
    len = length a 
    (l, r) = splitAt (len  `div` 2 ) a

daysInYear :: Integer -> [Day]
daysInYear y = [jan1..dec31]
  where jan1 = fromGregorian y 1 1
        dec31 = fromGregorian y 12 31

isFriday :: Day ->  Bool
isFriday d = x == 5
  where ( _, x ) = mondayStartWeek d

divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]
primes :: Int -> [Int]
primes n = [i | i <- [2..n], divisors i == []]

isPrimeDay :: Day ->  Bool
isPrimeDay d = divisors day == []
  where ( _, _, day) = toGregorian d


primeFridays :: Integer ->  [Day]
primeFridays y = filter (\d -> isFriday d && isPrimeDay d) ds
  where ds = daysInYear y


