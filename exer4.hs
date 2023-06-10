import Data.Maybe (Maybe(..))

twoToupleSum :: (Int, Int) -> Int
twoToupleSum (a, b) = a + b

pascal :: Int -> [Int]
pascal n
  | n == 0 = [1]
  | otherwise = [1] ++ (map twoToupleSum $ zip prev $ tail prev) ++ [1]
  where 
    prev = pascal (n - 1)

addPair :: (Int, Int) -> Int 
addPair = uncurry (+)


withoutZeros :: (Eq a, Num a) => [a] -> [a]
withoutZeros = filter (/= 0)


findElt:: Eq a => a -> [a] -> Maybe Int
findElt _ [] = Nothing
findElt n (x:xs) = findEltHelper n (x:xs) 0
  where 
    findEltHelper _ [] _ = Nothing
    findEltHelper n (x:xs) i 
      | n == x = Just i
      | otherwise = findEltHelper n xs (i + 1)
  

    
