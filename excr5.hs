import System.IO
import Data.Ratio (Ratio, (%))

myIterate :: (a ->  a) -> a -> [a]
myIterate f e = e : (myIterate f (f e))

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0 x = ([], x)
mySplitAt n [] = ([], [])
mySplitAt n (x:xs) = (x: ys, zs)
  where (ys, zs) = mySplitAt (n - 1) xs

rationalSum :: Int -> [Ratio Int]
rationalSum n = [x % y | x <- [1..n - 1], y <- [1..n - 1], x + y == n]

rationalSumLowest :: Int -> [Ratio Int]
rationalSumLowest n = [x % y | x <- [1..n - 1], y <- [1..n - 1], x + y == n && (gcd x y) == 1]

rationals :: [Ratio Int]
rationals = rationalsHelper 2
  where rationalsHelper n = (rationalSumLowest n) ++ (rationalsHelper (n + 1))


sumFile :: IO ()
sumFile = do
  contents <- readFile "input.txt"
  let numbers = map readInt (splitAtSeparator '\n' contents)
      result = sum numbers
  putStrLn (show result)
  

-- split a list around a given separator value
splitAtSeparator :: Eq a => a -> [a] -> [[a]]
splitAtSeparator sep [] = []
splitAtSeparator sep content = first : splitAtSeparator sep rest
    where
    first = takeWhile (/= sep) content
    firstlen = length first
    rest = drop (firstlen+1) content

-- convert an integer-like string to an integer
readInt :: String -> Int
readInt = read
