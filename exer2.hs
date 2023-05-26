divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]
primes :: Int -> [Int]
primes n = [i | i <- [2..n], divisors i == []]

pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [(x, y, z) | x <- [1..n], y <-[1..n], z <- [1..n], x < y && x ^ 2 + y ^ 2 == z ^ 2]

join :: String -> [String] -> String
join _ [] = ""
join x (y: ys)
  | ys == [] = y
  | otherwise = y ++ x ++ join x ys

fact' :: Int -> Int
fact' n = foldl (*) 1 [1..n]

hailLen :: Int -> Int
hailLen n = hailTail 0 n

hailTail :: Int -> Int -> Int
hailTail a n 
  | n == 1 = a
  | odd n = hailTail (a + 1) (3 * n + 1)
  | even n = hailTail (a + 1) (div n 2)





