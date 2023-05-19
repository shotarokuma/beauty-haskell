det a b c = b^2 - 4*a*c
quadsol1 a b c = (-b - sqrt (det a b c))/2*a
quadsol2 a b c = (-b + sqrt (det a b c))/2*a

third_a list = list !! 2
third_b (_:_:res:_) = res

fact 0 = 1
fact x = x * fact next
  where next = x - 1

hailstone n
  | odd n = 3 * n + 1
  | even n = div n 2

hailLen n
  | n == 1 = 0
  | odd n = 1 + hailLen (3 * n + 1)
  | even n = 1 + hailLen (div n 2)
