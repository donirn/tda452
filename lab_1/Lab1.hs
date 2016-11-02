
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 1
-- The program takes k substitutions until it finds the base case.
-- To check this, a subtle modification of the original power function
-- is given below
powerCount :: Integer -> Integer -> Integer
powerCount n k | k < 0 = error "power: negative argument"
powerCount n 0 = 1
powerCount n k = 1 + power n (k-1)

-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k = product [n*(t^0) | t <- [1..k]]

-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k = if even k then power2 (n*n) (div k 2) else n * (power2 n (k-1))
