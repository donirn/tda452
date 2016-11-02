import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k | k < 0 = 0 --error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 1
-- The program takes k substitutions until it finds the base case.
-- To check this, a subtle modification of the original power function
-- is given below
powerCount :: Integer -> Integer -> Integer
powerCount n k | k < 0 = 0 --error "power: negative argument"
powerCount n 0 = 1
powerCount n k = 1 + power n (k-1)

-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = 0 --error "power: negative argument"
power1 n k = product [n | t <- [1..k]]

-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = 0 --error "power: negative argument"
power2 n 0 = 1
power2 n k = if even k
              then power2 (n*n) (div k 2)
              else n * (power2 n (k-1))

-- Part 4
-- A. A test on the behavior when the powers are even/odd
-- since power2's implementation use specifically a trick based on the
-- exponent number properties. The different lists can be generated using
-- comprehension.
--
-- B. prop_powers implementation. The case on k < than 0 had to be treated
-- as true since the function '^' throws an error if that condition is not
-- fulfilled.
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = if k>=0 then (n^k == power1 n k) &&
                          (n^k == power2 n k) &&
                          (n^k == power n k)
                          else True

-- C. By executing an 'and' fold on the list of boolean results is possible
-- to check if any of the test cases is failing since prop_powers returns
-- false for such scenario and the logical function requires all its operands
-- to be True to return True as its result.

and [prop_powers n k |  n <-  [r | r <-[-100..100]],
                        k <-  [r | r <-[0..100], even r ]++
                              [r | r <-[0..100] , odd r ]]

-- D. Running quickCheck effectively shows that 100 tests pass.
