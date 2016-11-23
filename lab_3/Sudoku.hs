module Sudoku where
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Char
import System.Random
import Control.Monad
import Data.List
-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku {rows = replicate 9 (replicate 9 Nothing)}
sudokuTest = Sudoku {rows = replicate 9 (replicate 9 (Just 7))}

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud = length (rows sud) == 9
                && and (map (\r -> length r == 9 &&
                                and (map isSudokuElement r)) (rows sud))

isSudokuElement :: Maybe Int -> Bool
isSudokuElement Nothing  = True
isSudokuElement (Just e) = e `elem` [1..9]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = and ( map (\r -> and (map (\e -> not (isNothing e)) r )) (rows sud))

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = mapM_ (\r -> putStrLn (map getValue r)) (rows sud)

getValue :: Maybe Int -> Char
getValue Nothing = '.'
getValue (Just e) = intToDigit e

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = do file <- readFile filePath
                         let txtRows = lines file
                         let sud = Sudoku {rows = map lineToList txtRows}
                         if (isSudoku sud) then return sud
                         else error "Program error: Not a Sudoku!"

lineToList :: String -> [Maybe Int]
lineToList line = map getIntValue line

getIntValue :: Char ->  Maybe Int
getIntValue '.' = Nothing
getIntValue  c  = Just (digitToInt c)


-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(1, liftM Just (choose (1,9))), (9, return Nothing)]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud
-------------------------------------------------------------------------
-- given a block, checks if that block does not contain the same digit twice.

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock blk = length (nubBy (\x y -> x /= Nothing &&  x == y) blk) == 9

blockCorrectInstance :: Block
blockCorrectInstance = [Just 1, Just 2, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 4]

blockIncorrectInstance :: Block
blockIncorrectInstance = [Just 1, Just 1, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 4]
