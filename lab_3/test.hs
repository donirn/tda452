module Sudoku where
import Data.Maybe
import Test.QuickCheck
import Data.Char
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
                         l <- lines file


-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------
