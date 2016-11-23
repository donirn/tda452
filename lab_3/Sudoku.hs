module Sudoku where
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Char
import System.Random
import Control.Monad
import Data.List

-------------------------------------------------------------------------
-- Assignment A

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- Function allBlankSudoku: Returns a sudoku with just blanks.
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku {rows = replicate 9 (replicate 9 Nothing)}

-- Function isSudoku: Decides if sud is a valid representation
-- of a sudoku puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud = length (rows sud) == 9
                && and (map (\r -> length r == 9 &&
                                and (map isSudokuElement r)) (rows sud))

-- Function isSudokuElement: Checks if an element is a valid Sudoku
-- cell value.
isSudokuElement :: Maybe Int -> Bool
isSudokuElement Nothing  = True
isSudokuElement (Just e) = e `elem` [1..9]

-- Function isSolved: Checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = and ( map (\r -> and (map (\e -> not (isNothing e)) r )) (rows sud))

-------------------------------------------------------------------------
-- Assignment B

-- Function printSudoku: Prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = mapM_ (\r -> putStrLn (map fromInt r)) (rows sud)

-- Function fromInt: Converts a Maybe Int to a char
fromInt :: Maybe Int -> Char
fromInt Nothing = '.'
fromInt (Just e) = intToDigit e

-- Function readSudoku: Reads a Sudoku from the file, and either delivers it,
-- or stops if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = do file <- readFile filePath
                         let txtRows = lines file
                         let sud = Sudoku {rows = map lineToList txtRows}
                         if (isSudoku sud) then return sud
                         else error "Program error: Not a Sudoku!"

-- Function lineToList: Converts a string into a list useful to represent
-- Sudokus
lineToList :: String -> [Maybe Int]
lineToList line = map fromChar line

-- Function fromChar: Obtains the int value from a char.
fromChar :: Char ->  Maybe Int
fromChar '.' = Nothing
fromChar  c  = Just (digitToInt c)


-------------------------------------------------------------------------
-- Assigment C

-- Function cell: returns generator for Sudoku cell
cell :: Gen (Maybe Int)
cell = frequency [(1, liftM Just (choose (1,9))), (9, return Nothing)]

-- Instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- Function prop_Sudoku: Helper to ease automated checking.
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud
-------------------------------------------------------------------------
-- Assignment D
type Block = [Maybe Int]

-- Function isOkayBlock: given a block, checks if that block does not contain the same digit twice.
isOkayBlock :: Block -> Bool
isOkayBlock blk = length (nubBy (\x y -> x /= Nothing &&  x == y) blk) == 9
                    -- after removing the duplicates the length should be legal

-- Function Blocks: Extract lists representing the cell regions to be considered
-- when deciding over the correctness of a solution. (i.e. rows, columns and 3x3
-- squares)
blocks :: Sudoku -> [Block]
blocks sud = rows sud -- Rows
             ++ transpose (rows sud) -- Columns
             ++ getBlocks (rows sud) -- 3x3 Squares or 'blocks'

-- Function getBlocks: Build 3x3 regions from Sudoku's elements.
getBlocks :: [[Maybe Int]] -> [Block]
getBlocks [] = []
getBlocks rows =  takeFirstBlock rows : getBlocks (dropFirstBlock rows)

-- Function takeFirstBlock: Takes the top-left 3x3 block.
takeFirstBlock :: [[Maybe Int]] -> Block
takeFirstBlock rows = take 3 (rows!!0) ++ take 3 (rows!!1) ++ take 3 (rows!!2)

-- Function dropFirstBlock: Drops the top-left 3x3 block.
dropFirstBlock :: [[Maybe Int]] -> [[Maybe Int]]
dropFirstBlock rows | length (rows!!0) == 3 = drop 3 rows
                    | otherwise             = [drop 3 (rows!!0), drop 3 (rows!!1), drop 3 (rows!!2)]
                                                  ++ drop 3 rows

-- Function isOkay: Checks that all the relevant regions are correct and henceforth
-- the Sudoku is a valid one.
isOkay :: Sudoku -> Bool
isOkay sud = and (map isOkayBlock (blocks sud))
