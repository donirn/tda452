module Sudoku where
import           Control.Monad
import           Data.Char
--import           Data.Generics.Aliases
import           Data.List
import           Data.Maybe
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Property

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
isSolved sud = and ( map (\r -> and (map isJust r )) (rows sud))

-------------------------------------------------------------------------
-- Assignment B

-- Function printSudoku: Prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = mapM_ (\r -> putStrLn (map fromInt r)) (rows sud)

-- Function fromInt: Converts a Maybe Int to a char
fromInt :: Maybe Int -> Char
fromInt Nothing  = '.'
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

-- Function isOkayBlock: given a block, checks if that block does not contain
-- the same digit twice.
isOkayBlock :: Block -> Bool
isOkayBlock blk =
              length (nubBy (\x y -> x /= Nothing &&  x == y) blk) == 9
              -- after removing the duplicates the length should be legal

-- Function Blocks: Extract lists representing the cell regions to be
-- considered when deciding over the correctness of a solution. (i.e. rows,
-- columns and 3x3 squares)
blocks :: Sudoku -> [Block]
blocks sud = rows sud -- Rows
             ++ transpose (rows sud) -- Columns
             ++ get3x3Blocks (rows sud) -- 3x3 Squares or 'blocks'

-- Function get3x3Blocks: Build 3x3 regions from Sudoku's elements.
get3x3Blocks :: [[Maybe Int]] -> [Block]
get3x3Blocks []   = []
get3x3Blocks rows =  takeFirstBlock rows :
                        get3x3Blocks (dropFirstBlock rows)

-- Function takeFirstBlock: Takes the top-left 3x3 block.
takeFirstBlock :: [[Maybe Int]] -> Block
takeFirstBlock rows = concat (map (take 3) (take 3 rows))

-- Function dropFirstBlock: Drops the top-left 3x3 block.
dropFirstBlock :: [[Maybe Int]] -> [[Maybe Int]]
dropFirstBlock rows = filter (\x -> length x > 0)
                              (map (drop 3) (take 3 rows))
                      ++ drop 3 rows

-- Function isOkay: Checks that all the relevant regions are correct and
-- henceforth the Sudoku is a valid one.
isOkay :: Sudoku -> Bool
isOkay sud = and (map isOkayBlock (blocks sud))

-------------------------------------------------------------------------
-- Assignment E
type Pos = (Int,Int)

blanks :: Sudoku -> [Pos]
blanks sud =  concat (zipWith (\r c -> [(r, k) |k <- c])
                              [0..8]
                              (map (\r -> elemIndices Nothing r) (rows sud)))

-- Checks that that all cells in the blanks list are actually blank.
prop_allCellsAreBlank :: Sudoku -> Bool
prop_allCellsAreBlank sud = and (map (\(x,y) ->
                              (rows sud)!!x!!y == Nothing) (blanks sud))

-- Function (!!=) : Given a list, and a tuple containing an index in the list
-- and a new value, updates the given list with the new value at the given
-- index.
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,v) = case (splitAt i xs) of
  (l1,y:ys) -> l1 ++ (v:xs)

-- TODO: fix the prop_replace check length and the rest of element
-- Function prop_replace : Checks if the element is actually updated
prop_replace :: (Eq a) => [a] -> (Int,a) -> Bool
prop_replace [] (_,_) = True
prop_replace l (i,v) | (0 <= i && i < length(l)) = updatedList!!i == v
                                                   && length(updatedList) == length(l)
                                                   && h1 == h2
                                                   && t1 == t2
                     | otherwise                  = True
                      where updatedList = (l !!= (i,v))
                            (h1,_:t1) = splitAt i l
                            (h2,_:t2) = splitAt i updatedList

-- Function update: Given a Sudoku, a position, and a new cell value, updates
-- the given Sudoku at the given position with the new value.
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (rowInd,colInd) newVal =
  Sudoku { rows = ((rows sud) !!= (rowInd, newRow)) }
    where newRow = ((rows sud)!!rowInd) !!= (colInd, newVal)

-- Function prop_update : checks that the updated position really has gotten 
-- the new value.
prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update _ pos _ | not (inSudoku pos) = True
prop_update sud (row,col) val = (rows (update sud (row,col) val))!!row!!col == val

inSudoku :: Pos -> Bool
inSudoku (row, col) | row < 0  || col < 0  = False
                    | row >= 9 || col >= 9 = False
                    | otherwise            = True

-- Function candidates : Given a Sudoku, and a blank position, determines
-- which numbers could be legally written into that position.
candidates :: Sudoku -> Pos -> [Int]
candidates sud (rowInd, colInd) = [1..9] \\ catMaybes relatedBlocks
  where relatedBlocks = allBlocks!!rowInd ++ allBlocks!!(9 + colInd)
                        ++ allBlocks!!(18 + (rowInd `div` 3)*3 +
                                        (colInd `div` 3))
        allBlocks = blocks sud

-- Function prop_candidates : Check that updating with given candidates are
-- actually okay based on the check on isSudoku and isOkay
prop_candidates :: Sudoku -> Pos -> Bool
prop_candidates _ pos | not (inSudoku pos) = True
prop_candidates sud _ | not (isOkay sud && isSudoku sud) = True
prop_candidates sud pos = and(map checkCandidate (candidates sud pos))
    where checkCandidate c = isOkay (sud' c) && isSudoku (sud' c)
          sud' c = update sud pos (Just c)

-------------------------------------------------------------------------
-- Assignment F

-- Function solve: Solves a Sudoku. Return Nothing if the Sudoku does not
-- have a solution.
solve :: Sudoku -> Maybe Sudoku
solve sud | not( isSudoku sud && isOkay sud)  = Nothing
          | otherwise                         = solve' sud pos (candidates sud pos)
                                                  where pos = (head (blanks sud))

-- Function solve: Solves a Sudoku.
solve' :: Sudoku -> Pos -> [Int] -> Maybe Sudoku
solve' _ _ [] = Nothing
solve' sud pos (x:xs)
    | isSolved sud' = Just sud'
    | otherwise     = solve' sud' pos' (candidates sud' pos')
                      `orElse` solve' sud pos xs
    where sud' = update sud pos (Just x)
          pos' = head (blanks sud')

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse _        y = y

-- Function readAndSolve: Produces instructions for reading the Sudoku from
-- the given file, solving it, and printing the answer.
readAndSolve :: FilePath -> IO ()
readAndSolve filePath = do sud <- readSudoku filePath
                           let solution = solve sud
                           if isJust solution then
                             printSudoku (fromJust solution)
                           else putStrLn "(no solution)"


-- Function isSolutionOf: Checks, given two Sudokus, whether the first one is
-- a solution (i.e. all blocks are okay, there are no blanks), and also
-- whether the first one is a solution of the second one (i.e. all digits in
-- the second sudoku are maintained in the first one).
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf first second = isSolved first && isOkay first && checkSol
    where checkSol = and (zipWith (\fr sr ->
                        and (zipWith (\p q -> (p == q) || (q == Nothing)) fr sr))
                        (rows first)
                        (rows second))

-- prop_SolveSound: Tests that every supposed solution produced by solve
-- actually is a valid solution of the original problem.
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud | not (isOkay sud && isSudoku sud) = property True
prop_SolveSound sud | isNothing solvedSud = property True
                    | otherwise = property (fromJust solvedSud `isSolutionOf` sud)
                    where solvedSud = solve sud

-- use fewer tests when using quickCheck
fewerChecks :: Testable prop => prop -> IO ()
fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 30 } prop
