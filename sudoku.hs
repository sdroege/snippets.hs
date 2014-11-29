module Main
    (
      main
    ) where

import Data.List (intercalate, minimumBy, sortBy, foldl')
import Data.List.Split (chunksOf)
import Data.Function (on)
import Data.Char (isSpace, isDigit)

-- An example board, . or x are considered empty fields
boardString :: String
boardString = "5...4.....2.....3....6...1.7.....5.....9....4...1......31..8.......5.7........2.."
-- boardString = "...3..7...6...2.....2...8.36.7.9..3.....6.....8.2..6.4..5..92.....5...6...6.37.1."

-- Value of a field
type Value     = Int

-- Position of a field: (0, 0) to (8, 8)
type Position  = (Int, Int)

-- A field for which we didn't decide a value yet.
-- The list contains all values that would still
-- be possible without conflicting with another
-- already decided field.
type Undecided = (Position, [Value])

-- A field for which we decided a value already
type Decided   = (Position, Value)

-- A board contains a list of the already decided
-- fields and the list of undecided fields with
-- all their possible values
--
-- By construction every instance of Board always
-- has 81 fields and a non-conflicting list of
-- decided fields
newtype Board    = Board ([Decided], [Undecided])

-- A solution contains a list of decided fields
--
-- By construction all instances of Solution are
-- always valid solutions
newtype Solution = Solution [Decided]

-- Parse a board from a string and return the board
-- with all already decided fields and the undecided
-- fields with all their possible values.
parseBoard :: String -> Maybe Board
parseBoard s = if valid then Just board else Nothing
    where
        valid = length cleanedString == 81 && all (\a -> isDigit a || a `elem` ".x") cleanedString && validate decided

        -- Start with an initial board that has no decided
        -- field yet and every field still has all possibilities,
        -- and then decide the value of each already decided field
        -- one by one and remove all now conflicting possibilities
        board = foldl' (flip decideField) initialBoard decided
            where
                initialBoard = Board ([], zip allIdxs (repeat allValues))
                allValues = [1..9]

        -- Parse a list of already decided fields from the string
        -- by first filtering all whitespace from it and then
        -- "annotating" every character in the string with the
        -- position it corresponds to in the board.
        --
        -- Then filter out all x or . as they represent undecided
        -- fields and finally convert all string digits to integers
        --
        -- Format is a string of 81 digits or . or x. The last
        -- two are considered empty fields. Whitespace is ignored.
        -- The string is parsed line by line, i.e. first digit is
        -- at (0, 0), second at (0, 1), ninth at (0, 8), tenth at
        -- (1, 0), etc.
        decided = map conv . filter ((`notElem` ".x") . snd) $
                        zip allIdxs cleanedString
            where
                -- keep position but convert the char digit to an integer
                conv (p, c) = (p, read [c])

        cleanedString = filter (not . isSpace) s

        -- [(0,0),(0,1),..,(1,0),(1,1),..,(8,8)]
        allIdxs = [(x, y) | y <- [0..8], x <- [0..8]]

-- Print a board as a string of 9 lines with 9 digits
--
-- For this first sort all fields by their position so that
-- (0,0) < (0,1) < (1,0) < (1,1) < (8,8). Then drop all the positions,
-- then split this string into chunks of 9 characters and concatenate
-- them with a newline between each of them.
instance Show Board where
    show (Board (ds, us)) = intercalate "\n" . chunksOf 9 . map snd . sortBy comp $ decided ++ undecided
        where
            comp ((x, y), _) ((x', y'), _) | y == y'   = compare x x'
                                           | otherwise = compare y y'

            -- Convert decided fields to a list of positions and char digits
            decided = map (\(p, v) -> (p, head . show $ v)) ds

            -- Convert undecided fields to a list of positions and the char .
            undecided = map (\(p, _) -> (p, '.')) us

-- Print a solution
instance Show Solution where
    show (Solution ds) = show (Board (ds, []))

-- The 3x3 square in which the position parameter is located
square :: Position -> [Position]
square (x, y) = [(x', y') | x' <- [sx..sx+2], y' <- [sy..sy+2]]
    where
        sx = roundDown 3 x
        sy = roundDown 3 y

        roundDown d v = d * (v `div` d)

-- Check if a set of decided fields is valid by checking for each field
-- if it conflicts with any of the other fields.
validate :: [Decided] -> Bool
validate ds = all valid ds
    where
        -- A field is valid if its value does not conflict with
        -- any other field
        valid (p@(x, y), v) = null conflicts
            where
                -- List of all positions that are conflicting with our current
                -- field, i.e. they have the same value, a different position
                -- but are in the same row, column or 3x3 square
                conflicts = [p' | p'@(x', y') <- same, p /= p', x' == x || y' == y || p' `elem` square p]

                -- list of positions that have the same value as our current
                -- field, including the position of our current field
                same = map fst . filter ((== v) . snd) $ ds

-- Decide on a value for one field and return the new list
-- of decided fields and the new list of still undecided
-- fields. The still undecided fields have all the possibilities
-- pruned that would conflict with the newly decided field.
decideField :: Decided -> Board -> Board
decideField d@(p@(x, y), v) (Board (ds, us)) = Board (d : ds, pruned)
    where
        -- prune the values of all still undecided fields
        -- that would conflict with the newly decided field,
        -- and then filter out the newly decided field. It's
        -- not undecided anymore.
        pruned = filter ((/= p) . fst) . map pruneVals $ us

        -- remove now conflicting values from all possiblities of the still
        -- undecided field that are affected by the newly decided field
        pruneVals u@(p'@(x', y'), vs) | x' == x || y' == y || p' `elem` square p = (p', filter (/= v) vs)
                                      | otherwise                                = u

-- Decide which undecided field should be considered next. We choose
-- the one that has the smallest number of possibilities still left
-- to keep the branching in the search tree small.
--
-- Note that this will return an undecided field with no possibilities
-- if there is at least one with no possibilities. This path of the
-- search tree does not lead to a solution then.
nextField :: Board -> Undecided
nextField (Board (_, us)) = minimumBy (compare `on` length . snd) us

-- Based on a list of already decided fields and the still
-- undecided fields with their possible values, return a
-- list of all possible solutions. Each possible solution
-- is a list of 81 decided values.
--
-- To generate a solution we first get the next undecided field
-- we want to consider. Of that field we get all possible values
-- and try them all recursively. Whenever there is no possible
-- value we backtrack, whenever all values are decided we found
-- a solution and return it.
--
-- Note that this uses the list monad to try all possible values.
solutions :: Board -> [Solution]
solutions (Board (ds, [])) = [Solution ds]
solutions b                = do
    let (p, us) = nextField b
    u <- us
    let b' = decideField (p, u) b
    solutions b'

-- Parse the board string and find all possible solutions,
-- then print the first solution (so we only actually generate
-- the first solution!) or say that no solution exists.
-- Only when we count all possible solutions the next solutions
-- are generated
main :: IO ()
main = do
    let results = do
        board <- parseBoard boardString
        let ss = solutions board
        return (board, ss)

    case results of
        Nothing          -> putStrLn "Invalid board"
        Just (board, ss) -> do
            putStrLn $ "Board:\n" ++ show board ++ "\n"

            case ss of
                []    -> putStrLn "No solution exists"
                (s:_) -> do
                    putStrLn $ "Solution:\n" ++ show s ++ "\n"
                    putStrLn $ show (length ss) ++ " possible solutions"

