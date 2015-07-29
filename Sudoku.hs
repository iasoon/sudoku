module Sudoku
    ( PseudoSudoku, Sudoku, SudokuVar
    , solveSudoku
    , sudokuToList, sudokuFromList
    ) where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Control.Monad
import Data.Maybe (catMaybes)

import qualified FD
import SAT

type Sudoku         = PseudoSudoku Int

type SudokuVar      = PseudoSudoku (FD.Var Int)
type PseudoSudoku a = M.Map (Int, Int) a

--------------------------------------------------------------------------------
-- Conversions

sudokuFromList :: [[Maybe a]] -> PseudoSudoku a
sudokuFromList = M.fromList . catMaybes . zipWith zipMaybe indices . concat
    where zipMaybe a = fmap ((,) a)

sudokuToList :: PseudoSudoku a -> [[Maybe a]]
sudokuToList s = partsOf 9 $ map (flip M.lookup s) indices

partsOf n = takeWhile (not . null) . map (take n) . iterate (drop n)

--------------------------------------------------------------------------------
-- Coordinate helpers

indices :: [(Int, Int)]
indices = [(r,c) | r <- [1..9], c <- [1..9]]

row :: Int -> [(Int, Int)]
row r = [(r, c) | c <- [1..9]]

rows :: [[(Int, Int)]]
rows = map row [1..9]

column :: Int -> [(Int, Int)]
column c = [(r, c) | r <- [1..9]]

columns :: [[(Int, Int)]]
columns = map column [1..9]

square :: Int -> Int -> [(Int, Int)]
square y x = [(r,c) | r <- [(3*y-2)..(3*y)], c <- [(3*x-2)..(3*x)]]

squares :: [[(Int, Int)]]
squares = map (uncurry square) $ square 1 1

getElems :: PseudoSudoku a -> [(Int, Int)] -> [a]
getElems s = catMaybes . map (flip M.lookup s)

--------------------------------------------------------------------------------
--

getSudoku :: SudokuVar -> SAT Sudoku
getSudoku = fmap (M.fromList . catMaybes) . mapM getValue . M.assocs
    where getValue (ind, var) = FD.getValue var >>= return . fmap ((,) ind)

emptySudoku :: SAT SudokuVar
emptySudoku = do
    -- allocate vars
    vars <- replicateM (9*9) (FD.newVar [1..9])
    let sudoku = sudokuFromList . partsOf 9 . map Just $ vars
    -- apply constraints
    mapM_ (formula . FD.allDifferent) . map (getElems sudoku) $
        columns ++ rows ++ squares
    return sudoku

determinePair :: SudokuVar -> (Int, Int) -> Int -> SAT ()
determinePair var ind val = whenJust formula $ equality
    where equality = M.lookup ind var >>= FD.equals val
          whenJust = maybe (return ())

determineVars :: SudokuVar -> Sudoku -> SAT ()
determineVars var s = mapM_ (uncurry $ determinePair var) $ M.assocs s

solveSudoku :: Sudoku -> SAT Sudoku
solveSudoku vals = do
    var <- emptySudoku
    determineVars var vals
    solveSAT
    getSudoku var
