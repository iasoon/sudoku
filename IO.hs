module IO
    ( showSudoku, printSudoku
    , parseSudoku, readSudoku
    ) where

import Text.Parsec
import Data.List (intersperse)
import System.IO
import Control.Applicative ((<$>))
import qualified Data.Map as M

import Sudoku


parseSudoku :: String -> String -> Either ParseError Sudoku
parseSudoku source str = sudokuFromList <$> parse sudoku source str

readSudoku :: String -> IO (Either ParseError Sudoku)
readSudoku file = parseSudoku file <$> readFile file

sudoku = count 9 sudokuLine

sudokuLine = many (try garbageLine) >> count 9 sudokuSpace

garbageLine = sudokuGarbage >> endOfLine

sudokuSpace = sudokuGarbage >> (sudokuDigit <|> sudokuEmpty)

sudokuEmpty = oneOf empty >> return Nothing

sudokuDigit = do
    a <- oneOf digits
    return . Just . read $ [a]

sudokuGarbage = many . noneOf $ digits ++ empty ++ "\r\n"

digits = ['1'..'9']
empty  = [' ']

--------------------------------------------------------------------------------
-- Printing
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . showSudoku

showSudoku :: Sudoku -> String
showSudoku = unlines . pack 3 lineSep . map showLine . sudokuToList
    where lineSep = "+---+---+---+"
          showLine  = concat . pack 3 "|" . map showSpace
          showSpace = maybe " " show
          pack n sep = (sep:) . (++[sep]) . separate n sep
          separate n sep = concat . intersperse [sep] . partsOf n
          partsOf n = takeWhile (not . null) . map (take n) . iterate (drop n)
