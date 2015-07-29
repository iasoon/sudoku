import Control.Monad
import Control.Applicative ((<$>))
import System.Environment
import System.IO
import System.Exit (exitFailure)
import Text.Parsec (ParseError(..))

import Sudoku
import IO
import SAT

main = do
    args <- getArgs
    when (null args) $ printUsage >> exitFailure
    solveFromFile (args !! 0) >>= either print printSudoku

printUsage :: IO ()
printUsage = do
    name <- getProgName
    hPutStrLn stderr $ "Usage: " ++ name ++ " sudokufile"

solveFromFile :: String -> IO (Either ParseError Sudoku)
solveFromFile file = fmap (head . evalSAT . solveSudoku) <$> readSudoku file
