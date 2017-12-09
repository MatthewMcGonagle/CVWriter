module Main where

import System.IO
import CVWriter
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec

main :: IO ()
main = do

    handle <- openFile "info.cv" ReadMode
    contents <- hGetContents handle
    putStrLn "\nFile loaded."
    let latexString = cvToLatex <$> myParse contents
    print latexString
    case latexString of (Right str) -> writeFile "latexcv.tex" str
                        (Left err) -> do print "ERROR"
                                         print err 
    print contents
