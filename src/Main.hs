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
    let cvinfo = myParse contents 
        latexString = text . convertCV' <$> cvinfo 
        markdownString = jtext . convertCV' <$> cvinfo 
    print latexString
    case latexString of (Right str) -> writeFile "MatthewMcGonagleCV.tex" str
                        (Left err) -> do print "ERROR for cvinfo"
                                         print err 
    case markdownString of (Right str) -> writeFile "index.html" str
                           (Left err) -> do print "ERROR for cvinfo" 
                                            print err
    print cvinfo 
