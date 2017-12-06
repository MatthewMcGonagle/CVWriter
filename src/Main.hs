module Main where

import System.IO
import CVWriter
import Text.Parsec.Prim
import Text.Parsec.Combinator

main :: IO ()
main = do
    handle <- openFile "info.cv" ReadMode
    contents <- hGetContents handle
    putStrLn "File loaded."
    myParseTest contents
    let latexString = cvToLatex <$> myParse contents
    print latexString
    case latexString of (Right str) -> writeFile "latexcv.tex" str
                        (Left err) -> print err 
    print contents
