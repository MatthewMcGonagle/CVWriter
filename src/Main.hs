module Main where

import System.IO
import CVWriter

main :: IO ()
main = do

    handle <- openFile "info.cv" ReadMode
    contents <- hGetContents handle
    putStrLn "\nFile loaded.\n"
    let cvinfo = parseCV contents 
        latexString = text . convertCV <$> cvinfo 
        markdownString = jtext . convertCV <$> cvinfo 
    print latexString
    case latexString of (Right str) -> writeFile "MatthewMcGonagleCV.tex" str
                        (Left err) -> return () 
    case markdownString of (Right str) -> writeFile "index.html" str
                           (Left err) -> return () 
