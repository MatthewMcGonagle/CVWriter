module Main where

import System.IO
import System.Environment
import CVWriter

main :: IO ()
main = do

    args <- getArgs
    let (directory, filename) = case args of 
                                     [] -> ("", "info.cv")
                                     [name] -> ("", name)
                                     [dir, name] -> (dir, name)
    handle <- openFile (directory ++ filename) ReadMode
    contents <- hGetContents handle
    putStrLn "\nFile loaded.\n"
    let cvinfo = parseCV contents 
        latexString = text . convertCV <$> cvinfo 
        markdownString = jtext . convertCV <$> cvinfo 
    print latexString
    case latexString of (Right str) -> writeFile (directory ++ "MatthewMcGonagleCV.tex") str
                        (Left err) -> return () 
    case markdownString of (Right str) -> writeFile (directory ++ "index.html") str
                           (Left err) -> return () 
