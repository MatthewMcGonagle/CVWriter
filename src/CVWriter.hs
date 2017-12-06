module CVWriter 
( topicItem
, myParse
, myParseTest
, CV
, Topic
, cvToLatex
) where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as Prim
import Text.Parsec.Combinator

data Topic = Topic {title :: String, items :: [String]}
    deriving (Show)

data CV = CV {topics :: [Topic]}
    deriving (Show)

lBracket :: Prim.Parsec [Char] st Char 
lBracket = char '<'

rBracket :: Prim.Parsec [Char] st Char
rBracket = char '>'

lTag :: String -> Prim.Parsec [Char] st String
lTag name = do
       lBracket
       spaces
       string name
       spaces
       rBracket
       return name

rTag :: String -> Prim.Parsec [Char] st String
rTag name = lTag $ "/" ++ name 

topicItem :: Prim.Parsec [Char] st String
topicItem = do
            try (lTag "item")
                <?> "lTag <item>"
            item <- many $ noneOf "<" 
            try (rTag "item")
                <?> "rTag </item>"
            return item 

topicTitle :: Prim.Parsec [Char] st String
topicTitle = do
             try (lTag "title")
                <?> "lTag <title>"
             title <- many $ noneOf "<"
             try (rTag "title")
                 <?> "rTag </title>"
             return title

topic :: Prim.Parsec [Char] st Topic 
topic = do
        try (lTag "topic")
            <?> "lTag <topic>"
        spaces
        title <- topicTitle
        spaces
        rCol <- endBy topicItem spaces
        try (rTag "topic")
            <?> "rTag </topic>"
        return $ Topic {title = title, items = rCol}

topicList :: Prim.Parsec [Char] st [Topic]
topicList = do
            endBy topic spaces

cvfile :: Prim.Parsec String st CV
cvfile = do
         topics <- topicList
         spaces
         eof
         return $ CV {topics = topics}
         
myParse = parse cvfile "source name"
myParseTest = parseTest cvfile 

latexHeader :: String
latexHeader = "\\documentclass{article}\n\n"
    ++ "\\begin{document}\n"

latexEnd :: String
latexEnd = "\\end{document}\n"

cvToLatex :: CV -> String
cvToLatex x = latexHeader ++ body ++ latexEnd
    where body = concat $ map topicToLatex (topics x)

topicToLatex :: Topic -> String
topicToLatex Topic {title = t, items = []} = "\\begin{tabular}{ll}\n" 
    ++ t 
    ++ " & \n\\end{tabular}" 
topicToLatex Topic {title = t, items = i : is} = "\\begin{tabular}{ll}\n"
    ++ t
    ++ " & " ++ i ++ "\\\\\n"
    ++ topicString
    ++ "\\end{tabular}\n\n" 
    where topicString = concat $ map (\x -> " & " ++ x ++ "\\\\\n") is
