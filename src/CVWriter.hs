module CVWriter 
( topicItem
, myParse
, myParseTest
, CV
, Topic
, cvToLatex
, cvToMarkdown
) where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as Prim
import Text.Parsec.Combinator

data Topic =   Topic1 {title :: String, items :: [String]}
             | Topic2 {title :: String, itemPairs :: [(String, [String])]} 
    deriving (Show)

data CV = CV {topics :: [Topic]}
    deriving (Show)

data LatexEnv = Tabular1 | Tabular2 | Section

------------------------------
-- *.cv Text Parsers
------------------------------

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

topicItem :: String -> Prim.Parsec [Char] st String
topicItem category = do
                     let tagString = "item" ++ category
                     try (lTag tagString)
                         <?> "lTag <" ++ tagString ++ ">"
                     item <- many $ noneOf "<" 
                     try (rTag tagString) 
                         <?> "rTag </" ++ tagString ++ ">"
                     return item 

topicTitle :: Prim.Parsec [Char] st String
topicTitle = do
             try (lTag "title")
                 <?> "lTag <title>"
             title <- many $ noneOf "<"
             try (rTag "title")
                 <?> "rTag </title>"
             return title

topic1 :: Prim.Parsec [Char] st Topic 
topic1 = do
         try (lTag "topic1")
             <?> "lTag <topic1>"
         spaces
         title <- topicTitle
         spaces
         rCol <- endBy (topicItem "") spaces
         try (rTag "topic1")
             <?> "rTag </topic1>"
         return $ Topic1 {title = title, items = rCol}

itemPair :: Prim.Parsec [Char] st (String, [String])
itemPair = do
           first <- topicItem "1"
           spaces
           second <- endBy (topicItem "2") spaces
           return (first, second) 

topic2 :: Prim.Parsec [Char] st Topic
topic2 = do
         try (lTag "topic2")
             <?> "lTag <topic2>"
         spaces
         title <- topicTitle
         spaces
         itemPairs <- endBy itemPair spaces
         try (rTag "topic2")
             <?> "rTag </topic2>"
         return $ Topic2 {title = title, itemPairs = itemPairs} 
        
topicList :: Prim.Parsec [Char] st [Topic]
topicList = endBy (try topic1 <|> topic2) spaces 
            
cvfile :: Prim.Parsec String st CV
cvfile = do
         spaces
         topics <- topicList
         spaces
         eof
         return $ CV {topics = topics}
         
myParse = parse cvfile "source name"
myParseTest = parseTest cvfile 

latexHeader :: String
latexHeader = "\\documentclass{article}\n"
    ++ "\\usepackage{booktabs}\n\n"
    ++ "\\begin{document}\n"

latexEnd :: String
latexEnd = "\\end{document}\n"

------------------------------
-- General Formatting Stuff
------------------------------

myTab :: Int -> String
myTab n = concat $ replicate n singleTab
    where singleTab = "    "

------------------------------
-- CV to LaTeX file converters.
------------------------------

cvToLatex :: CV -> String
cvToLatex x = latexHeader 
              ++ beginTex Tabular1
              ++ body 
              ++ "\\end{tabular}\n"
              ++ latexEnd
    where body = concat $ map (\x -> topicToLatex x ++ topicSep) (topics x)

topicSep :: String
topicSep = myTab 1 ++ "\\midrule\n"

tabTex :: String
tabTex = "    "

beginTex :: LatexEnv -> String
beginTex Tabular1 = "\\begin{tabular}{llp{7cm}}\n"
beginTex Tabular2 = beginTex Tabular1 
beginTex _ = "" 

latexBold :: String -> String
latexBold x = "{\\bf " ++ x ++ "} "

itemToLatex :: String -> String
itemToLatex x = " & \\multicolumn{2}{l}{" ++ x ++ "}\\\\\n"

topPairToLatex :: (String, [String]) -> String
topPairToLatex (x, []) = " & " ++ x ++ " & \\\\\n"
topPairToLatex (x, i:is) = " & " ++ x ++ " & " ++ i ++ " \\\\\n"

restPairToLatex :: (String, [String]) -> String
restPairToLatex (x, []) = ""
restPairToLatex (x, i:is) = concat $ map rowToLatex is
    where rowToLatex y = myTab 2 ++ " & " ++ myTab 1 ++ " & " ++ y ++ " \\\\\n" 

topicToLatex :: Topic -> String
topicToLatex Topic1 {title = t, items = []} = 
    myTab 1 ++ latexBold t ++ " & & \\\\\n"
topicToLatex Topic1 {title = t, items = i : is} = 
    myTab 1 ++ latexBold t ++ itemToLatex i 
    ++ otherRows 
    where otherRows = concat $ map (\x -> myTab 2 ++ itemToLatex x) is
topicToLatex Topic2 {title = t, itemPairs = []} = 
    myTab 1 ++ latexBold t ++ " & & \\\\\n"
topicToLatex Topic2 {title = t, itemPairs = i : is} = 
    myTab 1 ++ latexBold t
    ++ topPairToLatex i 
    ++ restPairToLatex i
    ++ otherRows 
    where otherRows = concat $ map (\x -> myTab 2 ++ topPairToLatex x ++ restPairToLatex x) is

------------------------------------
-- CV to Jekyll Markdown Converters.
------------------------------------

cvToMarkdown :: CV -> String
cvToMarkdown x = markdownHeader ++ downloadLink ++ body ++ downloadLink
    where body =    "<table>\n"
                 ++ (concat $ map topicToMarkdown (topics x))
                 ++ "</table>\n"

markdownHeader :: String
markdownHeader =    "---\n"
                 ++ "layout: default\n"
                 ++ "title: Matthew McGonagle's CV\n"
                 ++ "---\n\n"
                 ++ "<h1>{{page . title}}</h1>\n"

emptyColMarkdown :: String
emptyColMarkdown = myTab 2 ++ "<td></td>\n"

downloadLink :: String
downloadLink = 
    " <p> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</p>\n"

itemToMarkdown :: String -> String
itemToMarkdown x = myTab 2 ++ "<td colspan = \"2\">" ++ x ++ "</td>\n"

itemRowToMarkdown :: String -> String
itemRowToMarkdown x = myTab 1 ++ "<tr>\n"
                   ++ emptyColMarkdown
                   ++ itemToMarkdown x
                   ++ myTab 1 ++ "</tr>\n"

topPairToMarkdown :: String -> (String, [String]) -> String
topPairToMarkdown title (x, []) = 
       myTab 2 ++ "<th>" ++ title ++ "</th>\n"
    ++ myTab 2 ++ "<td>" ++ x ++ "</td>\n"
    ++ myTab 2 ++ " <td></td>\n"

topPairToMarkdown title (x, i:is) = 
       myTab 2 ++ "<th>" ++ title ++ "</th>\n"
    ++ myTab 2 ++ "<td>" ++ x ++ "</td>\n"
    ++ myTab 2 ++ "<td>" ++ i ++ "</td>\n" 

restPairToMarkdown :: (String, [String]) -> String
restPairToMarkdown (x, []) = ""
restPairToMarkdown (x, i:is) = concat $ map makeRow is 
    where makeRow i =  myTab 1 ++ "<tr>\n"
                    ++ emptyColMarkdown
                    ++ emptyColMarkdown
                    ++ myTab 2 ++ "<td>" ++ i ++ "</td>\n"
                    ++ myTab 1 ++ "</tr>\n" 
          

topicToMarkdown :: Topic -> String
topicToMarkdown Topic1 {title = t, items = []} =
       myTab 1 ++ "<tr>\n"
    ++ myTab 2 ++ "<th>" ++ t ++ "</th>\n" 
    ++ myTab 1 ++ "</tr>\n"

topicToMarkdown Topic1 {title = t, items =  i : is} = 
       myTab 1 ++ "<tr>\n"
    ++ myTab 2 ++ "<th>" ++ t ++ "</th>\n" 
    ++ itemToMarkdown i
    ++ myTab 1 ++ "</tr>\n"
    ++ otherRows
    where otherRows = concat $ map itemRowToMarkdown is

topicToMarkdown Topic2 {title = t, itemPairs = []} =
       myTab 1 ++ "<tr>\n"
    ++ myTab 2 ++ "<th>" ++ t ++ "</th>\n" 
    ++ myTab 1 ++ "</tr>\n"

topicToMarkdown Topic2 {title = t, itemPairs =  i : is} = 
       myTab 1 ++ "<tr>\n"
    ++ topPairToMarkdown t i
    ++ myTab 1 ++ "</tr>\n"
    ++ restPairToMarkdown i
    ++ otherRows
    where otherRows = concat $ map(\x ->
               myTab 1 ++ "<tr>\n"
            ++ topPairToMarkdown "" x 
            ++ myTab 1 ++ "</tr>\n"
            ++ restPairToMarkdown x
            )
            is
