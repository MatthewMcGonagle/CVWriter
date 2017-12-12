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
             | Topic2 {title :: String, itemPairs :: [(String, String)]} 
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

topic1 :: Prim.Parsec [Char] st Topic 
topic1 = do
         try (lTag "topic1")
             <?> "lTag <topic1>"
         spaces
         title <- topicTitle
         spaces
         rCol <- endBy topicItem spaces
         try (rTag "topic1")
             <?> "rTag </topic1>"
         return $ Topic1 {title = title, items = rCol}

topic2 :: Prim.Parsec [Char] st Topic
topic2 = do
         try (lTag "topic2")
             <?> "lTag <topic2>"
         spaces
         title <- topicTitle
         spaces
         let itemPair = do
                        left <- topicItem
                        spaces
                        right <- topicItem
                        return (left, right)
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
topicSep = "\\midrule"

beginTex :: LatexEnv -> String
beginTex Tabular1 = "\\begin{tabular}{lll}\n"
beginTex Tabular2 = beginTex Tabular1 
beginTex _ = "" 

latexBold :: String -> String
latexBold x = "{\\bf " ++ x ++ "} "

topicToLatex :: Topic -> String
topicToLatex Topic1 {title = t, items = []} = 
    latexBold t 
    ++ " & & \\\\\n"
topicToLatex Topic1 {title = t, items = i : is} = 
    latexBold t
    ++ " & \\multicolumn{2}{l}{" ++ i ++ "}\\\\\n"
    ++ otherRows 
    where otherRows = concat $ map (\x -> " & \\multicolumn{2}{l}{" ++ x ++ "}\\\\\n") is
topicToLatex Topic2 {title = t, itemPairs = []} = 
    latexBold t 
    ++ " & & \\\\\n"
topicToLatex Topic2 {title = t, itemPairs = i : is} = 
    latexBold t
    ++ " & " ++ fst i  ++ " & " ++ snd i ++ "\\\\\n"
    ++ otherRows 
    where otherRows = concat $ map (\x -> " & " ++ fst x  ++ " & " ++ snd x ++ "\\\\\n") is

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

downloadLink :: String
downloadLink = 
    " <p> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</p>\n"

topicToMarkdown :: Topic -> String
topicToMarkdown Topic1 {title = t, items = []} =
       "    <tr>\n"
    ++ "        <th>" ++ t ++ "</th>\n" 
    ++ "    </tr>\n"

topicToMarkdown Topic1 {title = t, items =  i : is} = 
       "    <tr>\n"
    ++ "        <th>" ++ t ++ "</th>\n" 
    ++ "        <td colspan = \"2\">" ++ i ++ "</td>\n"
    ++ "    </tr>\n"
    ++ otherRows
    where otherRows = concat $ map(\x ->
               "   <tr>\n"
            ++ "        <td></td>\n"
            ++ "        <td colspan = \"2\">" ++ x ++ "</td>\n" 
            ++ "   </tr>\n"
            )
            is
topicToMarkdown Topic2 {title = t, itemPairs = []} =
       "    <tr>\n"
    ++ "        <th>" ++ t ++ "</th>\n" 
    ++ "    </tr>\n"

topicToMarkdown Topic2 {title = t, itemPairs =  i : is} = 
       "    <tr>\n"
    ++ "        <th>" ++ t ++ "</th>\n" 
    ++ "        <td>" ++ fst i ++ "</td>\n"
    ++ "        <td>" ++ snd i ++ "<td>\n"
    ++ "    </tr>\n"
    ++ otherRows
    where otherRows = concat $ map(\x ->
               "   <tr>\n"
            ++ "        <td></td>\n"
            ++ "        <td>" ++ fst x ++ "</td>\n" 
            ++ "        <td>" ++ snd x ++ "</td>\n"
            ++ "   </tr>\n"
            )
            is

    

