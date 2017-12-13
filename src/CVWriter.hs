module CVWriter 
( topicItem
, myParse
, myParseTest
, CV
, Topic
, Output (Latex, Jekyll)
, convertCV
) where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as Prim
import Text.Parsec.Combinator

data ItemPiece = JustText String | Italic String
    deriving (Show)

type Item= [ItemPiece]

data Topic =   Topic1 {title :: String, items :: [Item]}
             | Topic2 {title :: String, itemPairs :: [(Item, [Item])]} 
    deriving (Show)

data CV = CV {topics :: [Topic]}
    deriving (Show)

data LatexEnv = Tabular1 | Tabular2 | Section

data Output = Latex | Jekyll

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
       try (string name) <?> name 
       spaces
       try rBracket <?> ">" 
       return name

rTag :: String -> Prim.Parsec [Char] st String
rTag name = lTag $ "/" ++ name 

italicItemPiece :: Prim.Parsec String st ItemPiece
italicItemPiece = do
                  lTag "italic"
                  text <- many1 (noneOf "<")
                  try (rTag "italic") <?> "rTag </italic>" 
                  return $ Italic text

regularItemPiece :: Prim.Parsec String st ItemPiece
regularItemPiece = do
                   text <- many1 (noneOf "<")
                   return $ JustText text

itemPieces :: Prim.Parsec String st Item
itemPieces = do
             let piece = try italicItemPiece <|> regularItemPiece
             many $ piece
  
topicItem :: String -> Prim.Parsec [Char] st Item
topicItem category = do
                     let tagString = "item" ++ category
                     lTag tagString
                     item <- itemPieces
                     rTag tagString 
                     return item 

topicTitle :: Prim.Parsec [Char] st String
topicTitle = do
             lTag "title"
             title <- many $ noneOf "<"
             rTag "title"
             return title

topic1 :: Prim.Parsec [Char] st Topic 
topic1 = do
         lTag "topic1"
         spaces
         title <- topicTitle
         spaces
         rCol <- endBy (try $ topicItem "") spaces
         try (rTag "topic1")
             <?> "rTag </topic1>"
         return $ Topic1 {title = title, items = rCol}

itemPair :: Prim.Parsec [Char] st (Item, [Item])
itemPair = do
           first <- topicItem "1"
           spaces
           second <- endBy (try $ topicItem "2") spaces
           return (first, second) 

topic2 :: Prim.Parsec [Char] st Topic
topic2 = do
         lTag "topic2"
         spaces
         title <- topicTitle
         spaces
         itemPairs <- endBy (try itemPair) spaces
         rTag "topic2"
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

convertCV :: Output -> CV -> String
convertCV Latex cv =
       latexHeader
    ++ beginTex Tabular1
    ++ body
    ++ "\\end{tabular}\n"
    ++ latexEnd
    where body = concat $ map (\x -> convertTopic Latex x ++ topicSep) (topics cv) 

convertCV Jekyll cv = jekyllHeader ++ downloadLink ++ body ++ downloadLink
    where body =    "<table>\n"
                 ++ (concat $ map (convertTopic Jekyll) (topics cv))
                 ++ "</table>\n"

-- Function for converting Topic to String based on the output format.

convertTopic :: Output -> Topic -> String

convertTopic Latex Topic1 {title = t, items = []} = 
    myTab 1 ++ latexBold t ++ " & & \\\\\n"

convertTopic Latex Topic1 {title = t, items = i : is} = 
    myTab 1 ++ latexBold t ++ makeOneCol Latex i 
    ++ otherRows 
    where otherRows = concat $ map (\x -> myTab 2 ++ makeOneCol Latex x) is

convertTopic Latex Topic2 {title = t, itemPairs = []} = 
    myTab 1 ++ latexBold t ++ " & & \\\\\n"

convertTopic Latex Topic2 {title = t, itemPairs = i : is} = 
    myTab 1 ++ latexBold t
    ++ convertPairToCols Latex i 
    ++ convertPairToTailRows Latex i
    ++ otherRows 
    where otherRows = concat $ map (\x -> myTab 2 ++ convertPairToCols Latex x ++ convertPairToTailRows Latex x) is

convertTopic Jekyll Topic1 {title = t, items = []} =
       myTab 1 ++ "<tr>\n"
    ++ myTab 2 ++ "<th>" ++ t ++ "</th>\n" 
    ++ myTab 1 ++ "</tr>\n"

convertTopic Jekyll Topic1 {title = t, items =  i : is} = 
       myTab 1 ++ "<tr>\n"
    ++ myTab 2 ++ "<th>" ++ t ++ "</th>\n" 
    ++ makeOneCol Jekyll i
    ++ myTab 1 ++ "</tr>\n"
    ++ otherRows
    where otherRows = concat $ map makeNoTitleRow is

convertTopic Jekyll Topic2 {title = t, itemPairs = []} =
       myTab 1 ++ "<tr>\n"
    ++ myTab 2 ++ "<th>" ++ t ++ "</th>\n" 
    ++ myTab 1 ++ "</tr>\n"

convertTopic Jekyll Topic2 {title = t, itemPairs =  i : is} = 
       myTab 1 ++ "<tr>\n"
    ++ myTab 2 ++ "<th>" ++ t ++ "</th>\n"
    ++ convertPairToCols Jekyll i
    ++ myTab 1 ++ "</tr>\n"
    ++ convertPairToTailRows Jekyll i
    ++ otherRows
    where otherRows = concat $ map(\x ->
               myTab 1 ++ "<tr>\n"
            ++ myTab 2 ++ "<th>" ++ t ++ "</th>\n"
            ++ convertPairToCols Jekyll x 
            ++ myTab 1 ++ "</tr>\n"
            ++ convertPairToTailRows Jekyll x
            )
            is

-- Function to convert item String to String for the given output format.

makeOneCol :: Output -> Item-> String

makeOneCol Latex item = " & \\multicolumn{2}{l}{" ++ convertItem Latex item ++ "}\\\\\n"

makeOneCol Jekyll item = myTab 2 ++ "<td colspan = \"2\">" ++ convertItem Jekyll item ++ "</td>\n"

-- Function to convert each item to a string.

convertItem :: Output -> Item -> String
convertItem format = concat . map (convertItemPiece format) 

-- Function to convert item info.

convertItemPiece :: Output -> ItemPiece -> String
convertItemPiece _ (JustText x) = x
convertItemPiece Latex (Italic x) = 
       "\\textit{" ++ x ++ "}"
convertItemPiece Jekyll (Italic x) = "<i>" ++ x ++ "</i>" 
   
-- Function to make a row with nothing in the title column (i.e. the first column) for Jekyll. This
-- isn't necessary for LaTeX.

makeNoTitleRow :: Item -> String
makeNoTitleRow item = myTab 1 ++ "<tr>\n"
                   ++ emptyColJekyll
                   ++ convertItem Jekyll item 
                   ++ myTab 1 ++ "</tr>\n"

-- Function to convert item pair to first row data given the output format.

convertPairToCols :: Output -> (Item, [Item]) -> String

convertPairToCols Latex (x, []) = " & " ++ convertItem Latex x ++ " & \\\\\n"

convertPairToCols Latex (x, i:is) = " & " ++ convertItem Latex x ++ " & " ++ convertItem Latex i ++ " \\\\\n"

convertPairToCols Jekyll (x, []) = 
       myTab 2 ++ "<td>" ++ convertItem Jekyll x ++ "</td>\n"
    ++ myTab 2 ++ " <td></td>\n"

convertPairToCols Jekyll (x, i:is) = 
       myTab 2 ++ "<td>" ++ convertItem Jekyll x ++ "</td>\n"
    ++ myTab 2 ++ "<td>" ++ convertItem Jekyll i ++ "</td>\n" 

-- Function to convert item pair to remaining rows given the output format.

convertPairToTailRows :: Output -> (Item, [Item]) -> String

convertPairToTailRows Latex (x, []) = ""

convertPairToTailRows Latex (x, i:is) = concat $ map rowToLatex is
    where rowToLatex y = myTab 2 ++ " & " ++ myTab 1 ++ " & " ++ convertItem Latex y ++ " \\\\\n" 

convertPairToTailRows Jekyll (x, []) = ""

convertPairToTailRows Jekyll (x, i:is) = concat $ map makeRow is 
    where makeRow i =  myTab 1 ++ "<tr>\n"
                    ++ emptyColJekyll
                    ++ emptyColJekyll
                    ++ myTab 2 ++ "<td>" ++ convertItem Jekyll i ++ "</td>\n"
                    ++ myTab 1 ++ "</tr>\n" 

-- The header to put at the start of a jekyll file.

jekyllHeader :: String
jekyllHeader =    "---\n"
               ++ "layout: default\n"
               ++ "title: Matthew McGonagle's CV\n"
               ++ "---\n\n"
               ++ "<h1>{{page . title}}</h1>\n"

------------------------------
-- CV to LaTeX file converters.
------------------------------

topicSep :: String
topicSep = myTab 1 ++ "\\midrule\n"

beginTex :: LatexEnv -> String
beginTex Tabular1 = "\\begin{tabular}{llp{7cm}}\n"
beginTex Tabular2 = beginTex Tabular1 
beginTex _ = "" 

latexBold :: String -> String
latexBold x = "{\\bf " ++ x ++ "} "

------------------------------------
-- CV to Jekyll Markdown Converters.
------------------------------------

emptyColJekyll :: String
emptyColJekyll = myTab 2 ++ "<td></td>\n"

downloadLink :: String
downloadLink = 
    " <p> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</p>\n"

