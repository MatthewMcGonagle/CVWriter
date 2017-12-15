module CVWriter 
( topicItem
, myParse
, myParseTest
, CV
, Topic
, Output (Latex, Jekyll)
, LatexText (text)
, convertCV
, convertCV'
) where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as Prim
import Text.Parsec.Combinator


data ItemPiece = JustText String | Italic String | Hyperlink { hyperlabel :: String, url :: String }
    deriving (Show)

type Item= [ItemPiece]

data Topic =   Topic1 {title :: String, items :: [Item]}
             | Topic2 {title :: String, itemPairs :: [(Item, [Item])]} 
    deriving (Show)

data CV = CV {topics :: [Topic], cvTitle :: String}
    deriving (Show)

data LatexEnv = Tabular1 | Tabular2 | Section

data Output = Latex | Jekyll

data Row a = Row2 a a | Row3 a a a

class CVConvertible a where
    emptyCol :: a
    makeHeader' :: CV -> a
    makeTable :: [Row a] -> a
    makeRow :: Row a -> a
    convertItemPiece' :: ItemPiece -> a
    convertTitle :: String -> a
    endDoc :: a

------------------------------     
-- Latex Parsing
------------------------------

data LatexText = LatexText {text :: String}

instance Monoid LatexText where
    mempty = LatexText ""
    x `mappend` y = LatexText $ (text x) ++ (text y) 

instance CVConvertible LatexText where

    emptyCol = LatexText "     "

    makeHeader' cv = LatexText $  
           "\\documentclass{article}\n"
        ++ "\\usepackage{booktabs}\n"
        ++ "\\usepackage{hyperref}\n\n"
        ++ "\\begin{document}\n"
        ++ "\\begin{center}\n"
        ++ "\\textbf{\\Large " ++ cvTitle cv ++ "}\n"
        ++ "\\end{center}\n\n"

    makeTable rows = LatexText $   
           "\\noindent\\begin{tabular}{llp{7cm}}\n" 
        ++ concat (map (text . makeRow) rows)
        ++ "\\end{tabular}\n"

    makeRow (Row2 x y) = LatexText $  
           text x  ++ " & \\multicolumn{2}{l}{" ++ text y ++ "}\\\\\n"

    makeRow (Row3 x y z) = LatexText $  
           text x ++ " & " ++ text y ++ " & " ++ text z ++ "\\\\\n"

    convertItemPiece' (JustText x) = LatexText $ x

    convertItemPiece' (Italic x) =  LatexText $ "\\textit{" ++ x ++ "}"

    convertItemPiece' (Hyperlink {hyperlabel = label, url = url}) = LatexText $
           "\\href{" ++ url ++ "}{" ++ label ++ "}"

    convertTitle title = LatexText $
           "\\textbf{" ++ title ++ "}" 

    endDoc = LatexText $ "\\end{document}\n"
       
data JekyllText = JekyllText String

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

hyperlinkItemPiece :: Prim.Parsec String st ItemPiece
hyperlinkItemPiece = do
                     lTag "hyperlink"
                     label <- many1 (noneOf "<")
                     lTag "url"
                     spaces
                     url <- many1 (noneOf "<")
                     spaces
                     rTag "hyperlink"
                     return $ Hyperlink { hyperlabel = label, url = url}

itemPieces :: Prim.Parsec String st Item
itemPieces = do
             let piece = try italicItemPiece 
                     <|> try hyperlinkItemPiece 
                     <|> regularItemPiece
             many $ piece
  
topicItem :: String -> Prim.Parsec [Char] st Item
topicItem category = do
                     let tagString = "item" ++ category
                     lTag tagString
                     spaces
                     item <- itemPieces
                     rTag tagString 
                     return item 

titleBlock :: Prim.Parsec [Char] st String
titleBlock = do
             lTag "title"
             title <- many $ noneOf "<"
             rTag "title"
             return title

topic1 :: Prim.Parsec [Char] st Topic 
topic1 = do
         lTag "topic1"
         spaces
         topicTitle <- titleBlock 
         spaces
         rCol <- endBy (try $ topicItem "") spaces
         try (rTag "topic1")
             <?> "rTag </topic1>"
         return $ Topic1 {title = topicTitle, items = rCol}

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
         topicTitle <- titleBlock 
         spaces
         itemPairs <- endBy (try itemPair) spaces
         rTag "topic2"
         return $ Topic2 {title = topicTitle, itemPairs = itemPairs} 
        
topicList :: Prim.Parsec [Char] st [Topic]
topicList = endBy (try topic1 <|> topic2) spaces 
            
cvfile :: Prim.Parsec String st CV
cvfile = do
         spaces
         cvTitle <- titleBlock
         spaces
         topics <- topicList
         spaces
         eof
         return $ CV {topics = topics, cvTitle = cvTitle}
         
myParse = parse cvfile "source name"
myParseTest = parseTest cvfile 

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
       makeHeader Latex (cvTitle cv)
    ++ beginTex Tabular1
    ++ body
    ++ "\\end{tabular}\n"
    ++ latexEnd
    where body = concat $ map (\x -> convertTopic Latex x ++ topicSep) (topics cv) 

convertCV Jekyll cv = makeHeader Jekyll (cvTitle cv) ++ downloadLink ++ body ++ downloadLink
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
            ++ emptyColJekyll 
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

convertItemPiece Latex (Hyperlink {hyperlabel = label, url = url}) = 
    "\\href{" ++ url ++ "}{" ++ label ++ "}"
convertItemPiece Jekyll (Hyperlink {hyperlabel = label, url = url}) =
    "<a href = \"" ++ url ++ "\">" ++ label ++ "</a>" 
   
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

-- The header of the CV depending on the output type.

makeHeader :: Output -> String -> String
makeHeader Latex headerTitle = 
       "\\documentclass{article}\n"
    ++ "\\usepackage{booktabs}\n"
    ++ "\\usepackage{hyperref}\n\n"
    ++ "\\begin{document}\n"
    ++ "\\begin{center}\n"
    ++ "\\textbf{\\Large " ++ headerTitle ++ "}\n"
    ++ "\\end{center}\n\n"

makeHeader Jekyll headerTitle = 
       "---\n"
    ++ "layout: default\n"
    ++ "title: Matthew McGonagle's CV\n"
    ++ "---\n\n"
    ++ "<h1>" ++ headerTitle ++ "</h1>\n"

------------------------------
-- CV to LaTeX file converters.
------------------------------

topicSep :: String
topicSep = myTab 1 ++ "\\midrule\n"

beginTex :: LatexEnv -> String
beginTex Tabular1 = "\\noindent\\begin{tabular}{llp{7cm}}\n"
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

-------------------------------------
-- Generic CV converter
-------------------------------------

convertCV' :: (CVConvertible a, Monoid a) => CV -> a  
convertCV' cv = 
              makeHeader' cv 
    `mappend` makeTable tableRows
    `mappend` endDoc
    where tableRows = concat $ map convertTopic' (topics cv)

                 
convertTopic' :: (CVConvertible a, Monoid a) => Topic -> [Row a]
convertTopic' Topic1 {title = title, items = []} = 
    [Row2 (convertTitle title) emptyCol ] 

convertTopic' Topic1 {title = title, items = (i:is)} =  
    firstRow : otherRows
    where firstRow = Row2 (convertTitle title) (convertItem' i)
          otherRows = map justItem is
          justItem i = Row2 emptyCol (convertItem' i)

convertTopic' Topic2 {title = title, itemPairs = []} = 
    [Row3 (convertTitle title) emptyCol emptyCol]

convertTopic' Topic2 {title = title, itemPairs = (p:ps)} =
    firstSection ++ otherSections 
    where firstSection = convertPair (convertTitle title) p
          otherSections = concat $ map (convertPair emptyCol) ps
  
convertPair :: (CVConvertible a, Monoid a) => a -> (Item, [Item]) -> [Row a]
convertPair title (subtitle, []) = 
    [Row3 title (convertItem' subtitle) emptyCol]  

convertPair title (subtitle, i : is) = 
    firstRow : otherRows
    where firstRow = Row3 title (convertItem' subtitle) (convertItem' i)
          otherRows = map justColThree is
          justColThree i = Row3 emptyCol emptyCol (convertItem' i)

convertItem' :: (CVConvertible a, Monoid a) => Item -> a
convertItem' pieces = mconcat $ map convertItemPiece' pieces
