module CVWriter 
( topicItem
, myParse
, myParseTest
, CV
, Topic
, Output (Latex, Jekyll)
, LatexText (text)
, JekyllText (jtext)
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
       

------------------------------
-- Jekyll Parsing
------------------------------

data  JekyllText = JekyllText {jtext :: String}

instance Monoid JekyllText where
    mempty = JekyllText ""
    x `mappend` y = JekyllText $ jtext x ++ jtext y

instance CVConvertible JekyllText where

    emptyCol = JekyllText ""
   
    makeHeader' cv = JekyllText $ 
           "---\n"
        ++ "layout: default\n"
        ++ "title: Matthew McGonagle's CV\n"
        ++ "---\n\n" 
        ++ "<h1>" ++ cvTitle cv ++ "</h1>\n"
        ++ "<h2> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</h2>\n"



    makeTable rows = JekyllText $
           "<table>\n"
        ++ (jtext . mconcat) (map makeRow rows)
        ++ "</table>\n"

    makeRow (Row2 x y) = JekyllText $  
           myTab 1 ++ "<tr>\n"
        ++ myTab 2 ++ "<th>" ++ jtext x ++ "</th>\n"
        ++ myTab 2 ++ "<td colspan = \"2\">" ++ jtext y ++ "</td>\n"
        ++ myTab 1 ++ "</tr>\n"

    makeRow (Row3 x y z) = JekyllText $  
           myTab 1 ++ "<tr>\n"
        ++ myTab 2 ++ "<th>" ++ jtext x ++ "</th>\n"
        ++ myTab 2 ++ "<td>" ++ jtext y ++ "</td>\n"
        ++ myTab 2 ++ "<td>" ++ jtext z ++ "</td>\n"
        ++ myTab 1 ++ "</tr>\n"

    convertItemPiece' (JustText x) = JekyllText $ x

    convertItemPiece' (Italic x) =  JekyllText $ "<i>" ++ x ++ "</i>"

    convertItemPiece' (Hyperlink {hyperlabel = label, url = url}) = JekyllText $
           "<a href = \"" ++ url ++ "\">" ++ label ++ "</a>"

    convertTitle title = JekyllText title 

    endDoc = JekyllText  "<h2> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</h2>\n"

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
