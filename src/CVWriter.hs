module CVWriter 
( topicItem
, myParse
, myParseTest
, CV
, Topic
, LatexText (text)
, JekyllText (jtext)
, convertCV'
) where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as Prim
import Text.Parsec.Combinator


data ItemAtom =  JustText String 
               | Italic String 
               | Hyperlink { hyperlabel :: String, url :: String }
               | Newline
               deriving (Show)

data Item = Atoms [ItemAtom] | NestedTopics [Topic]
    deriving (Show)

data Topic =   Topic {title :: String, item :: Item}
    deriving (Show)

data CV = CV {topics :: [Topic], cvTitle :: String}
    deriving (Show)

data LatexEnv = Tabular1 | Tabular2 | Section

class CVConvertible a where
    emptyCol :: a
    makeHeader :: CV -> a
    beginTable :: a
    endTable :: a
    beginSubtable :: a -- Latex needs special characters for sub-table formatting.
    endSubtable :: a
    makeRow :: a -> a -> a
    convertItemAtom :: ItemAtom -> a
    convertTitle :: String -> a
    topicSeparator :: a
    subtopicSeparator :: a
    endDoc :: a
    paragraph :: a -> a
    parNewLine :: a

------------------------------     
-- Latex Conversion 
------------------------------

data LatexText = LatexText {text :: String}

instance Monoid LatexText where
    mempty = LatexText ""
    x `mappend` y = LatexText $ (text x) ++ (text y) 

instance CVConvertible LatexText where

    emptyCol = LatexText "     "

    makeHeader cv = LatexText $  
           "\\documentclass[12pt]{article}\n"
        ++ "\\usepackage{booktabs}\n"
        ++ "\\usepackage{hyperref}\n"
        ++ "\\usepackage[a4paper, total={6in, 8in}, margin=0.5in]{geometry}\n\n"
        ++ "\\begin{document}\n"
        ++ "\\begin{center}\n"
        ++ "\\textbf{\\Large " ++ cvTitle cv ++ "}\n"
        ++ "\\end{center}\n\n"

    beginTable = LatexText $ "\\noindent\\begin{tabular}{ll}\n"

    endTable = LatexText $ "\\end{tabular}\n"

    beginSubtable = LatexText $ "\\begin{tabular}{@{}lp{8cm}@{}}\n"
    
    endSubtable = LatexText $ "\\end{tabular}\n"

    makeRow x y = LatexText $
        text x ++ " & " ++ text y ++ "\\\\\n"

    convertItemAtom (JustText x) = LatexText $ x

    convertItemAtom (Italic x) =  LatexText $ "\\textit{" ++ x ++ "}"

    convertItemAtom (Hyperlink {hyperlabel = label, url = url}) = LatexText $
           "\\href{" ++ url ++ "}{" ++ label ++ "}"
    
    convertItemAtom Newline = LatexText "\\newline "

    convertTitle title = LatexText $
           "\\textbf{" ++ title ++ "}" 

    topicSeparator = LatexText "\\midrule\n"

    subtopicSeparator = LatexText "\\cmidrule{2-3}\n"

    endDoc = LatexText $ "\\end{document}\n"

    paragraph x = x

    parNewLine = LatexText $ "\\newline\n" ++ myTab 2
       

------------------------------
-- Jekyll Conversion 
------------------------------

data  JekyllText = JekyllText {jtext :: String}

instance Monoid JekyllText where
    mempty = JekyllText ""
    x `mappend` y = JekyllText $ jtext x ++ jtext y

instance CVConvertible JekyllText where

    emptyCol = JekyllText ""
   
    makeHeader cv = JekyllText $ 
           "---\n"
        ++ "layout: default\n"
        ++ "title: Matthew McGonagle's CV\n"
        ++ "---\n\n" 
        ++ "<h2> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</h2>\n"
        ++ "<h1>" ++ cvTitle cv ++ "</h1>\n"

    beginTable = JekyllText "<table>\n"

    endTable = JekyllText "</table>\n"

    beginSubtable = beginTable
    
    endSubtable = endTable

    makeRow x y = JekyllText $ 
           myTab 1 ++ "<tr>\n"
        ++ myTab 2 ++ "<th>" ++ jtext x ++ "</th>\n"
        ++ myTab 2 ++ "<td>" ++ jtext y ++ "</td>\n"
        ++ myTab 1 ++ "</tr>\n"

    convertItemAtom (JustText x) = JekyllText $ x

    convertItemAtom (Italic x) =  JekyllText $ "<i>" ++ x ++ "</i>"

    convertItemAtom (Hyperlink {hyperlabel = label, url = url}) = JekyllText $
           "<a href = \"" ++ url ++ "\">" ++ label ++ "</a>"

    convertItemAtom Newline = JekyllText "<br/>"

    convertTitle title = JekyllText title 

    topicSeparator = JekyllText $ 
           myTab 1 ++ "<tr>\n"
        ++ myTab 2 ++ "<td colspan = \"100%\"; style = \"border-bottom: 1px solid black\"></td>\n"
        ++ myTab 1 ++ "</tr>\n"

    subtopicSeparator = JekyllText $
           myTab 1 ++ "<tr>\n"
        ++ myTab 2 ++ "<td></td>\n"
        ++ myTab 2 ++ "<td colspan = \"2\"; style = \"border-bottom: 1px solid black\"></td>\n"
        ++ myTab 1 ++ "</tr>\n" 

    endDoc = JekyllText  "<h2> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</h2>\n"

    paragraph x = JekyllText $ "<p>" ++ jtext x ++ "</p>"

    parNewLine = JekyllText $ "<br/>\n" ++ myTab 2

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
       try (string name) <?> "string \"" ++ name ++ "\"" 
       spaces
       try rBracket <?> "> for lTag " ++ name 
       return name

rTag :: String -> Prim.Parsec [Char] st String
rTag name = lTag $ "/" ++ name 

italicAtom :: Prim.Parsec String st ItemAtom
italicAtom = do
             lTag "italic"
             text <- try (many1 $ noneOf "<") <?> "text to italicize"
             try (rTag "italic") <?> "rTag italic" 
             spaces
             return $ Italic text

regularAtom :: Prim.Parsec String st ItemAtom
regularAtom = do
              text <- try (many1 $ noneOf "<") <?> "regular text"
              return $ JustText text

hyperlinkAtom :: Prim.Parsec String st ItemAtom
hyperlinkAtom = do
                lTag "hyperlink"
                label <- try (many1 $ noneOf "<") <?> "hyperlink label" 
                lTag "url"
                spaces
                url <- try (many1 $ noneOf "<") <?> "hyperlink url"
                spaces
                rTag "hyperlink"
                spaces
                return $ Hyperlink {hyperlabel = label, url = url}

newlineAtom :: Prim.Parsec String st ItemAtom
newlineAtom = do
              lTag "newline"
              spaces
              return Newline

atoms :: Prim.Parsec String st Item
atoms = do
        let atom =     try italicAtom
                   <|> try hyperlinkAtom
                   <|> try newlineAtom
                   <|> try regularAtom
                   <?> "atom"
        atomlist <- many1 $ atom 
        return $ Atoms atomlist 
 
-- Remember to parse subtopic before parsing atoms. 
topicItem :: Prim.Parsec [Char] st Item
topicItem = do
            let subtopics = do 
                            subs <- endBy1 (try topic) spaces
                            return $ NestedTopics subs 
            item <- try subtopics <|> atoms 
            return item 

titleBlock :: Prim.Parsec [Char] st String
titleBlock = do
             lTag "title"
             title <- many $ noneOf "<"
             rTag "title"
             return title

topic :: Prim.Parsec [Char] st Topic 
topic = do
        lTag "topic"
        topicTitle <- many1 $ noneOf "<"
        rTag "topic"
        spaces
        lTag "item"
        spaces
        item <- topicItem 
        spaces
        rTag "item"
        return $ Topic {title = topicTitle, item = item}

topicList :: Prim.Parsec [Char] st [Topic]
topicList = endBy topic spaces 
            
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
              makeHeader cv 
    `mappend` convertTopicList (topics cv) 
    `mappend` endDoc

convertTopicList :: (CVConvertible a, Monoid a) => [Topic] -> a

convertTopicList [] = mempty

convertTopicList [t] = 
    beginTable
    `mappend` convertTopic t
    `mappend` endTable

convertTopicList (t:ts) = 
    beginTable
    `mappend` convertTopic t
    `mappend` mconcat (map convertTailTopic ts)
    `mappend` endTable
    where convertTailTopic x = topicSeparator `mappend` (convertTopic x)

convertTopic :: (CVConvertible a, Monoid a) => Topic -> a
convertTopic x = makeRow (convertTitle $ title x) (convertItem $ item x) 

convertItem :: (CVConvertible a, Monoid a) => Item -> a
convertItem (Atoms atoms) = mconcat $ map convertItemAtom atoms 
convertItem (NestedTopics ts) = convertSubtopics ts 

convertSubtopics :: (CVConvertible a, Monoid a) => [Topic] -> a
convertSubtopics [] = mempty
convertSubtopics [t] =
    beginSubtable
    `mappend` convertTopic t
    `mappend` endSubtable

convertSubtopics (t:ts) = 
    beginSubtable
    `mappend` convertTopic t
    `mappend` mconcat (map convertTailTopic ts)
    `mappend` endSubtable 
    where convertTailTopic x = topicSeparator `mappend` (convertTopic x)

