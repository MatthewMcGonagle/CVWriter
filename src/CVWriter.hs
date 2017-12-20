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
import qualified Control.Monad.State as Ctl 


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

type NestLevel = Int

class CVConvertible a where
    emptyCol :: a
    indent :: NestLevel -> a
    makeHeader :: CV -> a
    beginTable :: NestLevel -> a -- Latex needs special characters for sub-table formatting.
    endTable :: NestLevel -> a
    makeRow :: NestLevel -> a -> a -> a
    convertItemAtom :: NestLevel -> ItemAtom -> a
    convertTitle :: String -> a
    topicSeparator :: NestLevel -> a
    endDoc :: a
    paragraph :: a -> a

------------------------------     
-- Latex Conversion 
------------------------------

data LatexText = LatexText {text :: String}

instance Monoid LatexText where
    mempty = LatexText ""
    x `mappend` y = LatexText $ (text x) ++ (text y) 

instance CVConvertible LatexText where

    emptyCol = LatexText "     "

    indent n = LatexText $ replicate (4 * (n + 1)) ' '

    makeHeader cv = LatexText $  
           "\\documentclass[12pt]{article}\n"
        ++ "\\usepackage{booktabs}\n"
        ++ "\\usepackage{hyperref}\n"
        ++ "\\usepackage[a4paper, total={6in, 8in}, margin=0.5in]{geometry}\n\n"
        ++ "\\begin{document}\n"
        ++ "\\begin{center}\n"
        ++ "\\textbf{\\Large " ++ cvTitle cv ++ "}\n"
        ++ "\\end{center}\n\n"

    beginTable 0 = LatexText $ "\\noindent\\begin{tabular}{ll}\n"
    beginTable n = LatexText $ text (indent n) ++ "\\begin{tabular}{@{}lp{8cm}@{}}\n"

    endTable n = LatexText $ text (indent n) ++ "\\end{tabular}\n"

    makeRow 0 x y = LatexText $
        text (indent 0) ++ "\\textbf{" ++ text x ++ "} & " ++ text y ++ "\\\\\n\n"
    makeRow n x y = LatexText $
        text (indent n) ++ text x ++ " & " ++ text y ++ "\\\\\n\n"

    convertItemAtom _ (JustText x) = LatexText $ x

    convertItemAtom _ (Italic x) =  LatexText $ "\\textit{" ++ x ++ "}"

    convertItemAtom _ (Hyperlink {hyperlabel = label, url = url}) = LatexText $
           "\\href{" ++ url ++ "}{" ++ label ++ "}"
    
    convertItemAtom n Newline = LatexText $ "\\newline\n" ++ (text . indent) ( n + 1 )

    convertTitle x = LatexText x

    topicSeparator 0 = LatexText "\\midrule\n"
    topicSeparator n = LatexText $ text (indent n) ++ "\\midrule\n"

    endDoc = LatexText $ "\\end{document}\n"

    paragraph x = x


------------------------------
-- Jekyll Conversion 
------------------------------

data  JekyllText = JekyllText {jtext :: String}

instance Monoid JekyllText where
    mempty = JekyllText ""
    x `mappend` y = JekyllText $ jtext x ++ jtext y

instance CVConvertible JekyllText where

    emptyCol = JekyllText ""

    indent n = JekyllText $ replicate (4 * (n + 1)) ' '
   
    makeHeader cv = JekyllText $ 
           "---\n"
        ++ "layout: default\n"
        ++ "title: Matthew McGonagle's CV\n"
        ++ "---\n\n" 
        ++ "<h2> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</h2>\n"
        ++ "<h1>" ++ cvTitle cv ++ "</h1>\n"

    beginTable n = JekyllText $ jtext (indent n) ++ "<table>\n"

    endTable n = JekyllText $ jtext (indent n) ++ "</table>\n"

    makeRow 0 x y = JekyllText $ 
           indent0 ++ "<tr>\n"
        ++ indent0 ++ "<th>" ++ jtext x ++ "</th>\n"
        ++ indent0 ++ "<td>" ++ jtext y ++ "</td>\n"
        ++ indent0 ++ "</tr>\n\n"
        where indent0 = jtext $ indent 0 
    makeRow n x y = JekyllText $ 
           indentn ++ "<tr>\n"
        ++ indentn ++ "<td>" ++ jtext x ++ "</td>\n"
        ++ indentn ++ "<td>" ++ jtext y ++ "</td>\n"
        ++ indentn ++ "</tr>\n\n"
        where indentn = jtext $ indent n 

    convertItemAtom _ (JustText x) = JekyllText $ x

    convertItemAtom _ (Italic x) =  JekyllText $ "<i>" ++ x ++ "</i>"

    convertItemAtom _ (Hyperlink {hyperlabel = label, url = url}) = JekyllText $
           "<a href = \"" ++ url ++ "\">" ++ label ++ "</a>"

    convertItemAtom n Newline = JekyllText $ jtext (indent n) ++ "<br/>"

    convertTitle x = JekyllText x

    topicSeparator n = JekyllText $ 
           indentn ++ "<tr>\n"
        ++ indentn ++ "<td colspan = \"100%\"; style = \"border-bottom: 1px solid black\"></td>\n"
        ++ indentn ++ "</tr>\n"
        where indentn = jtext $ indent n 

    endDoc = JekyllText  "<h2> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</h2>\n"

    paragraph x = JekyllText $ "<p>" ++ jtext x ++ "</p>"

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
    `mappend` body 
    `mappend` endDoc
    where body = fst $ topicsWIndent
          topicsWIndent = Ctl.runState (convertTopicList $ topics cv) 0

convertTopicList :: (CVConvertible a, Monoid a) => [Topic] -> Ctl.State NestLevel a

convertTopicList [] = return mempty

convertTopicList [t] = 
    do 
    nestLevel <- Ctl.get
    t' <- convertTopic t
    return $ beginTable nestLevel 
             `mappend` t' 
             `mappend` endTable nestLevel

convertTopicList (t:ts) = 
    do
    nestLevel <- Ctl.get
    t' <- convertTopic t
    let convertTailTopic x = 
            do
            nestLevel' <- Ctl.get
            x' <- convertTopic x
            return $ topicSeparator nestLevel' `mappend` x' 
    ts' <- mapM convertTailTopic ts 
    return $ beginTable nestLevel
             `mappend` t' 
             `mappend` mconcat ts' 
             `mappend` endTable nestLevel

-- convertTopic :: (CVConvertible a, Monoid a) => Topic -> a
-- convertTopic x = makeRow (convertTitle $ title x) (convertItem $ item x) 

convertTopic :: (CVConvertible a, Monoid a) => Topic -> Ctl.State NestLevel a
convertTopic x = do
                 nestLevel <- Ctl.get 
                 x' <- convertItem $ item x
                 return $ makeRow nestLevel (convertTitle $ title x) x' 

-- convertItem :: (CVConvertible a, Monoid a) => Item -> a
-- convertItem (Atoms atoms) = mconcat $ map convertItemAtom atoms 
-- convertItem (NestedTopics ts) = convertSubtopics ts 

convertItem :: (CVConvertible a, Monoid a) => Item -> Ctl.State NestLevel a
convertItem (Atoms atoms) = 
    do
    nestLevel <- Ctl.get 
    let atoms' = map (convertItemAtom nestLevel) atoms
    return $ mconcat atoms'

convertItem (NestedTopics ts) = 
    do
    nestedLevel <- Ctl.get
    Ctl.put $ nestedLevel + 1
    ts' <- convertTopicList ts 
    Ctl.put nestedLevel
    return $ ts'
