module CVWriter 
( topicItem
, parseCV 
, CV
, LatexText (text)
, JekyllText (jtext)
, convertCV
) where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as Prim
import Text.Parsec.Combinator
import qualified Control.Monad.State as Ctl 

-- | Holds primitive output pieces.
data ItemAtom 
    =  JustText String -- ^ For regular text. 
    | Italic String -- ^ Italicized text. 
    -- | Hyperlink label and url strings. 
    | Hyperlink { hyperlabel :: String -- ^ Text to attach hyperlink to.
                , url :: String -- ^ The url address for the hyperlink.
                }
    | Newline -- ^ Newline for output rendering (final html and pdf). Not formatting inside source files. 
    deriving (Show)

-- | Holds item associated to a topic. Can either be a list of atoms to print, or a list of subtopics.
data Item 
    = Atoms [ItemAtom] -- ^ List of atoms making up a single item.
    | NestedTopics [Topic] -- ^ A list of subtopics.
    deriving (Show)

-- | A topic is effectively a row of a table/subtable. It has a title and a single item. 
data Topic = 
    Topic { title :: String -- ^ Title goes in the first column.
          , item :: Item -- ^ The item goes in the second column.
          }
    deriving (Show)

-- | The parts that make up the entire CV.
data CV = 
    CV { topics :: [Topic] -- ^ The list of topics in body table of CV.
       , cvTitle :: String -- ^ The title of the CV.
       }
    deriving (Show)

{- | 
   For formatting and printing purposes, we will keep track of the current nesting level, which
   should be a non-negative integer. 
-}
type NestLevel = Int

-- | Class of types for which an instance of CV may be converted into. 
class CVConvertible a where
    emptyCol :: a -- ^ Represents an empty column.
    indent :: NestLevel -> a -- ^ Indents amount based on current nested topic level.
    makeHeader :: CV -> a -- ^ Makes header based on information inside CV.
    beginTable :: NestLevel -> a -- ^ Beginning of table based on current nested topic level. 
    endTable :: NestLevel -> a -- ^ Ending of table based on current nested topic level. 
    makeRow :: NestLevel -> a -> a -> a -- ^ Make a row based on current nested topic level.
    convertItemAtom :: NestLevel -> ItemAtom -> a -- ^ Convert an atom of info into output based on nesting level.
    convertTitle :: String -> a -- ^ Convert CV title.
    topicSeparator :: NestLevel -> a -- ^ Something to separate topics based on nesting level.
    endDoc :: a -- ^ End of the document.
    paragraph :: a -> a -- ^ Wrap stuff inside a paragraph format.

------------------------------     
-- Latex Conversion 
------------------------------

-- | Type representing strings for Latex code.
data LatexText = 
    LatexText { text :: String -- ^ The text for Latex code.
              }

-- | LatexText is a monoid simply inherited from wrapping a String. 
instance Monoid LatexText where
    mempty = LatexText "" -- ^ Empty Latex string.
    x `mappend` y = LatexText $ (text x) ++ (text y) -- ^ Simply String ++. 

-- | Basic elements of converting CV to Latex code.
instance CVConvertible LatexText where

    emptyCol = LatexText $ replicate 4 ' ' -- ^ Simply empty spaces. 

    indent n = LatexText $ replicate (4 * (n + 1)) ' ' -- ^ Just use a certain number of empty spaces.

    -- | Header includes document class, packages to use, and the title of the CV.
    makeHeader cv = LatexText $  
           "\\documentclass[12pt]{article}\n"
        ++ "\\usepackage{booktabs}\n"
        ++ "\\usepackage[colorlinks = true, urlcolor = blue]{hyperref}\n"
        ++ "\\usepackage[a4paper, total={6in, 8in}, margin=0.5in]{geometry}\n\n"
        ++ "\\begin{document}\n"
        ++ "\\begin{center}\n"
        ++ "\\textbf{\\Large " ++ cvTitle cv ++ "}\n"
        ++ "\\end{center}\n\n"

    -- | For no nesting, make sure to include no indentation. Also just use two left justified columns.
    beginTable 0 = LatexText "\\noindent\\begin{tabular}{ll}\n"
    
    -- | For nested tables, use a left justified column and a paragraph column.
    beginTable _ = LatexText "\\begin{tabular}{@{}lp{8cm}@{}}\n"

    -- | The ending of a table is always the same.
    endTable n = LatexText $ text (indent n) ++ "\\end{tabular}\n"

    -- | For no nesting, the first column is put in bold face.
    makeRow 0 x y = LatexText $
        text (indent 0) ++ "\\textbf{" ++ text x ++ "} & " ++ text y ++ "\\\\\n\n"
    -- | For nesting, both columns are treated as nothing special. 
    makeRow n x y = LatexText $
        text (indent n) ++ text x ++ " & " ++ text y ++ "\\\\\n\n"

    -- | Just rewraps string.
    convertItemAtom _ (JustText x) = LatexText $ x
    
    -- | Wraps text in \textit Latex macro.
    convertItemAtom _ (Italic x) =  LatexText $ "\\textit{" ++ x ++ "}"

    -- | Uses the \href Latex macro from the hyperref Latex package.
    convertItemAtom _ (Hyperlink {hyperlabel = label, url = url}) = LatexText $
           "\\href{" ++ url ++ "}{" ++ label ++ "}"
    
    -- | Use a tabular \newline (note this isn't the "\\" newline).
    convertItemAtom n Newline = LatexText $ "\\newline\n" ++ (text . indent) ( n + 1 )

    -- | Just rewrap String.
    convertTitle x = LatexText x

    -- | Use \midrule in all cases. 
    topicSeparator n = LatexText $ text (indent n) ++ "\\midrule\n"

    -- | Simple end of document in Latex.
    endDoc = LatexText $ "\\end{document}\n"

    -- | Paragraph is equivalent to the identity for Latex.
    paragraph x = x


------------------------------
-- Jekyll Conversion 
------------------------------

-- | Wraps a String in a type for Jekyll compatible HTML code.
data  JekyllText = JekyllText {jtext :: String}

-- | JekyllText is a monoid since it is a wrapper for String and String is a monoid.
instance Monoid JekyllText where
    mempty = JekyllText ""
    x `mappend` y = JekyllText $ jtext x ++ jtext y

-- | A CV may be converted into Jekyll compatible HTML.
instance CVConvertible JekyllText where

    -- | For HTML, an empty column doesn't require using any text, so it is an empty string.
    emptyCol = JekyllText ""

    -- | Use appropriate number of ' ' characters to indent. 
    indent n = JekyllText $ replicate (4 * (n + 1)) ' '
  
    -- | Header contain's information for Jekyll followed by Header tags. 
    makeHeader cv = JekyllText $ 
           "---\n"
        ++ "layout: default\n"
        ++ "title: " ++ cvTitle cv ++ "'s CV\n"
        ++ "---\n\n" 
        ++ "<h2> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</h2>\n"
        ++ "<h1>" ++ cvTitle cv ++ "</h1>\n"

    -- | Nested level only affects the indentation level.  
    beginTable n = JekyllText $ jtext (indent n) ++ "<table>\n"

    -- | Nested level only affects the indentation level.
    endTable n = JekyllText $ jtext (indent n) ++ "</table>\n"

    -- | For nested level 0, the first column is inside table header tags.
    makeRow 0 x y = JekyllText $ 
           indent0 ++ "<tr>\n"
        ++ indent0 ++ "<th>" ++ jtext x ++ "</th>\n"
        ++ indent0 ++ "<td>" ++ jtext y ++ "</td>\n"
        ++ indent0 ++ "</tr>\n\n"
        where indent0 = jtext $ indent 0 

    -- | For higher nested levels, all columns are put inside table data tags.
    makeRow n x y = JekyllText $ 
           indentn ++ "<tr>\n"
        ++ indentn ++ "<td>" ++ jtext x ++ "</td>\n"
        ++ indentn ++ "<td>" ++ jtext y ++ "</td>\n"
        ++ indentn ++ "</tr>\n\n"
        where indentn = jtext $ indent n 

    -- | Simply rewrap the string inside a JekyllText type.
    convertItemAtom _ (JustText x) = JekyllText $ x

    -- | Put the text inside italic tags.
    convertItemAtom _ (Italic x) =  JekyllText $ "<i>" ++ x ++ "</i>"

    -- | Appropriately add the label and url using the <a href = "url"> label </a> tags.
    convertItemAtom _ (Hyperlink {hyperlabel = label, url = url}) = JekyllText $
           "<a href = \"" ++ url ++ "\">" ++ label ++ "</a>"

    {- | Rendered html will contain a newline in this column using the <br/> tag. Nesting level
         is simply for making the appropriate indentation.
    -}
    convertItemAtom n Newline = JekyllText $ "<br/>\n" ++ (jtext . indent) (n + 1)

    -- | Simply wraps String inside the JekyllText type.
    convertTitle x = JekyllText x

    -- | Topic Separator is a horizontal line with width given by the current table/sub-table width.
    topicSeparator n = JekyllText $ 
           indentn ++ "<tr>\n"
        ++ indentn ++ "<td colspan = \"100%\"; style = \"border-bottom: 1px solid black\"></td>\n"
        ++ indentn ++ "</tr>\n"
        where indentn = jtext $ indent n 

    {- | The end of the document is simply a link to pdf version. No need for </html> tag since output is
         meant to be processed by Jekyll.
    -}
    endDoc = JekyllText  "<h2> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</h2>\n"

    -- | Put the string inside <p> tags.
    paragraph x = JekyllText $ "<p>" ++ jtext x ++ "</p>"

------------------------------
-- *.cv Text Parsers
------------------------------

-- | A simple '<' character.
lBracket :: Prim.Parsec [Char] st Char 
lBracket = char '<'

-- | A simple '>' character.
rBracket :: Prim.Parsec [Char] st Char
rBracket = char '>'

-- | HTML style beginning tag; lTag name is the string "<" ++ name ++ ">".
lTag :: String -> Prim.Parsec [Char] st String
lTag name  -- ^ String inside brackets.
    = do
      lBracket
      spaces
      try (string name) <?> "string \"" ++ name ++ "\"" 
      spaces
      try rBracket <?> "> for lTag " ++ name 
      return name

-- | HTML style ending tag; rTag name is the string "</" ++ name ++ ">".
rTag :: String -> Prim.Parsec [Char] st String
rTag name -- ^ String inside brackets and after '/'. 
    = lTag $ "/" ++ name 

{- | All of the text from the current position until the next use of '<'. Skips over 
     the formatting characters '\n' and '\t'.
-}
skipFormatting :: Prim.Parsec String st String
skipFormatting = concat `fmap` sepEndBy1 (many1 $ noneOf "<\n\t") (many1 $ oneOf "\n\t")

{- | A group of text that is meant to be italicized. Skips over formatting characters
     '\n' and '\t'. Data should be arranged as "<italic>italicized text</italic>".
-}
italicAtom :: Prim.Parsec String st ItemAtom
italicAtom = do
             lTag "italic"
             text <- try skipFormatting <?> "text to italicize"
             try (rTag "italic") <?> "rTag italic" 
             spaces
             return $ Italic text

{- | Regular text that doesn't include "<". Skips over any use of the formatting characters
     '\n' and '\t'. This doesn't need any tags.
-}
regularAtom :: Prim.Parsec String st ItemAtom
regularAtom = do
              text <- try skipFormatting <?> "regular text other than \"<\" or \"\\n\""
              return $ JustText text

{- | A hyperlink label followed by the url. Skips over formatting characters '\n' and '\t'.
     The data should be arranged as "<hyperlink>label<url>url</hyperlink>". 
-}
hyperlinkAtom :: Prim.Parsec String st ItemAtom
hyperlinkAtom = do
                lTag "hyperlink"
                label <- try skipFormatting <?> "hyperlink label" 
                lTag "url"
                spaces
                url <- try skipFormatting <?> "hyperlink url"
                spaces
                rTag "hyperlink"
                spaces
                return $ Hyperlink {hyperlabel = label, url = url}

-- | Tag that marks use of newline in rendering. It is simply "<newline>".
newlineAtom :: Prim.Parsec String st ItemAtom
newlineAtom = do
              lTag "newline"
              spaces
              return Newline

-- | Parses a group of atoms.
atoms :: Prim.Parsec String st Item
atoms = do
        let atom =     try italicAtom
                   <|> try hyperlinkAtom
                   <|> try newlineAtom
                   <|> regularAtom
        atomlist <- many1 $ atom 
        return $ Atoms atomlist 

-- | Parse the contents of item. This is either a grouping of subtopics or a group of atoms. 
topicItem :: Prim.Parsec [Char] st Item
topicItem = do
            let subtopics = do 
                            subs <- endBy1 (try topic) spaces
                            return $ NestedTopics subs 
            -- Remember to parse subtopic before parsing atoms. 
            item <- try subtopics <|> atoms 
            return item 

{- | Parse the title of the CV. Skips formatting characters '\n' and '\t'. Data should
     be in the format "<title>Title</title>".
-}
titleBlock :: Prim.Parsec [Char] st String
titleBlock = do
             lTag "title"
             title <-skipFormatting  
             rTag "title"
             return title

{- | Parse a topic and its item. Skips formatting characters '\n' and '\t'. Data should be
     in the format "<topic>TopicName<item>Atoms Or Subtopics</topic>".
-}
topic :: Prim.Parsec [Char] st Topic 
topic = do
        lTag "topic"
        topicTitle <-skipFormatting  
        spaces
        lTag "item"
        spaces
        item <- topicItem 
        spaces
        rTag "topic"
        return $ Topic {title = topicTitle, item = item}

-- | A list of topics separated by spaces.
topicList :: Prim.Parsec [Char] st [Topic]
topicList = endBy topic spaces 
            
{- | A CV file. It should be in the format 
     "<title>CV Title (most likely your name)</title> 
      <topic>Name 1 <item> Atoms or Subtopics </topic>
      <topic> Name 2 <item> Atoms or Subtopics </topic>
      ....
-}
cvfile :: Prim.Parsec String st CV
cvfile = do
         spaces
         cvTitle <- titleBlock
         spaces
         topics <- topicList
         spaces
         eof
         return $ CV {topics = topics, cvTitle = cvTitle}
         
parseCV = parse cvfile "source name"

------------------------------
-- General Formatting Stuff
------------------------------

myTab :: Int -> String
myTab n = concat $ replicate n singleTab
    where singleTab = "    "

-------------------------------------
-- Generic CV converter
-------------------------------------

convertCV :: (CVConvertible a, Monoid a) => CV -> a  
convertCV cv = 
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
