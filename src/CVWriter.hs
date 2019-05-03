module CVWriter 
( parseCV 
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
    indent :: Ctl.State NestLevel a -- ^ Indents amount based on current nested topic level.
    makeHeader :: CV -> a -- ^ Makes header based on information inside CV.
    beginTable :: Ctl.State NestLevel a -- ^ Beginning of table based on current nested topic level. 
    endTable :: Ctl.State NestLevel a -- ^ Ending of table based on current nested topic level. 
    makeRow :: a -> a -> Ctl.State NestLevel a -- ^ Make a row based on current nested topic level.
    convertItemAtom :: ItemAtom -> Ctl.State NestLevel a -- ^ Convert an atom of info into output based on nesting level.
    convertTitle :: String -> a -- ^ Convert CV title.
    topicSeparator :: Ctl.State NestLevel a -- ^ Something to separate topics based on nesting level.
    endDoc :: a -- ^ End of the document.
    paragraph :: a -> a -- ^ Wrap stuff inside a paragraph format.

------------------------------     
-- Latex Conversion 
------------------------------

-- | Type representing strings for Latex code.
data LatexText = 
    LatexText { text :: String -- ^ The text for Latex code.
              }

convertLatex :: String -> String
convertLatex x = foldr (\c acc -> (convertChar c) ++ acc) "" x
    where convertChar '_' = "\\_"
          convertChar y = [y]

-- | LatexText is a monoid simply inherited from wrapping a String. 
instance Monoid LatexText where
    mempty = LatexText "" -- ^ Empty Latex string.
    x `mappend` y = LatexText $ (text x) `mappend` (text y) -- ^ Simply String `mappend`. 

-- | Basic elements of converting CV to Latex code.
instance CVConvertible LatexText where

    emptyCol = LatexText $ replicate 4 ' ' -- ^ Simply empty spaces. 

    -- | Number of spaces depends on nesting level.
    indent = 
        do
        nestLevel <- Ctl.get
        let spacing = replicate (4 * (nestLevel + 1)) ' ' 
        return $ LatexText spacing

    -- | Header includes document class, packages to use, and the title of the CV.
    makeHeader cv = LatexText $  
           "\\documentclass[12pt]{article}\n"
        ++ "\\usepackage{booktabs}\n"
        ++ "\\usepackage{longtable}\n"
        ++ "\\usepackage[colorlinks = true, urlcolor = blue]{hyperref}\n"
        ++ "\\usepackage[a4paper, total={6in, 8in}, margin=0.5in]{geometry}\n"
        ++ "\\input{glyphtounicode}\n"
        ++ "\\pdfgentounicode=1\n\n"
        ++ "\\begin{document}\n"
        ++ "\\begin{center}\n"
        ++ "\\textbf{\\Large " ++ cvTitle cv ++ "}\n"
        ++ "\\end{center}\n\n"

    {- | For no nesting, make sure to include no indentation. Also just use two left justified columns.
         For nesting, use a left justified column and a paragraph column.
    -}
    beginTable = 
        do
        nestLevel <- Ctl.get
        case nestLevel of 0 -> return $ LatexText "\\noindent\\begin{longtable}{ll}\n"
                          otherwise -> return $ LatexText "\\begin{tabular}{@{}lp{10cm}@{}}\n"

    -- | Just use the nesting level to determine the indentation. 
    endTable = 
        do
        nestLevel <- Ctl.get
        indentation <- indent 
        let tableType = case nestLevel of 0 -> "longtable"
                                          otherwise -> "tabular"
            endTableText =  indentation `mappend` LatexText ("\\end{" ++ tableType ++ "}\n")
        return endTableText

    {- | For no nesting, the first column is put in bold face. For higher nesting,
         we just use regular text in both columns.
    -}
    makeRow x y = 
        do
        indentation <- indent
        nestLevel <- Ctl.get
        let x' = LatexText $ convertLatex (text x) -- only convert x, y will be converted later.
            col1 = case nestLevel of
                        0 -> LatexText $ "\\textbf{" ++ text x' ++ "}" 
                        otherwise -> x'
            endOfRow = LatexText "\\\\\n"
            colSep = LatexText " & "
            rowText = indentation 
                      `mappend` col1 `mappend` colSep 
                      `mappend` y `mappend` endOfRow 
        return rowText

    -- | Just rewraps string.
    convertItemAtom (JustText x) = let x' = convertLatex x
                                   in
                                   return $ LatexText x' 
    
    -- | Wraps text in \textit Latex macro.
    convertItemAtom (Italic x) =  let x' = convertLatex x
                                  in
                                  return $ LatexText ("\\textit{" ++ x' ++ "}")

    -- | Uses the \href Latex macro from the hyperref Latex package.
    convertItemAtom (Hyperlink {hyperlabel = label, url = url}) 
        = let label' = convertLatex label
              url' = convertLatex url
          in
          return  $ LatexText ("\\href{" ++ url' ++ "}{" ++ label' ++ "}")
    
    -- | Use a tabular \newline (note this isn't the "\\" newline). Use the nesting level for indentation.
    convertItemAtom Newline = 
        do
        indentation <- indent 
        let newLineText = LatexText "\\newline\n" `mappend` indentation 
        return newLineText

    -- | Just rewrap String.
    convertTitle x = LatexText x

    -- | Use \midrule in all cases. Use the nesting level for indentation. 
    topicSeparator = 
        do
        indentation <- indent
        return $ indentation `mappend` LatexText "\\midrule\n"

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

    -- | Use appropriate number of ' ' characters to indent based on the nesting level. 
    indent = 
        do
        nestLevel <- Ctl.get
        let nSpaces = 4 * (nestLevel + 1)
            spaces = replicate nSpaces ' '
        return $ JekyllText spaces
  
    -- | Header contain's information for Jekyll followed by Header tags. 
    makeHeader cv = JekyllText $ 
           "---\n"
        ++ "layout: default\n"
        ++ "title: " ++ cvTitle cv ++ "'s CV\n"
        ++ "---\n\n" 
        ++ "<h2> <a href = \"{{site . url}}/cv/MatthewMcGonagleCV.pdf\">Click here</a> if you wish to view my CV as a pdf.</h2>\n"
        ++ "<h1>" ++ cvTitle cv ++ "</h1>\n"

    -- | Don't use the nesting level at all. 
    beginTable = return $ JekyllText " <table>\n"

    -- | Nested level only affects the indentation level.
    endTable = 
        do
        indentation <- indent
        return $ indentation `mappend` JekyllText "</table>\n"

    {- | For nested level 0, the first column is inside table header <th> tags. For higher nesting
         levels, both columns are wrapped with <td> tags.
    -}
    makeRow x y = 
        do
        indentation <- indent 
        nestLevel <- Ctl.get
        Ctl.put $ nestLevel + 1
        extraIndent <- indent
        Ctl.put $ nestLevel
        let col1 = case nestLevel of 
                            0 -> JekyllText "<th>" `mappend` x `mappend` JekyllText "</th>\n"
                            otherwise -> JekyllText "<td>" `mappend` x `mappend` JekyllText "</td>\n"
                                
            col2 = JekyllText "<td>" `mappend` y `mappend` JekyllText "</td>\n"
            rowText = indentation `mappend` JekyllText "<tr>\n"
                      `mappend` extraIndent `mappend` col1
                      `mappend` extraIndent `mappend` col2
                      `mappend` indentation `mappend` JekyllText "</tr>\n"
        return rowText

    -- | Simply rewrap the string inside a JekyllText type.
    convertItemAtom (JustText x) = return $ JekyllText x

    -- | Put the text inside italic tags.
    convertItemAtom (Italic x) =  return $ JekyllText ("<i>" ++ x ++ "</i>")

    -- | Appropriately add the label and url using the <a href = "url"> label </a> tags.
    convertItemAtom (Hyperlink {hyperlabel = label, url = url}) = 
        return $ JekyllText ("<a href = \"" ++ url ++ "\">" ++ label ++ "</a>")

    {- | Rendered html will contain a newline in this column using the <br/> tag. Nesting level
         is simply for making the appropriate indentation.
    -}
    convertItemAtom Newline = 
        do
        indentation <- indent
        return $ JekyllText "<br/>\n" `mappend` indentation 

    -- | Simply wraps String inside the JekyllText type.
    convertTitle x = JekyllText x

    -- | Topic Separator is a horizontal line with width given by the current table/sub-table width.
    topicSeparator = 
        do
        indentation <- indent
        let lineText = JekyllText "<td colspan = \"100%\"; style = \"border-bottom: 1px solid black\"></td>\n"
            sepText =  
                indentation `mappend` JekyllText "<tr>\n"
                `mappend` indentation `mappend` lineText 
                `mappend` indentation `mappend` JekyllText "</tr>\n"
        return sepText 

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
         
parseCV = parse cvfile "source .cv file"

-------------------------------------
-- Generic CV converter
-------------------------------------

{- | For a type where primitive CV converter functions are defined and is a monoid,
     turn a CV type into this type.
-}
convertCV :: (CVConvertible a, Monoid a) => CV -> a  
convertCV cv -- ^ The CV to convert.
    = 
                 makeHeader cv 
       `mappend` body 
       `mappend` endDoc
       where body = fst $ topicsWIndent
             topicsWIndent = Ctl.runState (convertTopicList $ topics cv) 0

-- | Convert a list of topics. We need to include the state of the current nesting level.
convertTopicList :: (CVConvertible a, Monoid a) => [Topic] -> Ctl.State NestLevel a

-- | Empty list just goes to mempty.
convertTopicList [] = return mempty

-- | For a list of one object we just convert the single topic and put it in a table.
convertTopicList [t] = 
    do 
    beginTableText <- beginTable
    t' <- convertTopic t
    endTableText <- endTable
    return $ beginTableText
             `mappend` t' 
             `mappend` endTableText

{- | For a list of two or more objects, we just do a simple conversion for the first topic.
     For the other topics, we first put in a topic separator before converting.
-}
convertTopicList (t:ts) = 
    do
    beginTableText <- beginTable
    nestLevel <- Ctl.get
    t' <- convertTopic t
    let convertTailTopic x = 
            do
            sep <- topicSeparator
            x' <- convertTopic x
            return $ sep `mappend` x' 
    ts' <- mapM convertTailTopic ts 
    endTableText <- endTable
    return $ beginTableText
             `mappend` t' 
             `mappend` mconcat ts' 
             `mappend` endTableText

-- | We convert the title and item of the topic. 
convertTopic :: (CVConvertible a, Monoid a) => Topic -> Ctl.State NestLevel a
convertTopic x = do
                 x' <- convertItem $ item x
                 makeRow (convertTitle $ title x) x'

-- | Convert the item of a topic. 
  
convertItem :: (CVConvertible a, Monoid a) => Item -> Ctl.State NestLevel a
{- | In the case that the item is just a collection of atoms, then we just  
     combine them.
-}
convertItem (Atoms atoms) = 
    do
    atoms' <- mapM convertItemAtom atoms
    return $ mconcat atoms'

{- In the case that the item is a list of subtopics, we need to increase
   the nesting level state before converting the subtopics.
-}
convertItem (NestedTopics ts) = 
    do
    nestedLevel <- Ctl.get
    Ctl.put $ nestedLevel + 1
    ts' <- convertTopicList ts 
    Ctl.put nestedLevel
    return ts'
