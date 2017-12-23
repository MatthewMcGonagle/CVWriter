# CVWriter

The purpose of this small library is to provide functions to turn an XML-like source file of CV information into two formats:
1. A Jekyll compatible .html file. This is ready to be turned into an HTML version of a CV that can be used by Jekyll to add to a website.
2. A Latex compatible .tex file that is ready to be turned into a .pdf file by simply processing it with `pdflatex`.

This allows one to create two versions of a CV from a single .cv file. An example file is given by the file `info.cv` in this repository.

# Format of .cv Files

The format of a .cv (as seen in `info.cv`) is as follows:

```
<title> Your name </title>

<topic>
Topic 1
<item>
Put something about topic 1 here.
</topic>

<topic>
Topic 2
<item>
    <topic> 
    Subtopic 2a
    <item>
    Put something about Subtopic 2a here.
    </topic>

    <topic>
    Subtopic 2b
    <item>
    Put something about Subtopic 2b here. <newline>
    Put this <italic> italicized text </italic> on a new line. <newline>
    On this new line, the user will see <hyperlink> this text <url> myUrl </hyperlink> 
            as a link to the url myUrl.
    </topic>
</topic>
```
Each topic are put into the first column of the overall table. The information after `<item>` is put into
the second column. The second column is capable of having a sub-table that is captured by using subtopics.
Subtopics are placed after the `<item>` tag of the parent topic.

The use of tabs and newlines are up to you. The parser ignores white space after tags (including newlines and tabs). 
Furthermore, the newlines are always ignored; so use them freely for formatting. If you wish to put some text on
a newline for the final output, then use the `<newline>` tag.

If you wish for text to be italicized, then put it between the `<italic>` tag and the `</italic>` tag.

For a hyperlink, put the label as it appears to the user between the `<hyperlink>` tag and the `<url>` tag. Then put the actual url between the `<url>` tag and the `</hyperlink>` tag. 

# Converting a .cv File

An example of how to do this is provided in `src/Main.hs` inside this repository. After the `.cv` file is opened, we parse its contents into the data type `CV` using the function `parseCV :: String -> Either ParseError CV`. The parsing uses the standard Parsec library, so it keeps track of Errors using the `Either` monad.

Then we can use the function `convertCV :: CV -> LatexText` and `convertCV :: CV -> JekyllText` to turn the `CV` type into Latex text and Jekyll text, respectively.


