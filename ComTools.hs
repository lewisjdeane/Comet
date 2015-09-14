{-
	Handles all the necessary functions relating to the commenting ability of the
	app.
	
	Author(s):     Lewis Deane
	Last Modified: 13/9/2015
-}

module ComTools (currentComment, setComment, appendComment, deleteComment) where

    -- Imports
    import System.IO
    import Control.Applicative
    import System.Directory
    import Data.Char
    import Data.Time.Clock
    import Data.Time.Calendar
    import Data.List (elemIndex, elemIndices, isPrefixOf)

    import Config as C

    -- Data type composed of the supported languages.
    data Lang = C | CPP | CoffeeScript | CSharp | CSS | ERB | Go | HAML | Haskell | HTML | Java | JavaScript | MatLab | PHP | Python | R | Ruby | Scala | SASS | SCSS | XML deriving (Eq, Show)
    
    -- Type synoyms to make the code more readable.
    type FileName = String
    type Comment  = String

    -- Sets a comment to the desired file.
    setComment :: FileName -> Comment -> IO ()

    setComment f c = do
        let l = getLang f

        content <- lines <$> readFile f

        writeComment c content l f

    
    -- Appends a comment to the desired file.
    appendComment :: FileName -> Comment -> IO ()

    appendComment f c = do
      let l = getLang f

      content <- lines <$> readFile f
      current <- getComment content l

      let n = current ++ " " ++ c

      writeComment n content l f


    -- Writes a comment to a file, file is read beforehand in either setComment or appendComment.
    writeComment :: String -> [String] -> Lang -> FileName -> IO ()

    writeComment s c l f = do
      d  <- getDate
      a  <- getAuthor
      s' <- splitInput l s
      c' <- removeIfComment c l

      let d'  = "Last Modified: " ++ d
          a'  = "Author(s):     " ++ a
          h   = [getBlockStart l, comment l s', comment l "", comment l a', comment l d', getBlockEnd l] 
          c'' = unlines $ h ++ c'

      (tempName, tempHandle) <- openTempFile "." "temp"

      hPutStr tempHandle c''
      hClose  tempHandle

      removeFile f
      renameFile tempName f


    -- Deletes the comment from the current file.
    deleteComment :: FileName -> IO ()

    deleteComment f = do
      let l = getLang f

      c <- lines <$> readFile f
      b <- getCommentBlock c l

      let c' = drop (length b) c

      (tempName, tempHandle) <- openTempFile "." "temp"

      hPutStr tempHandle $ unlines c'
      hClose  tempHandle

      removeFile f
      renameFile tempName f


    -- Gets the comment from the file and prints it.
    currentComment :: FileName -> IO ()

    currentComment f = do
      let l = getLang f

      content <- lines <$> readFile f

      (++) <$> pure "\n" <*> getComment content l >>= putStrLn


    -- Gets the comment from the lines from the file.
    getComment :: [String] -> Lang -> IO String

    getComment c l = do
      a <- getCommentBlock c l
      getCommentFromBlock a l


    -- Gets a list of lines relating to the lines within the block comment.
    getCommentBlock :: [String] -> Lang -> IO [String]

    getCommentBlock c l = if hasBlockComments l then extract c l else extract' c l


    getCommentFromBlock :: [String] -> Lang -> IO String

    getCommentFromBlock s l = do
      let a = map (drop (length $ getCommentChar l)) ((init . tail) s)
      (return . unlines . takeWhile (/= "") . map trim) a


    -- Extracts the comment block for languages with block comments.
    extract :: [String] -> Lang -> IO [String]

    extract c l = do
      let k x = length (filter (`isPrefixOf` x) [getCommentChar l, getBlockStart l, getBlockEnd l]) > 0
      return $ takeWhile k c


    -- Same as extract but handles languages that don't have block comments.
    extract' :: [String] -> Lang -> IO [String]

    extract' c l = do
      let k x = getCommentChar l `isPrefixOf` x
      return $ takeWhile k c


    -- Takes a string and wraps the length according the the comment width setting.
    splitInput :: Lang -> String -> IO String

    splitInput l c = do
      lim <- read <$> C.getValue "comment-width"
      
      let lim' = lim - (length $ getCommentChar l)
          w = words c
          x = f [] w
          f [] (x:xs)  = f [x] xs
          f acc []     = acc
          f acc (x:xs) = if (length . last) acc + length x < lim' then f (init acc ++ [(last acc ++ " " ++ x)]) xs else f (acc ++ [x]) xs

      (return . unlines) x


    -- Removes the current comment section.
    removeIfComment :: [String] -> Lang -> IO [String]

    removeIfComment c l = do
      b <- getCommentBlock c l
      (return . drop (length b)) c


    -- Gets the string relating to the start of a block comment for the supported languages.
    getBlockStart :: Lang -> String

    getBlockStart l | l `elem` [C, CPP, CSharp, CSS, Go, Java, JavaScript, PHP, Scala, SCSS, SASS] = "/*"
                    | l `elem` [ERB, HTML, XML]                                                    = "<!--"
                    | l `elem` [Haskell]                                                           = "{-"
                    | l `elem` [MatLab]                                                            = "%{"
                    | otherwise                                                                    = getCommentChar l


    -- Gets the string relating to the end of a block comment for the supported languages.
    getBlockEnd :: Lang -> String

    getBlockEnd l | l `elem` [C, CPP, CSharp, CSS, Go, Java, JavaScript, PHP, Scala, SCSS, SASS] = " */"
                  | l `elem` [ERB, HTML, XML]                                                    = "-->"
                  | l `elem` [Haskell]                                                           = "-}"
                  | l `elem` [MatLab]                                                            = "%}"
                  | otherwise                                                                    = getCommentChar l


    -- Gets the string relating to the start of each comment line within a block comment.
    getCommentChar :: Lang -> String

    getCommentChar l | l `elem` [C, CPP, CSharp, CSS, Go, Java, JavaScript, PHP, Scala, SCSS, SASS] = " * "
                     | l `elem` [CoffeeScript, Python, R, Ruby]                                     = "# "
                     | l `elem` [HAML]                                                              = "-# "
                     | l `elem` [ERB, Haskell, HTML, MatLab, XML]                                   = "\t"


    -- Says which languages support block comments.
    hasBlockComments :: Lang -> Bool

    hasBlockComments l | l `elem` [C, CPP, CSharp, CSS, ERB, Go, Haskell, HTML, MatLab, Java, JavaScript, PHP, Scala, SCSS, SASS, XML] = True
                       | otherwise                                                                                                     = False


    -- Gets the author value from the config file.
    getAuthor :: IO String

    getAuthor = C.getValue "author"


    -- Gets todays date in a formatted string.
    getDate :: IO String

    getDate = getCurrentTime >>= f . toGregorian . utctDay where f (y, m, d) = return $ show d ++ "/" ++ show m ++ "/" ++ show y


    -- Gets the language from the file extension.
    getLang :: FileName -> Lang

    getLang f | suf == ".c"      = C
              | suf == ".h"      = C
              | suf == ".cpp"    = CPP
              | suf == ".coffee" = CoffeeScript
              | suf == ".cs"     = CSharp
              | suf == ".css"    = CSS
              | suf == ".erb"    = ERB
              | suf == ".go"     = Go
              | suf == ".haml"   = HAML
              | suf == ".hs"     = Haskell
              | suf == ".html"   = HTML
              | suf == ".htm"    = HTML
              | suf == ".xhtml"  = HTML
              | suf == ".java"   = Java
              | suf == ".js"     = JavaScript
              | suf == ".matlab" = MatLab
              | suf == ".php"    = PHP
              | suf == ".py"     = Python
              | suf == ".r"      = R
              | suf == ".rb"     = Ruby
              | suf == ".scala"  = Scala
              | suf == ".sass"   = SASS
              | suf == ".scss"   = SCSS
              | suf == ".xml"    = XML
              | otherwise        = error $ suf ++ " files are not supported at this time."
              where suf = getFileSuffix f


    -- Gets the file ending from the file name.
    getFileSuffix :: String -> String

    getFileSuffix str = map toLower $ drop (last $ elemIndices '.' str) str 


    -- Takes a string and chops off any whitespace at either end.
    trim :: String -> String

    trim x = let f = reverse . dropWhile (\x -> x == ' ' || x == '\t') in (f . f) x


    -- Comments a string in the style of the language.
    comment :: Lang -> String -> String

    comment l s | s == ""   = getCommentChar l
                | otherwise = c l s
                where c l' s' = (init . unlines . map (\x -> getCommentChar l' ++ x) . lines) s'
