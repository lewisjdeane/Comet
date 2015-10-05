{-
	Handles all the necessary functions relating to the
	commenting ability of the app.
	
	Author(s):     Lewis Deane
	Last Modified: 20/9/2015
-}

module ComTools (currentComment, setComment, appendComment, deleteComment) where

    -- Imports
    import Control.Applicative
    import Data.Char
    import Data.List (elemIndex, elemIndices, isPrefixOf)
    import Data.List.Split
    import Data.Time.Calendar
    import Data.Time.Clock
    import System.Directory
    import System.IO

    import qualified Config as C

    -- Data type composed of the supported languages.
    data Lang = C | CPP | CoffeeScript | CSharp | CSS | ERB | Go | HAML | Haskell | HTML | Java | JavaScript | MatLab | PHP | Python | R | Ruby | Scala | SASS | SCSS | XML deriving (Eq, Show)

    -- Type synoyms to make the code more readable.
    type FileName = String
    type Comment  = String
    type Line     = String
    type Field    = String
    type Content  = String

    -- Move these two functions to main?
    -- List of the fields that can be shown in the header.
    fields :: [Field]

    fields = ["Author(s)", "License", "Last Modified"]

    getFieldsFromChar :: [String] -> [Field]

    getFieldsFromChar p = map f p
                        where f "-l"  = "License"
                              f "-a"  = "Author(s)"
                              f "-lm" = "Last Modified"
                              f x     = error "No such parameter '" ++ x ++ "'."


    getParams :: [Field] -> IO [(Field, Content)]

    getParams params = zippity >>= (return . filter (\x -> not $ (fst x) `elem` (getFieldsFromChar params)))

    -- Map over lines.
    -- Check if any of these lines
    getCurrentParams :: [Line] -> Lang -> [(Field, Content)]

    getCurrentParams l lang = map fmat $ takeWhile f $ dropWhile (not . f) l
      where f x  = any id $ (map (\y -> y `isPrefixOf` x) (((++) (getCommentChar lang)) <$> fields))
            fmat z = (\a -> (trim (a !! 0), trim (a !! 1))) ((parts . d) z)
            parts = splitOn ":"
            d = drop ((length . getCommentChar) lang)


    -- Sets a comment to the desired file.
    setComment :: FileName -> Comment -> [Field] -> IO ()

    setComment f c params = getParams params >>= writeComment' f c


    appendComment :: FileName -> Comment -> [Field] -> IO ()

    appendComment f c params = do
      let lang = getLang f

      l <- lines <$> readFile f
      p <- if length params == 0 then return (getCurrentParams l lang) else getParams params
      c' <- getComment l lang

      writeComment' f (c' ++ " " ++ c) p


    -- Write comment should take a file name, a comment and the field-content array.
    writeComment' :: FileName -> Comment -> [(Field, Content)] -> IO ()

    writeComment' fname comment fields = do
      let lang = getLang fname

      content   <- lines <$> readFile fname
      content'  <- removeIfComment content lang
      comBlock <-  generateCommentBlock comment fields lang
      
      let c'' = comBlock ++ content'

      (tempName, tempHandle) <- openTempFile "." "temp"

      hPutStr tempHandle $ unlines c''
      hClose  tempHandle

      removeFile fname
      renameFile tempName fname 

    
    zippity :: IO [(Field, Content)]

    zippity = do
      a <- getAuthor
      l <- getLicense
      d <- getDate
      return $ zip fields [a, l, d]


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
    getComment :: [Line] -> Lang -> IO Comment

    getComment c l = do
      a <- getCommentBlock c l
      getCommentFromBlock a l

    
    generateCommentBlock :: Comment -> [(Field, String)] -> Lang -> IO [Line]

    generateCommentBlock c x l = do
      s <- splitInput l c
      return $ [getBlockStart l] ++ (lines . comment l) s ++ generateFields x l ++ [getBlockEnd l]


    generateFields :: [(Field, Content)] -> Lang -> [Line]

    generateFields [] _ = []
    generateFields x l  = [getCommentChar l] ++ map (\a -> getCommentChar l ++ fst a ++ ":" ++ rep " " (s a) ++ snd a) x
                        where mlen = foldl max 0 len
                              len  = map ((+) 1 . length . fst) x
                              s y  = mlen - ((length . fst) y)


    -- Replicates a String an amount of times.
    rep :: String -> Int -> String

    rep s 0 = ""
    rep s n = s ++ rep s (n - 1)


    -- Gets a list of lines relating to the lines within the block comment.
    getCommentBlock :: [Line] -> Lang -> IO [Line]

    getCommentBlock c l = do
      let f x = (length . filter (`isPrefixOf` x)) [getCommentChar l, getBlockStart l, getBlockEnd l] > 0
          a   = takeWhile f c

      return a


    -- Rewrite
    getCommentFromBlock :: [Line] -> Lang -> IO Comment

    getCommentFromBlock s l = do
      let a = map (drop (length $ getCommentChar l)) ((init . tail) s)
      (return . unlines . takeWhile (/= "") . map trim) a


    -- Takes a string and wraps the length according the the comment width setting.
    splitInput :: Lang -> Comment -> IO String

    splitInput l c = do
      lim <- read <$> C.readValue "comment-width"
      
      let lim' = lim - (length $ getCommentChar l)
          w = words c
          x = foldl f [] w
          f [] x   = [x]
          f acc [] = acc
          f acc x  = if (length . last) acc + length x < lim' then init acc ++ [last acc ++ " " ++ x] else acc ++ [x]

      (return . unlines) x


    -- Removes the current comment section.
    removeIfComment :: [Line] -> Lang -> IO [Line]

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


    -- Gets the author value from the config file.
    getAuthor :: IO String

    getAuthor = C.readValue "author"


    getLicense :: IO String

    getLicense = C.readValue "license"


    -- Gets todays date in a formatted string.
    getDate :: IO String

    getDate = getCurrentTime >>= f . toGregorian . utctDay where f (y, m, d) = return $ show d ++ "/" ++ show m ++ "/" ++ show y


    -- NOTE: Is there any way we can make this more concise? Perhaps zipwith?
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
              where suf = suffix f


    -- Takes a filename and returns its file extension e.g. test.hs returns ".hs" and main.html.erb returns ".erb".
    suffix :: FileName -> String

    suffix = (:) '.' . map toLower . last . splitOn "."


    -- Takes a string and chops off any whitespace at either end.
    trim :: String -> String

    trim x = let f = reverse . dropWhile (\x -> x == ' ' || x == '\t') in (f . f) x


    -- NOTE: This could do with tidying up.
    -- Comments a string in the style of the language.
    comment :: Lang -> Comment -> String

    comment l s | s == ""   = getCommentChar l
                | otherwise = (init . unlines . map ((++) $ getCommentChar l) . lines) s
