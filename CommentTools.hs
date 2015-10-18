{-
    Handles all the functionality surrounding the commenting ability. This
    covers things like everything to do with reading and writing comments and
    provides lots of helper functions.
    
    Author(s):     Lewis Deane
    License:       MIT
    Last Modified: 18/10/2015
-}

module CommentTools (setComment, appendComment, updateComment, deleteComment, currentComment) where

    -- Imports for things we will need.
    import Control.Applicative
    import Data.List (isPrefixOf)
    import Data.Time.Calendar
    import Data.Time.Clock
    import System.Directory
    import System.IO

    import qualified Config as C
    import LangTools

    -- Useful type synonyms.
    type FileName = String
    type Comment  = String
    type Params   = [String]
    type Line     = String
    type Lines    = [Line]
    type Field    = (String, String)
    type Fields   = [Field]


    -- Writes a comment to filename with the associated params.
    write :: FileName -> Comment -> Lines -> Params -> IO ()

    write fname com content params = do
        let lang     = getLang fname
            content' = removeCommentBlock lang content

        f     <- getFields params
        block <- generateCommentBlock lang com f

        writeToFile fname $ block ++ content'


    -- Opens a temp file, writes content to it. It does this all safely.
    writeToFile :: FileName -> Lines -> IO ()

    writeToFile fname content = do
        (tempName, tempHandle) <- openTempFile "." "temp"

        hPutStr tempHandle $ unlines content
        hClose  tempHandle

        removeFile fname
        renameFile tempName fname 


    -- Sets a comment to a file and applies the params if needed.
    setComment :: FileName -> Comment -> Params -> IO ()

    setComment fname com params = do
        content <- lines <$> readFile fname
        write fname com content params


    -- Appends a comment to a file with the appropriate parameters.
    appendComment :: FileName -> Comment -> Params -> IO ()

    appendComment fname com params = do
        content <- lines <$> readFile fname

        let lang   = getLang fname
            oldCom = getComment lang content

        write fname (oldCom ++ " " ++ com) content params


    -- Updates the comment block if there is any new information.
    updateComment :: FileName -> Params -> IO ()

    updateComment fname params = do
        content <- lines <$> readFile fname

        let lang   = getLang fname
            oldCom = getComment lang content

        write fname oldCom content params


    -- Deletes a comment block from a file.
    deleteComment :: FileName -> IO ()

    deleteComment fname = do
        let lang = getLang fname
        content <- lines <$> readFile fname
        writeToFile fname $ removeCommentBlock lang content


    -- Retrieves the current comment from a file.
    currentComment :: FileName -> IO ()

    currentComment fname = do
        let lang = getLang fname
        content <- lines <$> readFile fname
        (putStrLn . unlines) $ getCommentBlock lang content


    -- Removes the current comment section.
    removeCommentBlock :: Lang -> Lines -> Lines

    removeCommentBlock lang content = dropWhile (isInCommentBlock lang) content


    -- Gets the comment block if it exists.
    getCommentBlock :: Lang -> Lines -> Lines

    getCommentBlock lang content = if head content /= getBlockStart lang then error "No comment block found. Run 'comet' for a list of legal commands." else takeWhile (isInCommentBlock lang) content


    -- Checks to see if the given line begin with a comment lies within a comment block.
    isInCommentBlock :: Lang -> Line -> Bool

    isInCommentBlock lang line = any (`isPrefixOf` line) [getBlockStart lang, getCommentChar lang, getBlockEnd lang]


    -- Gets a the comment from a bunch of lines making up the file.
    getComment :: Lang -> Lines -> Comment

    getComment lang content = if head content /= getBlockStart lang then error "No comment found. Run 'comet' for a list of legal commands." else (unlines . map (strip lang) . takeWhile (\x -> (trim . strip lang) x /= "" && trim x /= getBlockEnd lang)) $ tail content


    -- Adds a comment character to the start of a string needing commenting.
    comment :: Lang -> Line -> Line

    comment lang line = getCommentChar lang ++ line


    -- Generates the comment block to be added to the header of the file.
    generateCommentBlock :: Lang -> Comment -> Fields -> IO Lines

    generateCommentBlock lang com fields = do
        splitLines <- splitInput lang com
        return $ [getBlockStart lang] ++ map (comment lang) splitLines ++ map (comment lang) (generateFieldBlock lang fields) ++ [getBlockEnd lang]


    -- Creates a correctly formatted field block that will go in the comment block.
    generateFieldBlock :: Lang -> Fields -> Lines

    generateFieldBlock _ []        = []
    generateFieldBlock lang fields = [""] ++ map (\a -> fst a ++ ":" ++ rep " " (s a) ++ snd a) fields
                                     where mlen = foldl max 0 len
                                           len  = map (succ . length . fst) fields
                                           s y  = mlen - ((length . fst) y)


    -- Replicates a string an amount of times.
    rep :: String -> Int -> String

    rep s 0 = ""
    rep s n = s ++ rep s (n - 1)


    -- Takes a string and chops off any whitespace at either end.
    trim :: String -> String

    trim x = let f = reverse . dropWhile (\x -> x == ' ' || x == '\t') in (f . f) x


    -- Removes the comment character preceding the line.
    strip :: Lang -> Line -> Line

    strip lang = drop ((length . getCommentChar) lang)
    

    -- Takes a string and wraps the length according the the comment width setting.
    splitInput :: Lang -> Comment -> IO Lines

    splitInput l c = do
        lim <- read <$> C.readValue "comment-width"
  
        let lim' = lim - (length $ getCommentChar l)
            w = words c
            x = foldl f [] w
            f [] x   = [x]
            f acc [] = acc
            f acc x  = if (length . last) acc + length x < lim' then init acc ++ [last acc ++ " " ++ x] else acc ++ [x]

        return x


    -- List of the fields that can be shown in the header.
    fields :: [String]

    fields = ["Author(s)", "License", "Last Modified"]


    -- Turns the shorthand notation into the full version.
    getFieldsFromParams :: [String] -> [String]

    getFieldsFromParams p = map f p
                        where f "-l"  = "License"
                              f "-a"  = "Author(s)"
                              f "-lm" = "Last Modified"
                              f x     = error "No such parameter '" ++ x ++ "'."


    -- Zips the possible parameters against their current value.
    fieldify :: IO Fields

    fieldify = do
      a <- author
      l <- license
      d <- date
      return $ zip fields [a, l, d]


    -- Returns a list of fields that we want after filtering out ones not wanted.
    getFields :: [String] -> IO Fields

    getFields params = fieldify >>= (return . filter (\x -> not $ (fst x) `elem` (getFieldsFromParams params)))


    -- Gets the value associated with the 'author' key in the config file.
    author :: IO String

    author = C.readValue "author"


    -- Gets the value associated with the 'license' key in the config file.
    license :: IO String

    license = C.readValue "license"


    -- Gets the value associated with the 'comment-width' key in the config file.
    commentWidth :: IO String

    commentWidth = C.readValue "comment-width"


    -- Gets todays date.
    date :: IO String

    date = getCurrentTime >>= f . toGregorian . utctDay where f (y, m, d) = return $ show d ++ "/" ++ show m ++ "/" ++ show y
