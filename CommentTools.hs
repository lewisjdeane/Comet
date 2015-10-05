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
            let lang = getLang fname
            
            f     <- getFields params
            block <- generateCommentBlock lang com f
            
            writeToFile fname $ block ++ content


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
            oldCom  <- getComment content

            write fname (oldCom ++ " " ++ com) content params


        -- Updates the comment block if there is any new information.
        updateComment :: FileName -> Params -> IO ()

        updateComment fname params = putStrLn "Hello"


        -- Deletes a comment block from a file.
        deleteComment :: FileName -> IO ()

        deleteComment fname = do
            putStrLn "Deleted."


        -- Retrieves the current comment from a file.
        currentComment :: FileName -> IO ()

        currentComment fname = do
            content <- lines <$> readFile fname
            getComment content >>= putStrLn


        -- Removes the current comment section.
        removeCommentBlock :: Lang -> Lines -> Lines

        removeCommentBlock lang content = dropWhile (isInCommentBlock lang) content


        getCommentBlock :: Lang -> Lines -> Lines

        getCommentBlock lang content = takeWhile (isInCommentBlock lang) content


        -- Checks to see if the given line begin with a comment lies within a comment block.
        isInCommentBlock :: Lang -> Line -> Bool

        isInCommentBlock lang line = any (`isPrefixOf` line) [getBlockStart lang, getCommentChar lang, getBlockEnd lang]


        -- Gets a the comment from a bunch of lines making up the file.
        getComment :: Lines -> IO Comment

        getComment content = return "Hello"


        -- Adds a comment character to the start of a string needing commenting.
        comment :: Lang -> Line -> Line

        comment lang line = getCommentChar lang ++ line


        -- Generates the comment block to be added to the header of the file.
        generateCommentBlock :: Lang -> Comment -> Fields -> IO Lines

        generateCommentBlock lang com fields = do
            splitLines <- splitInput fname com
            return $ [getBlockStart lang] ++ map (comment lang) splitLines ++ map (comment lang) ["License: BSD"] ++ [getBlockEnd lang]

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
        

        -- Needs rewriting.
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


        fieldify :: IO Fields

        fieldify = do
          a <- author
          l <- license
          d <- date
          return $ zip fields [a, l, d]


        getFields :: [String] -> IO Fields

        getFields params = fieldify >>= (return . filter (\x -> not $ (fst x) `elem` (getFieldsFromParams params)))



        author :: IO String

        author = C.readValue "author"


        license :: IO String

        license = C.readValue "license"


        commentWidth :: IO String

        commentWidth = C.readValue "comment-width"


        date :: IO String

        date = getCurrentTime >>= f . toGregorian . utctDay where f (y, m, d) = return $ show d ++ "/" ++ show m ++ "/" ++ show y