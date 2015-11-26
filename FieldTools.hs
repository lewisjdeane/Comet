{-
    This file handles the passing of parameters and manages things to do with
    the fields within a header comment.
    
    Author(s):     Lewis Deane
    License:       MIT
    Last Modified: 14/11/2015
-}

module FieldTools (getFields) where

    import Control.Applicative
    import Data.List (isPrefixOf, sortBy, elemIndex)
    import Data.List.Split (splitOn)
    import Data.String.Utils (replace)
    import Data.Time.Calendar
    import Data.Time.Clock

    import qualified Config as C

    data Field = Author | License | LastModified | Maintainer deriving (Show, Eq)
    data State = Visible | Hidden | Custom String deriving (Show, Eq)
    data FieldState = FieldState { field :: Field, state :: State } deriving (Show)

    type Params = [String]


    -- Define the order in which we want the various fields to appear in.
    fieldOrder :: [Field]

    fieldOrder = [Author, Maintainer, License, LastModified]


    -- Sort the field order by the order in which we defined above.
    sortFields :: [FieldState] -> [FieldState]

    sortFields x = sortBy f x
                 where f a b = compare (k a) (k b)
                       k y   = (field y) `elemIndex` fieldOrder


    -- Parse the parameters passed to the function so we can work out what we want with each field.
    parseParams :: Params -> [FieldState]

    parseParams [] = []
    parseParams (x:xs) = if "-" `isPrefixOf` x
                                then FieldState { field = field', state = Hidden } : (parseParams xs)
                                else if length xs > 0
                                        then if (f . head) xs
                                                then FieldState { field = field', state = Custom (head xs)} : (parseParams (tail xs))
                                                else FieldState { field = field', state = Visible} : (parseParams xs)
                                        else FieldState { field = field', state = Visible } : (parseParams xs)
                         where field' = (getFieldFromShortcut . tail) x
                               f a    = not $ any (\x -> x `isPrefixOf` a) ["-", "+"]


    -- Merges the default fields with the parameters we have just passed.
    mergeWithDefaultFields :: [FieldState] -> IO [FieldState]

    mergeWithDefaultFields fields = do
        def <- map (\x -> FieldState {field = x, state = Visible}) <$> getDefaultFields
        (return . sortFields) $ merge def fields


    -- Define what we want to happen when we are merging fields.
    merge :: [FieldState] -> [FieldState] -> [FieldState]

    merge def fields = foldl f def fields
                     where f list e  = if any (\z -> field z == field e) list then map (m e) list else e : list
                           m new old = if field new == field old
                                            then new
                                            else old


    -- Define what value we want respective fields to take.
    fieldValue :: Field -> IO String

    fieldValue x | x == Author       = author
                 | x == Maintainer   = maintainer
                 | x == License      = license
                 | x == LastModified = date
                 | otherwise         = error "No such field."


    -- Define what title we want respective fields to have.
    fieldTitle :: Field -> String

    fieldTitle x | x == Author       = "Author(s)"
                 | x == Maintainer   = "Maintainer(s)"
                 | x == License      = "License"
                 | x == LastModified = "Last Modified"
                 | otherwise         = error "No such field."
     

    -- Gets the default fields.
    getDefaultFields :: IO [Field]

    getDefaultFields = (map getFieldFromShortcut . splitOn ",") <$> C.readValue "default-fields"


    -- Gets the appropriate field from the shortcut.
    getFieldFromShortcut :: String -> Field

    getFieldFromShortcut x | x == "a"  = Author
                           | x == "m"  = Maintainer
                           | x == "l"  = License
                           | x == "lm" = LastModified
                           | otherwise = error $ x ++ " is not a valid field."


    -- Returns a list of fields that we want after filtering out ones not wanted.
    getFields :: Params -> IO [(String, String)]

    getFields fields = do
        merged <- (mergeWithDefaultFields . parseParams) fields

        let merged'  = filter (not . isHidden) merged
            isHidden = (\x -> state x == Hidden)

        sequence $ map extractValue merged'


    -- Pairs a field with it value. Uses the default value unless the state is 'Custom', then we take the custom value.
    extractValue :: FieldState -> IO (String, String)

    extractValue FieldState { field = a, state = b } = if isCustom b
                                                            then return $ (fieldTitle a, getCustomValue b)
                                                            else fieldValue a >>= (return . (\y -> (fieldTitle a, y)))


    -- Decides if the state passed in is 'Custom' or not.
    isCustom :: State -> Bool

    isCustom (Custom _) = True
    isCustom _          = False


    -- Gets the value from inside the 'Custom' wrapper.
    getCustomValue :: State -> String

    getCustomValue (Custom x) = x
    getCustomValue _          = error "Custom state not passed to function."


    -- Gets the value associated with the 'author' key in the config file.
    author :: IO String

    author = C.readValue "author"


    -- Gets the value associated with the 'maintainer' key in the config file.
    maintainer :: IO String

    maintainer = C.readValue "maintainer"


    -- Gets the value associated with the 'license' key in the config file.
    license :: IO String

    license = C.readValue "license"


    -- Gets the value associated with the 'comment-width' key in the config file.
    commentWidth :: IO String

    commentWidth = C.readValue "comment-width"


    -- Gets todays date in the format the user has specificied in the config file.
    date :: IO String

    date = do
        ct      <- getCurrentTime
        dformat <- C.readValue "date-format"

        let f (y, m, d) = (replace "dd" (show d) . replace "mm" (show m) . replace "yy" ((drop 2 . show) y) . replace "yyyy" (show y)) dformat
        
        (return . f . toGregorian . utctDay) ct
