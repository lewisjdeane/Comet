{-
    Main file for 'comet', handles commands and provides documentation regarding
    usage.
    
    Author(s):     Lewis Deane
    License:       MIT
    Last Modified: 14/11/2015
-}

{-
    TODO: Get it so we use the current settings when appending and updating as opposed to simply using defaults everytime.
-}

-- Imports
import Control.Applicative
import Data.List (delete, intercalate, isPrefixOf)
import Data.List.Split (splitOn)
import Paths_comet
import System.Environment
import System.Directory
import System.IO
import Text.Regex.Posix

import qualified Config       as C
import qualified CommentTools as T

-- Type synonyms for increased readability.
type FileName = String
type Comment  = String


-- Where everything begins.
main :: IO ()

main = getArgs >>= parse


-- Allowable settings.
settings :: [String]

settings = ["author", "comment-width", "date-format", "license", "maintainer", "default-fields"]


-- Parses the input from the command line and handles what should be done.
parse :: [String] -> IO ()

parse [] = doc

parse ["v"]       = printVersion
parse ["version"] = printVersion

parse ["ls"]            = printSettings
parse ["list-settings"] = printSettings

parse ["author"]         = configG "author"
parse ["comment-width"]  = configG "comment-width"
parse ["date-format"]    = configG "date-format"
parse ["default-fields"] = configG "default-fields"
parse ["license"]        = configG "license"
parse ["maintainer"]     = configG "maintainer"

parse ["author", x]         = configS "author"        x
parse ["comment-width", x]  = configS "comment-width" x
parse ["date-format", x]    = if isLegalDateFormat x then configS "date-format" x else error $ x ++ " is not a valid date format."
parse ["default-fields", x] = if isFieldShortcut $ tail x then configS' "default-fields" x else error $ tail x ++ " is not a valid field shortcut."
parse ["license", x]        = configS "license"    x
parse ["maintainer", x]     = configS "maintainer" x     

parse ["g", x]   = T.currentComment x
parse ["get", x] = T.currentComment x

parse ["d", x]      = T.deleteComment x
parse ["delete", x] = T.deleteComment x

parse ("u":x:xs)      = T.updateComment x xs
parse ("update":x:xs) = T.updateComment x xs

parse ("s":x:y:xs)   = T.setComment x y xs
parse ("set":x:y:xs) = T.setComment x y xs

parse ("a":x:y:xs)       = T.appendComment x y xs
parse ("append" :x:y:xs) = T.appendComment x y xs

parse _ = putStrLn usage


-- Checks if the passed symbol is a shortcut for a setting.
isFieldShortcut :: String -> Bool

isFieldShortcut x | x == "a"  = True
                  | x == "m"  = True
                  | x == "l"  = True
                  | x == "lm" = True
                  | otherwise = False


-- Checks if the user has supplied a legal date format pattern.
isLegalDateFormat :: String -> Bool

isLegalDateFormat s | s =~ "(dd(/|-)mm(/|-)(yy|yyyy))" = True
                    | s =~ "(mm(/|-)dd(/|-)(yy|yyyy))" = True
                    | s =~ "(dd(/|-)(yy|yyyy)(/|-)mm)" = True
                    | s =~ "(mm(/|-)(yy|yyyy)(/|-)dd)" = True
                    | s =~ "((yy|yyyy)(/|-)dd(/|-)mm)" = True
                    | s =~ "((yy|yyyy)(/|-)mm(/|-)dd)" = True
                    | otherwise                        = False


-- Provides a reusable string to be used after errors explaining what the user can do to get help.
usage :: String

usage = "Run 'comet' for a list of legal commands."


-- Launches the appropriate config action.
configS :: String -> String -> IO ()

configS k v = if k `elem` settings then C.writeValue (k, v) else error $ "No such setting '" ++ k ++ "' " ++ usage


-- This version of the above function handles the adding and removing of shortcuts within the value associated with the key.
configS' :: String -> String -> IO ()

configS' k v = do
    values <- splitOn "," <$> C.readValue k

    let v'   = tail v
        mode = head v
        new  = if v' `elem` values
                    then if mode == '-'
                            then delete v' values
                            else values
                    else if mode == '+'
                            then v' : values
                            else values
        new' = intercalate "," new

    configS k new'


-- Gets the current setting from confifg.
configG :: String -> IO ()

configG k = if k `elem` settings then prettyPrint <$> C.readValue k >>= putStrLn else error $ "No such setting '" ++ k ++ "' " ++ usage


-- Adds a new line before and after a string.
prettyPrint :: String -> String

prettyPrint s = "\n" ++ s ++ "\n"


-- What should be printed out when no args are passed to our inital command.
doc :: IO ()

doc = (putStrLn . unlines) $ [prettyPrint "---- USAGE ----"] ++ commands ++ [prettyPrint "You can also pass parameters when setting, appending or updating."] ++ parameters ++ [prettyPrint "\n---- SUPPORTED LANGUAGES ----"] ++ languages ++ [prettyPrint "\n---- EXAMPLES ----"] ++ examples ++ [""]


-- List of commands to be outputted when 'comet' is run.
commands = T.genBlock "" 4 c
          where c = [("COMMAND"                    , "DESCRIPTION"),
                     ("comet s|set    FILE COMMENT", "Write comment to file."),
                     ("comet a|append FILE COMMENT", "Append comment to file."),
                     ("comet d|delete FILE"        , "Delete comment from file."),
                     ("comet g|get    FILE"        , "Get comment block from file."),
                     ("comet u|update FILE"        , "Updates file with current settings."),
                     ("comet v|version"            , "Get current version."),
                     ("comet ls|list-settings"     , "List settings with values."),
                     ("comet author"               , "Get author."),
                     ("comet author NAME"          , "Set author to name."),
                     ("comet comment-width"        , "Get comment width."),
                     ("comet comment-width NUM"    , "Set comment width to num."),
                     ("comet date-format"          , "Get date format."),
                     ("comet date-format PATTERN"  , "Set date format to pattern."),
                     ("comet default-fields"       , "Get default-fields."),
                     ("comet default-fields +l"    , "Add license to default-fields."),
                     ("comet default-fields -m"    , "Remove maintainer from default-fields."),
                     ("comet license"              , "Get license."),
                     ("comet license NAME"         , "Set license to name."),
                     ("comet maintainer"           , "Get maintainer."),
                     ("comet maintainer PATTERN"   , "Set maintainer to name.")]
 

-- Explains what the parameters mean.
parameters :: [String]

parameters = T.genBlock "" 4 c
            where c = [("PARAMETER", "ACTION"),
                       ("-a"       , "Hide author field."),
                       ("-l"       , "Hide license field."),
                       ("-lm"      , "Hide last modified field."),
                       ("-m"       , "Hide maintainer field."),
                       ("+a"       , "Show author field."),
                       ("+l"       , "Show license field."),
                       ("+lm"      , "Show last modified field."),
                       ("+m"       , "Show maintainer field."),
                       ("+a  VALUE", "Show and overwrite author field."),
                       ("+l  VALUE", "Show and overwrite license field."),
                       ("+lm VALUE", "Show and overwrite last modified field."),
                       ("+m  VALUE", "Show and overwrite maintainer field.")]


-- Nicely formats allowed files and extensions.
languages :: [String]

languages = T.genBlock "" 4 l
            where l = [("LANGUAGE"   , "FILE EXTENSION"),
                      ("Arduino"     , ".pde .ino"),
                      ("C"           , ".c .h"),
                      ("C++"         , ".cpp"),
                      ("CoffeeScript", ".coffee"),
                      ("C#"          , ".cs"),
                      ("CSS"         , ".css"),
                      ("ERB"         , ".erb"),
                      ("Go"          , ".go"),
                      ("HAML"        , ".haml"),
                      ("Haskell"     , ".hs"),
                      ("HTML"        , ".html .htm .xhtml"),
                      ("Java"        , ".java"),
                      ("JavaScript"  , ".js"),
                      ("Lisp"        , ".lisp"),
                      ("MatLab"      , ".matlab"),
                      ("PHP"         , ".php"),
                      ("Python"      , ".py"),
                      ("R"           , ".r"),
                      ("Ruby"        , ".rb"),
                      ("Scala"       , ".scala"),
                      ("SASS"        , ".sass"),
                      ("SCSS"        , ".scss"),
                      ("XML"         , ".xml")]


-- List of example usages to be printed.
examples :: [String]

examples = ["comet g Main.hs            -> Get the comment block from Main.hs.",
            "comet s Main.hs 'Hello' -l -> Set 'hello' as comment and hide the license field.",
            "comet delete Main.hs       -> Removes the comment block from Main.hs.",
            "comet author 'Mark Twain'  -> Set current author to 'Mark Twain'."]


-- Returns the current version number.
printVersion :: IO ()

printVersion = putStrLn "v1.1.0"


-- Prints a formatted block containing settings and their current values.
printSettings :: IO ()

printSettings = do
    path    <- getDataFileName "config.txt"
    content <- lines <$> readFile path

    (putStrLn . unlines . T.genBlock ":" 1 . map (\x -> let a = splitOn ":" x in (head a, last a))) content
    