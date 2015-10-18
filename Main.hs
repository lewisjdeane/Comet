{-
    Main file for 'comet', handles commands and provides documentation regarding
    usage.
    
    Author(s):     Lewis Deane
    License:       MIT
    Last Modified: 18/10/2015
-}

-- Imports
import Control.Applicative
import Data.List (isPrefixOf)
import System.Environment
import System.Directory
import System.IO

import qualified Config as C
import qualified CommentTools as T

-- Type synonyms for increased readability.
type FileName = String
type Comment  = String


-- Where everything begins.
main = getArgs >>= parse


-- Allowable settings.
settings :: [String]

settings = ["author", "comment-width", "license"]


-- Parses the input from the command line and handles what should be done.
parse :: [String] -> IO ()

parse [] = doc

parse ["v"]       = version
parse ["version"] = version

parse ["license"]       = configG "license"
parse ["author"]        = configG "author"
parse ["comment-width"] = configG "comment-width"

parse ["license", x]       = configS "license"       x
parse ["author", x]        = configS "author"        x
parse ["comment-width", x] = configS "comment-width" x

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


check :: [String] -> Int -> [String]

check params num = if length params == num then params else error $ "Expected " ++ show num ++ " parameters but found " ++ ((show . length) params) ++ ". " ++ usage


-- Provides a reusable string to be used after errors explaining what the user can do to get help.
usage :: String

usage = "Run 'comet' for a list of legal commands."


-- Launches the appropriate config action.
configS :: String -> String -> IO ()

configS k v = if k `elem` settings then C.writeValue (k, v) else error $ "No such setting '" ++ k ++ "' " ++ usage


-- Gets the current setting from config.
configG :: String -> IO ()

configG k = if k `elem` settings then prettyPrint <$> C.readValue k >>= putStrLn else error $ "No such setting '" ++ k ++ "' " ++ usage


-- Adds a new line before and after a string.
prettyPrint :: String -> String

prettyPrint s = "\n" ++ s ++ "\n"


-- What should be printed out when no args are passed to our inital command.
doc :: IO ()

doc = (putStrLn . unlines) $ [prettyPrint "---- USAGE ----"] ++ commands ++ [prettyPrint "You can also pass parameters when setting, appending or updating."] ++ parameters ++ [prettyPrint "\n---- SUPPORTED LANGUAGES ----"] ++ languages ++ [prettyPrint "\n---- EXAMPLES ----"] ++ examples ++ [""]


-- List of commands to be outputted when 'comet' is run.

commands = zipWith3 concat3 x (repeat "\t") y
          where x = map fst c
                y = map snd c 
                c = [("COMMAND                    ", "DESCRIPTION"),
                     ("comet s|set FILE COMMENT   ", "Write comment to file."),
                     ("comet a|append FILE COMMENT", "Append comment to file."),
                     ("comet d|delete FILE        ", "Delete comment from file."),
                     ("comet g|get FILE           ", "Get comment block from file."),
                     ("comet u|update FILE        ", "Updates file with current settings."),
                     ("comet v|version            ", "Get current version."),
                     ("comet author               ", "Get author."),
                     ("comet author NAME          ", "Set author to name."),
                     ("comet comment-width        ", "Get comment width."),
                     ("comet comment-width NUM    ", "Set comment width to num."),
                     ("comet license              ", "Get license."),
                     ("comet license NAME         ", "Set license to name.")]
 
-- Explains what the parameters mean.
parameters :: [String]

parameters = zipWith3 concat3 x (repeat "\t") y
            where x = map fst c
                  y = map snd c
                  c = [("PARAMETER", "ACTION"),
                       ("-a       ", "Hide author field."),
                       ("-l       ", "Hide license field."),
                       ("-lm      ", "Hide last modified field.")]


-- Nicely formats allowed files and extensions.
languages :: [String]

languages = zipWith3 concat3 x (repeat "\t") y
            where x = map fst l
                  y = map snd l
                  l = [("LANGUAGE   ", "FILE EXTENSION"),
                      ("C           ", ".c .h"),
                      ("C++         ", ".cpp"),
                      ("CoffeeScript", ".coffee"),
                      ("CSS         ", ".css"),
                      ("ERB         ", ".erb"),
                      ("Go          ", ".go"),
                      ("HAML        ", ".haml"),
                      ("Haskell     ", ".hs"),
                      ("HTML        ", ".html .htm .xhtml"),
                      ("Java        ", ".java"),
                      ("JavaScript  ", ".js"),
                      ("MatLab      ", ".matlab"),
                      ("PHP         ", ".php"),
                      ("Python      ", ".py"),
                      ("R           ", ".r"),
                      ("Ruby        ", ".rb"),
                      ("Scala       ", ".scala"),
                      ("SASS        ", ".sass"),
                      ("SCSS        ", ".scss"),
                      ("XML         ", ".xml")]


examples :: [String]
examples = ["comet g Main.hs            -> Get the comment block from Main.hs.",
            "comet s Main.hs 'Hello' -l -> Set 'hello' as comment and hide the license field.",
            "comet delete Main.hs       -> Removes the comment block from Main.hs.",
            "comet author 'Mark Twain'  -> Set current author to 'Mark Twain'."]


-- Joins three strings together.
concat3 :: String -> String -> String -> String

concat3 x y z = x ++ y ++ z


-- Returns the current version number.
version :: IO ()

version = putStrLn "v1.0"
