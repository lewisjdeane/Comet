{-
    Handles everything to do with the language being acted on.
    
    Author(s):     Lewis Deane
    License:       MIT
    Last Modified: 18/10/2015
-}

module LangTools (Lang, getLang, getBlockStart, getCommentChar, getBlockEnd) where

    import Data.Char
    import Data.List.Split

    -- Data type composed of the supported languages.
    data Lang = Arduino | C | CPP | CoffeeScript | CSharp | CSS | ERB | Go | HAML | Haskell | HTML | Java | JavaScript | Lisp | MatLab | PHP | Python | R | Ruby | Scala | SASS | SCSS | XML deriving (Eq, Show)

    -- Useful type synonyms.
    type FileName  = String
    type ComChar   = String
    type Extension = String

    -- Gets the string relating to the start of a block comment for the supported languages.
    getBlockStart :: Lang -> ComChar

    getBlockStart l | l `elem` [Arduino, C, CPP, CSharp, CSS, Go, Java, JavaScript, PHP, Scala, SCSS, SASS] = "/*"
                    | l `elem` [ERB, HTML, XML]                                                             = "<!--"
                    | l `elem` [Haskell]                                                                    = "{-"
                    | l `elem` [MatLab]                                                                     = "%{"
                    | otherwise                                                                             = getCommentChar l


    -- Gets the string relating to the end of a block comment for the supported languages.
    getBlockEnd :: Lang -> ComChar

    getBlockEnd l | l `elem` [Arduino, C, CPP, CSharp, CSS, Go, Java, JavaScript, PHP, Scala, SCSS, SASS] = " */"
                  | l `elem` [ERB, HTML, XML]                                                             = "-->"
                  | l `elem` [Haskell]                                                                    = "-}"
                  | l `elem` [MatLab]                                                                     = "%}"
                  | otherwise                                                                             = getCommentChar l


    -- Gets the string relating to the start of each comment line within a block comment.
    getCommentChar :: Lang -> ComChar

    getCommentChar l | l `elem` [Arduino, C, CPP, CSharp, CSS, Go, Java, JavaScript, PHP, Scala, SCSS, SASS] = " * "
                     | l `elem` [CoffeeScript, Python, R, Ruby]                                              = "# "
                     | l `elem` [HAML]                                                                       = "-# "
                     | l `elem` [Lisp]                                                                       = ";;; "
                     | l `elem` [ERB, Haskell, HTML, MatLab, XML]                                            = "    "


    -- Gets the language from the file extension.
    getLang :: FileName -> Lang

    getLang f | suf == ".ino"    = Arduino
              | suf == ".pde"    = Arduino
              | suf == ".c"      = C
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
              | suf == ".lisp"   = Lisp
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
    suffix :: FileName -> Extension

    suffix = (:) '.' . map toLower . last . splitOn "."
