{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Parsers.ParseLatex where 

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void
import Data.Char
import Parsers.FileReader
import Control.Monad 
import System.IO ()
import GHC.Generics
import Data.Aeson

{- 
Env
    - text 
    - env
    - text 

-}


type Parser = Parsec Void String


data Hole = Hole

type Name = String 

data Latex  = NoEnv 
          | Env {name :: Name, content :: [Latex] } 
          | PlainText String 
          deriving (Eq, Generic)

instance ToJSON Latex
instance FromJSON Latex


instance Show Latex where 
    show  = detok


detok :: Latex -> String
detok l = case l of 
    NoEnv -> ""
    PlainText t -> show t
    Env n c -> envToStr n c


envBeginStr :: Name -> String 
envBeginStr n = "\\begin{" <> n <> "}\n"

envEndStr :: Name -> String
envEndStr n = "\\end{" <> n <> "}\n"


envToStr :: Name -> [Latex] -> String
envToStr n c = envBeginStr n <> content <> envEndStr n
    where  content = concatMap detok c <> "\n"


specialChars :: [Char]
specialChars = ['\\']

validChar :: Parser Char
validChar = anyCharBut specialChars

anyCharBut :: [Char]-> Parser Char 
anyCharBut l = satisfy (`notElem` l)

anyChar :: Parser Char
anyChar = satisfy $ const True

anyString :: Parser String
anyString = many anyChar

envBegin :: Parser String
envBegin = do
    envBeginTag
    name <- some alphaNumChar
    char '}'
    return name

envBeginTag :: Parser String
envBeginTag = string "\\begin{"

envEndTag :: Parser String
envEndTag = string "\\end{"

envEnd :: String -> Parser ()
envEnd envName = do
    envEndTag
    string envName
    char '}'
    return ()


plainTextParser :: Parser String
plainTextParser = do
    notFollowedBy envBeginTag
    notFollowedBy envEndTag
    content <- many validChar
    rest <- plainTextParser <|> pure ""
    return (content ++ rest)

envParser :: Parser Latex
envParser = do 
    envName <- try envBegin
    content <- many $ envParser <|>  PlainText <$> plainTextParser
    envEnd envName
    return $ Env envName content



testFile =  "../latex_files/test.tex"
parseTester p = runParser p testFile <$> readFile testFile

parseFromFile = parseTester (envParser)

showFile = do
    fileContents <- parseFromFile
    case fileContents of
        Left _ -> fail "Error"
        Right c -> print c