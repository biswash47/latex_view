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


data Latex  = NoEnv 
          | Env {name :: String, content :: [Latex] } 
          | PlainText String 
          deriving (Show, Eq, Generic)

instance ToJSON Latex
instance FromJSON Latex

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


envParser = do 
    envName <- try envBegin
    content <- many $ envParser <|>  PlainText <$> plainTextParser
    envEnd envName
    return $ Env envName content



testFile =  "../latex_files/test.tex"
parseTester p = runParser p testFile <$> readFile testFile

parseFromFile = parseTester (envParser)