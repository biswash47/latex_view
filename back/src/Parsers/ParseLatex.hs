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

type Content = String

data EnvContent = E Env | C Content
    deriving (Show, Eq, Generic)

instance ToJSON EnvContent
instance FromJSON EnvContent


data Env  = Start | Env {
    name :: String, 
    content :: [EnvContent]
    } deriving (Show, Eq, Generic)

instance ToJSON Env
instance FromJSON Env


validChar :: Parser Char
validChar = alphaNumChar 
    <|> spaceChar 
    <|> newline
    <|> satisfy ('.' ==)

anyChar :: Parser Char
anyChar = satisfy $ const True

anyString :: Parser String
anyString = many anyChar

envBegin :: Parser String
envBegin = do
    string "\\begin{"
    name <- some alphaNumChar
    char '}'
    return name

envEnd :: String -> Parser ()
envEnd envName = do
    string "\\end{"
    string envName
    char '}'
    return ()


--testParser :: Parser (String, String)
testParser = do
    string "\\begin{"
    name <- some alphaNumChar
    char '}'
    content <- some validChar
    string "\\end{"
    --name2 <- some (alphaNumChar <|> spaceChar)
    string name
    char '}'
    return (Env name [C content])

envParser = do 
    envName <- envBegin
    content <-  some validChar
    envEnd envName
    return (Env envName [C content])



envParser1 :: Env -> Parser Env
envParser1 env = do
    envName <- envBegin
    content <- manyTill anyChar (try $ envEnd envName)
    
    case env of
        Start -> return $ Env envName [C content]
        (Env n c) -> do 
            return $ envParser1 $ Env n ( E newEnv : c)
                where  newEnv = Env envName [C content]
            
                



testFile =  "../latex_files/test.tex"
parseTester p = runParser p testFile <$> readFile testFile

parseFromFile = parseTester (envParser1 Start)