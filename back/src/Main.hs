{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where


import Web.Scotty
import Network.Wai.Middleware.Cors
import Parsers.ParseLatex 
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import qualified Data.Text as T

data Hole = Hole

main = do
    scotty 4000 $ do
        middleware simpleCors
        get "/hello" $ do
            res <- liftIO $ parseFromFile
            (getResult res)
            where
                getResult res =
                    case res of 
                        Left _ ->  json ("error" :: String)
                        Right e -> json e