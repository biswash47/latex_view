{-# LANGUAGE OverloadedStrings #-}

module Parsers.FileReader where

import System.IO ()
import System.Directory ( getCurrentDirectory )


xtestFile = readFile "../latex_files/test.tex"
currDir = getCurrentDirectory >>= putStrLn

printTestFile = putStrLn =<< xtestFile