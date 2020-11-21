module Compiler.Detokenizer where

import Parsers.ParseLatex


-- convert Latex objects to strings
-- detok = detokenizer

{-detok :: Latex -> String
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
  where
    content = concatMap detok c
-}