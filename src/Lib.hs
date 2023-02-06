module Lib where

import Parser
import Evaluate

parseThenEval :: String -> Either String (String, Integer)
parseThenEval s  = do
    (ast, _) <- runParser program s
    evaluate ast []
