module Main where

import LexTree
import ParTree
import PrintTree
import AbsTree
import ErrM

import System.Environment ( getArgs, getProgName )

-- convert GF RGL trees to different formats or analyse them
-- how AR used it 28/8/2013
--   runghc Summarize.hs mandela.trees >summarized-mandela.trees
--   runghc Translate.hs -trees -s ParseEng.pgf <mandela.trees >mandela-eng.txt
--   runghc Translate.hs -trees -s ParseEng.pgf <summarized-mandela.trees >summarized-mandela-eng.txt
--   diff mandela-eng.txt summarized-mandela-eng.txt
-- 


main :: IO ()
main = do 
  args <- getArgs
  let (analyse,input) = case args of
        file:_ -> (analyseSummarize,file)
        _ -> error "usage: Summarize <tree-file>"
  s <- readFile input
  putStrLn $ analyse (map parseGTree (lines s))

analyseSummarize :: [GTree] -> String
analyseSummarize = unlines . map (printTree . summarize)

summarize :: GTree -> GTree 
summarize t = case t of
  GTApp h@(GFun (Ident f)) ts -> case f of
    "AdjCN" -> summarize (ts !! 1)  -- AdjCN ap cn -> cn
    "RelNP" -> summarize (ts !! 0)  
    "AdvNP" -> summarize (ts !! 0)  
    -- add your own summarization patterns here

    _ -> GTApp h (map summarize ts)

  GTApp h ts -> GTApp h (map summarize ts)
  _ -> t

-- utilities

parseGTree :: String -> GTree
parseGTree s = case pGTree (myLexer s) of
  Ok t -> t
  _ -> GTAtom GMeta 
