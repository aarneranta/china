module Main where

import AnalyseTree

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexTree
import ParTree
import PrintTree
import AbsTree

import ErrM



-- convert GF RGL trees to different formats or analyse them

main :: IO ()
main = do 
  args <- getArgs
  let (analyse,input) = case args of
        "-metas":file:_     -> (analyseMetaFreq,file)
        "-funs":file: _     -> (analyseFunFreq,file)
        "-chunks":file:_    -> (analyseChunk,file)
        "-resolve":file:_   -> (analyseResolve,file)
        "-summarize":file:_ -> (analyseSummarize,file)
        _ -> error "usage: Convert (-metas | -chunks | -resolve | -funs) <tree-file>"
  s <- readFile input
  putStrLn $ analyse (map parseGTree (lines s))

