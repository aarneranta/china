module Main where

import AnalyseTree

import LexTree
import ParTree
import PrintTree
import AbsTree

import ErrM

import PGF

import Data.Char
import Data.List
import System.Environment (getArgs)
import System.Cmd (system)

-- AR 3/4/2013
-- translate via RGL + chunking
-- you need a symlink to GF/src/runtime/c/ParseEngAbs3.probs
-- the output is in tokenized in a file, line by line
-- example: 
--  ./Translate ParseEng.pgf ParseEngFin.pgf ParseEng <google.txt
-- echo "colorless green ideas sleep furiously" | ./Translate -parse ParseEng.pgf ParseEng


main :: IO ()
main = do 
  args0 <- getArgs
  let silent = elem "-s" args0
  let args = filter (/="-s") args0
  case args of
    "-parse":pgf1:source:_ -> do
       s <- getContents
       writeFile tmpInput s
       runC pgf1 source tmpInput                -- get trees from input file by parsing PGF
       ts <- fmap (map parseGTree . getParses . lines) $ readFile tmpOutput  -- read resulting trees
       putStrLn $ unlines $ map printTree ts
    "-trees":pgf2:_ -> do
       pgf <- readPGF pgf2                      -- read translation PGF
       s <- getContents
       let ts = map parseGTree (lines s)    -- read source trees
       let es = map (translateChunked silent False Nothing pgf (languages pgf)) (zip (zip (repeat "") ts) (map (map gtree2expr . chunkResolve) ts))
       putStrLn $ unlines $ es
    pgf1:pgf2:source:_ -> do
       s <- getContents
       writeFile tmpInput s      
       runC pgf1 source tmpInput                       -- get trees from input file by parsing PGF
       pgf <- readPGF pgf2                             -- read translation PGF
       ss <- fmap lines $ readFile tmpLex              -- read source sentences
       ts <- fmap (map parseGTree . getParses . lines) $ readFile tmpOutput
       let es = map (translateChunked silent True (Just source) pgf (languages pgf)) (zip (zip ss ts) (map (map gtree2expr . chunkResolve) ts))
       putStrLn $ unlines $ es
    _ -> putStrLn "usage: translate (-s) (<parsing-pgf> <translation-pgf> <source-lang>  |  -trees <translation-pgf>  |  -parse <parsing-pgf> <source-lang>)"

runC :: FilePath -> String -> FilePath -> IO ()
runC pgf source input = do
  s <- readFile input
  writeFile tmpLex $ unlines $ map lexText $ sentences s 
  let cmd = unwords ["pgf-translate",pgf,"Phrase",source,source,"ParseEngAbs3.probs", "<", tmpLex, ">", tmpOutput]
  system cmd
  putStrLn cmd
  return ()

pLanguage = mkCId ---maybe "XXX" id . readLanguage

tmpInput  = "translate.in.tmp"
tmpOutput = "translate.out.tmp"
tmpLex    = "translate.lex.tmp"

-- pgf-translate tree output format:  [129.880936] ? (PositAdvAdj actual_A) (UsePron it_Pron) 
getParses = map (unwords . tail) . filter ((=='[') . head . head) . filter (not . null) . map words

gtree2expr :: GTree -> Expr
gtree2expr = maybe (mkInt 666) id . readExpr . printTree ----

translateChunked :: Bool -> Bool -> Maybe String -> PGF -> [Language] -> ((String,GTree),[Expr]) -> String
translateChunked silent fromSrc mlang pgf langs ((str,gtr),exps) = 
  let trans = 
        if silent then [unlex (langCode lang) "." (concatMap words (map (linearize pgf lang) exps)) | lang <- langs, Just (showLanguage lang) /= mlang]
                  else [unwords ((showLanguage lang ++ ">") : [unlex (langCode lang) "." (concatMap words (intersperse "//" (map (linearize pgf lang) exps)))]) | lang <- langs]
  in unlines $ 
    if silent then trans else 
      ((if fromSrc then  ["SOURCE> " ++ str] else []) ++ 
       ["TREE> " ++ printTree gtr,
        "METAS> " ++ unwords (intersperse "//" (lines (analyseMetaFreq [gtr])))]) ++ 
       trans

langCode :: Language -> String
langCode = reverse . take 3 . reverse . showLanguage

unlex :: String -> String -> [String] -> String
unlex lang punct = addPunct punct . capHead . unwords . map unscore . unl  . preproc lang where
  unl ws = case ws of
    [d]:ww | isDigit d -> let (dd,rest) = span (all isDigit) ws in concat dd : unl rest
    w1:"&+":w2:ww -> unl ((w1 ++ w2) : ww)
    w1:p:ww | isPunct p -> unl ((w1 ++ p):ww)
    w:ww -> w : unl ww
    _ -> ws
  capHead (c:cs) = toUpper c : cs
  capHead s = s
  isPunct p = elem p [".",",","?","!"]
  addPunct p s = s ++ p
  unscore = map (\c -> if c == '_' then ' ' else c) --- '_' is used to glue separate words in grammars
  preproc lang ws = case lang of
    "Fin" -> case ws of
      w : "&+" : ":" : "&+" : e : ww -> preproc lang ((addEndingFin w e) : ww)
      w:ww -> w : preproc lang ww
      _ -> ws    
    _ -> ws

addEndingFin w e = case (lw,e) of
  (_,'i':'i':es) | elem lw "aeiouy" -> w ++ [lw,lw] ++ es  -- Monaco ++ iin -> Monacoon
  (_,'i':    es) | elem lw "aeiouy" -> w ++            es  -- Monaco ++ in  -> Monacon
  _                                 -> w ++ e              -- Lyon ++ iin   -> Lyoniin
 where
   lw = last w

lexText :: String -> String
lexText = uncap . unwords . lext where
  lext s = case s of
    [c]  | isMajorPunct c -> []                     --- ignore last punct mark
    c:cs | isMajorPunct c -> [c] : lext (uncap cs)
    c:cs | isMinorPunct c || isJunk c -> [c] : lext cs
    c:cs | isSpace c ->       lext cs 
    c:cs | isDigit c -> [c] : lext cs               --- split 23 to 2 3
--    p:' ':cs | isJunk p    -> lext cs
--    ' ':p:cs | isJunk p    -> lext cs
    _:_ -> let (w,cs) = break (\x -> isSpace x || isPunct x) s in w : lext cs
    _ -> [s]
  uncap s = case s of
    '"':cs -> uncap cs
    '\'':cs -> uncap cs
    'I':' ':_ -> s          --- don't uncap English pronoun "I"
    c:d:cs | isUpper d -> c : d : cs   -- don't uncap SDP
    c:cs -> toLower c : cs
    _ -> s

isPunct = flip elem ".?!,:;"
isMajorPunct = flip elem ".?!"
isMinorPunct = flip elem ",:;"
isJunk = flip elem "()-'\""

sentences :: String -> [String]
sentences = lines . map newl where
  newl c = case c of
    _ | isMajorPunct c -> '\n' -- convert major punctuation to newline
    '\n' -> ' '                -- convert input newline to space 
    _ -> c
