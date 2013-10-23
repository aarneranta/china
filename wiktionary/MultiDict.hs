import Data.List
import Data.Char
import Control.Monad
import qualified Data.Map as M


--------------
-- datatypes
---------------

type Fun    = String
type Cat    = String
type Lang   = String
type Module = String
type Weight = Double

type Metadata = [(String,String)]   -- argument-value pairs

data Rule = RR {
  lin       :: String,
  lemma     :: String,
  metar     :: Metadata   -- source, status, date, author...
  } deriving (Eq,Show)

data Entry = EE {        -- example: fun watch_supervise_V2 
  cat       :: Cat,                  -- V2
  supercat  :: Cat,                  -- V
  synonyms  :: [Fun],                -- supervise_watch_V2
  hypernyms :: [Fun],                -- observe_V2
  weight    :: Weight,               -- probability or other weight
  word      :: String,               -- watch
  subwords  :: [String],             -- [supervise]
  meta      :: Metadata,             -- from wiktionary
  rules     :: M.Map Lang [Rule]     -- linearization variants
  }   


data Dictionary = DD {
  entries    :: M.Map Fun Entry,        -- the abstract and concrete syntaxes
  opens      :: M.Map Lang [Module],    -- the resource modules needed
  baseabs    :: [Module],               -- the basic abstracts extended, e.g. Cat
  basecncs   :: M.Map Lang [Module],    -- the basic concretes extended, e.g. Cat*
  addcats    :: [Cat],                  -- additional categories
  addlincats :: M.Map Lang (M.Map Cat String) -- lincats of additional categories
  }

-------------------
-- constructors
-------------------

initDictionary :: Dictionary
initDictionary = DD {
  entries    = M.empty,
  opens      = M.empty,
  baseabs    = initBaseAbs,
  basecncs   = M.empty,
  addcats    = [],
  addlincats = M.empty
  }

initEntry :: Fun -> Entry
initEntry f = EE {
  cat       = c,
  supercat  = getSupercat c,
  synonyms  = [],
  hypernyms = [],
  weight    = 1.0,
  word      = w,
  subwords  = ss,
  meta      = [],
  rules     = M.empty
  }
  where 
    (w,ss,c) = analyseFun f

initLang :: Lang -> Dictionary -> Dictionary
initLang lang dd = dd {
  opens    = M.insert lang (initOpens lang) $ opens dd,
  basecncs = M.insert lang (initBaseCnc lang) $ opens dd
  }

initBaseAbs :: [Module]
initBaseAbs = ["Cat"]

initBaseCnc :: Lang -> [Module]
initBaseCnc lang = [langModule "Cat" lang]

initOpens :: Lang -> [Module]
initOpens lang = [langModule "Paradigms" lang]

initRule :: Cat -> String -> Rule
initRule cat s = RR {
  lemma = s,
  lin   = mkLin cat s,
  metar = [("status","guess")]
  }


changeRules :: Lang -> [(Fun,[Rule])] -> Dictionary -> Dictionary
changeRules = updateRulesBy M.insert

updateRules :: Lang -> [(Fun,[Rule])] -> Dictionary -> Dictionary
updateRules = updateRulesBy (M.insertWith union)

updateRulesBy :: (Lang -> [Rule] -> M.Map Lang [Rule] -> M.Map Lang [Rule]) -> Lang -> [(Fun,[Rule])] -> Dictionary -> Dictionary
updateRulesBy upd lang funruls dict0 = 
  let dict = case M.lookup lang (opens dict0) of    -- if the lang doesn't exist
        Just _ -> dict0
        _ -> initLang lang dict0                    -- add the lang
      entry fun = case M.lookup fun (entries dict) of   -- if a fun doesn't exist
        Just e -> e
        _ -> initEntry fun                          -- add the fun
      addOne (fun,ruls) d = d {
        entries = M.insert fun ((entry fun) {                   -- add the new rule
          rules = upd lang ruls (rules (entry fun))    -- possibly on top of other rules for the same fun and lang 
          }) 
         (entries d)
         }
   in foldr addOne dict funruls

updateDictionary :: [(Lang,[(Fun,[Rule])])] -> Dictionary -> Dictionary
updateDictionary ens dict = foldr (\ (l,frs) d -> updateRules l frs d) dict ens


-------------------
-- queries
-------------------


abstractDict :: Dictionary -> M.Map Fun (Cat,Weight)
abstractDict d = M.fromList [(f,(cat e, weight e)) | (f,e) <- M.assocs (entries d)]

concreteLang :: Lang -> Dictionary -> M.Map Fun [Rule]
concreteLang lang d = M.fromList [(f,rs) | (f,e) <- M.assocs (entries d), Just rs <- [M.lookup lang (rules e)]]

allFuns :: Dictionary -> [Fun]
allFuns = M.keys . entries

allLanguages :: Dictionary -> [Lang]
allLanguages = M.keys . opens

lookIf :: Ord k => M.Map k [v] -> k -> [v]
lookIf m k = maybe [] id $ M.lookup k m

intersectDict :: Dictionary -> Dictionary
intersectDict d = intersectDictLang (allLanguages d) d

intersectDictLang :: [Lang] -> Dictionary -> Dictionary
intersectDictLang langs dict = dict {
  entries = M.fromList [(f,e) | (f,e) <- M.assocs (entries dict), isCompleteEntryLang langs e]
  }

isCompleteEntryLang :: [Lang] -> Entry -> Bool
isCompleteEntryLang langs e = all (\l -> M.member l (rules e)) langs 

lookupRules :: Fun -> Lang -> Dictionary -> [Rule]
lookupRules f l d = maybe [] (maybe [] id . M.lookup l . rules) $ M.lookup f (entries d)


-------------------
-- printing Dict
-------------------

prDictDict = prDictFiles "Dict"

-- print tab-separated (.tsv) table
-- structure: Abstract, Category, Bul lemma, Bul rule, Eng lemma, Eng rule, ...
prDictTab :: Dictionary -> [String]
prDictTab d = map prTab $ ("Abstract":"Category":langHeader) : [f : cat e : prPad (rules e) | (f,e) <- M.assocs (entries d)] where
  prTab = concat . intersperse "\t"
  prPad rmap = [pr (M.lookup lang rmap) | lang <- langs]
  langs = allLanguages d
  langHeader = [prTab [lang ++ " lemma", lang ++ " rule"] | lang <- langs]
  pr mrule = case mrule of
    Just rs -> prTab [concat (intersperse ", " (map lemma rs)),unwords (intersperse "|" (map lin rs))]
    _ -> "-"

prDictFiles :: Module -> Dictionary -> IO ()
prDictFiles name d = do
  writeFile (gfFile name) (unlines (prAbstract name d))
  mapM_ (\lang -> writeFile (gfFile (langModule name lang)) (unlines (prConcrete name lang d))) (allLanguages d)

prAbstract :: Module -> Dictionary -> [String]
prAbstract name d = 
  [unwords (["abstract",name,"="] ++ intersperse ", " (baseabs d) ++ ["**", "{"])] ++
  [prAbsRule f c w | (f,(c,w)) <- M.assocs (abstractDict d)] ++
  ["}"]

prConcrete :: Module -> Lang -> Dictionary -> [String] 
prConcrete name lang d = 
  [unwords (["concrete",langModule name lang,"of", name, "="] ++ intersperse ", " (lookIf (basecncs d) lang))] ++
  [unwords (["**","open"] ++ intersperse ", " (lookIf (opens d) lang) ++ ["{"])] ++
  [prCncRule f rs | (f,rs) <- M.assocs (concreteLang lang d)] ++
  ["}"]

prAbsRule :: Fun -> Cat -> Weight -> String
prAbsRule f c w = unwords ["fun",f,":",c,";","--",show w]

prCncRule :: Fun -> [Rule] -> String
prCncRule f rs = 
  unwords $ ["lin",f,"="] ++ (intersperse "|" (map lin rs)) ++ [";","--"] ++ map (prMetadata . metar) rs 
  ---- metadata should be by variants

prMetadata :: Metadata -> String
prMetadata ms = concat $ intersperse ", " [a ++ "=" ++ v | (a,v) <- ms]


-----------------------------------
-- getting Dict by parsing GF files
-----------------------------------

---- TODO: use the real GF source file parser
 
-- from one-line GF rules: (lin) work_V = mkV "arbeta" ;
gfLine2lin :: String -> [(Fun,[Rule])]
gfLine2lin = get . words where
  get ws = case ws of
    "lin":ww   -> get ww                                           -- drop leading lin
    _  :"=":"variants":('{':'}':_):_ -> []
    fun:"=":ww -> [(fun, map lin2rule (variants (unwords ww)))]    ---- metadata actually per rule, not per variant
    _ -> [] ---error ("cannot get lin rule from " ++ unwords ws)

-- this reads a GF lexicon file, accepting lines with '=' and ';'
---- TODO: should get opens and extends from the header, via parser
gfFile2dictionaryUpdate :: Dictionary -> Lang -> String -> Dictionary
gfFile2dictionaryUpdate dict lang s = 
  updateDictionary [(lang, concatMap gfLine2lin (filter isLinRule (lines s)))] dict

isLinRule :: String -> Bool
isLinRule s = elem '=' s && elem ';' s

-------------------
-- utilities
-------------------

-- assume form word_expl_Cat which is a valid GF identifier
-- takes watch_supervise_V2 to (watch,[supervise],V2)
analyseFun :: String -> (String,[String],String)
analyseFun s = case words (map (\c -> if c == '_' then ' ' else c) s) of
  ws@(_:_:_) -> (head ws, tail (init ws), last ws)
  _          -> (s,[],"Word")  -- default category


-- takes V2 and V2V to V
getSupercat :: Cat -> Cat
getSupercat = take 1


-- Dict ++ Eng = DictEng
langModule :: Module -> Lang -> Module
langModule m l = m ++ l

-- make sure a fun or cat is a valid GF ident --- TODO: this may produce clashes 
mkIdent :: String -> String
mkIdent = initial . map fc where
  fc c = if isAlphaNum c then c else '_'
  initial w@(c:_) = if isLetter c then w else "word__" ++ w
  initial w = "word__" ++ w

-- modulename.gf
gfFile :: Module -> FilePath
gfFile m = m ++ ".gf"

-- mkC "word"
mkLin :: Cat -> String -> String
mkLin cat word = "mk" ++ cat ++ " \"" ++ word ++ "\""

-- heuristic: mkV "x" "y" "z"  gives lemma x ; comment is metadata
lin2rule :: String -> Rule
lin2rule s = RR {
  lin = r,
  lemma = case filter ((=='"') . head) (words r) of 
    w@(_:_:_):_ -> init (tail w)   --- the first quoted string is the lemma
    _ -> r,                --- if not found, the whole entry is the lemma
  metar = case dropWhile (flip elem " ;-") c of
            "" -> []
            co -> [("comment",co)]
  }
 where
  (r,c) = span (/= ';') s   ---- divide at the first ;
  
-- module.gf -> module
getModule :: FilePath -> Module
getModule = reverse . drop 3 . reverse

-- DictEng -> Eng
getLang :: Module -> Lang
getLang = reverse . take 3 . reverse

variants :: String -> [String]
variants = lines . map (\c -> if c=='|' then '\n' else c)

chop :: Eq a => a -> [a] -> [[a]]
chop c xs = case break (== c) xs of
  (x1,_:x2@(_:_)) -> x1 : chop c x2
  (x1,_)          -> [x1]

normalizeSpaces :: String -> String
normalizeSpaces = unwords . words

----------------------------
---- to test
----------------------------

test = getDictFromGFFiles [
  dictDir ++ "/src/english/DictEng.gf",
  dictDir ++ "/src/bulgarian/DictEngBul.gf",
  dictDir ++ "/src/chinese/DictEngChi.gf",
  dictDir ++ "/src/finnish/stemmed/DictEngFin.gf",
---  dictDir ++ "/src/french/DictEngFre.gf", --- not in utf8
  dictDir ++ "/src/german/DictEngGer.gf"
  ]
  where
    dictDir = "/Users/aarne/GF/lib"

getDictFromGFFiles :: [FilePath] -> IO Dictionary
getDictFromGFFiles = foldM updateDictFromGFFile initDictionary

updateDictFromGFFile :: Dictionary -> FilePath -> IO Dictionary
updateDictFromGFFile dict0 file = do
  s <- readFile file
  let lang = getLang $ getModule file
  return $ gfFile2dictionaryUpdate dict0 lang s


-- example from Wiktionary:
-- *Main> w <- readFile "en-cmn-enwiktionary.txt" >>= return . lines
-- *Main> let d = dictW "Chi" w initDictionary
-- *Main> putStrLn $ unlines $ take 100 $ prConcrete "Wikt" "Chi" d


-------------------------------------
-- getting Dict by parsing Wiktionary
-------------------------------------

-- format:
-- absorb {v}    (to occupy fully ) :: 全神貫注, 全神贯注 /quánshénguànzhù/
-- fun    cat    disamb                lin variants     pronunciation

dictW :: Lang -> [String] -> Dictionary -> Dictionary
dictW lang ls dict = 
  let ruls = linRulesW lang $ concatMap analyseLineW ls 
  in  
  updateDictionary [(lang,[(fun,rs) | (fun,_,rs,_) <- ruls])] dict

analyseLineW :: String -> [(Fun, Cat, String, [String], Metadata)]
analyseLineW l = do
  let ws = filter (notElem '/') (words l)                    -- ignore pronunciation
  if (notElem "::" ws || notElem '{' l) then fail l else do  -- ignore incomplete or ill-formatted line
    let (ws1,c:ws2) = span ((/= '{') . head) ws              -- English lemma and cat
    let fun = mkIdent (unwords ws1)
    let cat = catW c
    let (disamb,_:ws3) = span (/="::") ws2                   -- disambiguation part and linearizations
    let lins = map normalizeSpaces (chop ',' (unwords ws3))  -- comma-separated lins
    return (fun,cat,unwords disamb,lins,srcMetadata "Wiktionary")

-- build the final fun names and lin rules
linRulesW :: Lang -> [(Fun, Cat, String, [String], Metadata)] -> [(Fun,Cat,[Rule],Metadata)]
linRulesW lang = map buildRule . concatMap disambFuns . groupBy sameFunCat . concatMap getCats
  where
   getCats r = [r]  ---- TODO: look up subcategories of verbs and expand verb entries accordingly
   sameFunCat (f1,c1,_,_,_) (f2,c2,_,_,_) = f1==f2 && c1==c2
   disambFuns rs = case rs of
     [(f,c,d,ls,m)] -> [(f ++"_" ++ c, c, d, ls, m)]
     _   -> [(disambiguateFun f c d i, c, d, ls, m) | ((f,c,d,ls,m),i) <- zip rs [1..]]
   buildRule (f,c,d,ls,m) = (f,c, map (initRule c) ls, m ++ glossMetadata d)

disambiguateFun :: Fun -> Cat -> String -> Int -> Fun
disambiguateFun f c _ i = concat $ intersperse "_" [f,show i, c]

srcMetadata :: String -> Metadata
srcMetadata s = [("src",s)]

glossMetadata :: String -> Metadata
glossMetadata s = [("gloss",s)]

catW :: String -> Cat
catW s = case (init (tail s)) of
  "adj"  -> "A"
  "n"    -> "N"
  "v"    -> "V"
  "prop" -> "PN"
  "adv"  -> "Adv"
  "conj" -> "Conj"
  "interj" -> "Interj"
  "proverb" -> "Utt"
  "determiner" -> "Det"
  _ -> "Word"  -- the catch-all category
