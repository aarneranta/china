import Data.List
import Data.Char
import Control.Monad
import qualified Data.Map as M


----------------------------
---- to test
----------------------------

myGFDictDir = "/Users/aarne/GF/lib/src/"
myWiktDir = "/Users/aarne/wiktionary/ding/"

-- Dictionary from GF DictEng*

testGFDict :: IO ()
testGFDict = do
  d <- getDictGFDict
  let ds = unlines $ take 200 $ sample 500 $ prDictTab optRule d
  writeFile "dict.tsv" ds

getDictGFDict :: IO Dictionary
getDictGFDict = getDictFromGFFiles [
  myGFDictDir ++ "english/DictEng.gf",
  myGFDictDir ++ "bulgarian/DictEngBul.gf",
  myGFDictDir ++ "chinese/DictEngChi.gf",
  myGFDictDir ++ "finnish/DictEngFin.gf",
  myGFDictDir ++ "french/DictEngFre.gf",
  myGFDictDir ++ "german/DictEngGer.gf",
  myGFDictDir ++ "hindi/DictEngHin.gf",
  myGFDictDir ++ "swedish/DictEngSwe.gf",
  myGFDictDir ++ "urdu/DictEngUrd.gf"
  ]


-- Dictionary from Wiktionary

testWiktionary :: IO ()
testWiktionary = do
  d <- getDictWiktionary
  let isd = intersectDictLang (allLanguages d) d
  putStrLn "Size of totally implemented common part:"
  print $ length $ prAbstract "Wikt" $ isd
  let ids = unlines $ prDictTab optLemma isd
  writeFile "wikt-intersection.tsv" ids
  let ds = unlines $ take 200 $ sample 500 $ prDictTab optLemma d
  writeFile "wikt-sample.tsv" ds

getDictWiktionary :: IO Dictionary
getDictWiktionary = getDictFromWiktionaries [
  ("Eng",             "en-en-enwiktionary.txt"),
  ("Bul",  myWiktDir ++ "en-bg-enwiktionary.txt"),
  ("Cat",  myWiktDir ++ "en-ca-enwiktionary.txt"),
  ("Chi",  myWiktDir ++ "en-cmn-enwiktionary.txt"),
  ("Fin",  myWiktDir ++ "en-fi-enwiktionary.txt"),
  ("Ger",  myWiktDir ++ "en-de-enwiktionary.txt"),
  ("Gre",  myWiktDir ++ "en-el-enwiktionary.txt"),
  ("Hin",  myWiktDir ++ "en-hi-enwiktionary.txt"),
  ("Ita",  myWiktDir ++ "en-it-enwiktionary.txt"),
  ("Jpn",  myWiktDir ++ "en-ja-enwiktionary.txt"),
  ("Dut",  myWiktDir ++ "en-nl-enwiktionary.txt"),
  ("Nor",  myWiktDir ++ "en-no-enwiktionary.txt"),
  ("Pol",  myWiktDir ++ "en-pl-enwiktionary.txt"),
  ("Ron",  myWiktDir ++ "en-ro-enwiktionary.txt"),
  ("Rus",  myWiktDir ++ "en-ru-enwiktionary.txt"),
  ("Spa",  myWiktDir ++ "en-es-enwiktionary.txt"),
  ("Swe",  myWiktDir ++ "en-sv-enwiktionary.txt")
  ]



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
  subcats   :: [Cat],                -- possible subcats, obtained from some other source
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

initEntry :: Fun -> Metadata -> Entry
initEntry f m = EE {
  cat       = c,
  supercat  = cat2supercat c,
  subcats   = [],
  synonyms  = [],
  hypernyms = [],
  weight    = 1.0,
  word      = w,
  subwords  = ss,
  meta      = m,
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


changeRules :: Lang -> [(Fun,[Rule],Metadata)] -> Dictionary -> Dictionary
changeRules = updateRulesBy M.insert

updateRules :: Lang -> [(Fun,[Rule],Metadata)] -> Dictionary -> Dictionary
updateRules = updateRulesBy (M.insertWith union)

updateRulesBy :: (Lang -> [Rule] -> M.Map Lang [Rule] -> M.Map Lang [Rule]) -> Lang -> [(Fun,[Rule],Metadata)] -> Dictionary -> Dictionary
updateRulesBy upd lang funruls dict0 = 
  let dict = case M.lookup lang (opens dict0) of    -- if the lang doesn't exist
        Just _ -> dict0
        _ -> initLang lang dict0                    -- add the lang
      entry fun meta = case M.lookup fun (entries dict) of   -- if a fun doesn't exist
        Just e -> e
        _ -> initEntry fun meta                              -- add the fun and its metadata
      addOne (fun,ruls,meta) d = d {
        entries = M.insert fun ((entry fun meta) {          -- add the new rules
          rules = upd lang ruls (rules (entry fun meta))    -- possibly on top of other rules for the same fun and lang 
          }) 
         (entries d)
         }
   in foldr addOne dict funruls

updateDictionary :: [(Lang,[(Fun,[Rule],Metadata)])] -> Dictionary -> Dictionary
updateDictionary ens dict = foldr (\ (l,frs) d -> updateRules l frs d) dict ens


-------------------
-- queries
-------------------


statisticsDict :: Dictionary -> Metadata
statisticsDict dict = [
  ("funs",   show $ length $ M.keys $ entries dict),
  ("shared", show $ length $ M.keys $ entries $ intersectDict dict),
  ("langs", show $ length $ allLanguages dict)
  ] ++ 
  [("lins" ++ lang, show $ length $ M.assocs $ concreteLang lang dict) 
      | lang <- allLanguages dict]

abstractDict :: Dictionary -> M.Map Fun (Cat, Weight, Metadata)
abstractDict d = M.fromList [(f,(cat e, weight e, meta e)) | (f,e) <- M.assocs (entries d)]

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
prDictTab :: TabOptions -> Dictionary -> [String]
prDictTab opts d = map prTab $ ("Abstract":"Category":langHeader) : [f : cat e : prPad (rules e) | (f,e) <- M.assocs (entries d)] where
  prTab = concat . intersperse "\t"
  prPad rmap = [pr (M.lookup lang rmap) | lang <- langs]
  langs = allLanguages d
  chooseOpt lr = case opts of  --- [lemma,rule]
    _ | opts == optAll   -> lr
    _ | opts == optRule  -> drop 1 lr
    _ | opts == optLemma -> take 1 lr
  langHeader = [prTab (chooseOpt [lang ++ " lemma", lang ++ " rule"]) | lang <- langs]
  pr mrule = prTab $ chooseOpt $ case mrule of
    Just rs -> [concat (intersperse ", " (map lemma rs)),unwords (intersperse "|" (map lin rs))]
    _ -> ["-","-"]

type TabOptions = Int --- just the option of showing lemmas vs. rules
optAll, optRule, optLemma :: TabOptions
optAll = 0
optRule = 1
optLemma = 2


prDictFiles :: Module -> Dictionary -> IO ()
prDictFiles name d = do
  writeFile (gfFile name) (unlines (prAbstract name d))
  mapM_ (\lang -> writeFile (gfFile (langModule name lang)) (unlines (prConcrete name lang d))) (allLanguages d)

prAbstract :: Module -> Dictionary -> [String]
prAbstract name d = 
  [unwords (["abstract",name,"="] ++ intersperse ", " (baseabs d) ++ ["**", "{"])] ++
  [prAbsRule f c w m | (f,(c,w,m)) <- M.assocs (abstractDict d)] ++
  ["}"]

prConcrete :: Module -> Lang -> Dictionary -> [String] 
prConcrete name lang d = 
  [unwords (["concrete",langModule name lang,"of", name, "="] ++ intersperse ", " (lookIf (basecncs d) lang))] ++
  [unwords (["**","open"] ++ intersperse ", " (lookIf (opens d) lang) ++ ["{"])] ++
  [prCncRule f rs | (f,rs) <- M.assocs (concreteLang lang d)] ++
  ["}"]

prAbsRule :: Fun -> Cat -> Weight -> Metadata -> String
prAbsRule f c w m = unwords ["fun",f,":",c,";","--", prWeight w, prMetadata m]

prCncRule :: Fun -> [Rule] -> String
prCncRule f rs = 
  unwords $ ["lin",f,"="] ++ (intersperse "|" (map lin rs)) ++ [";","--"] ++ map (prMetadata . metar) rs 
  ---- metadata should be by variants

prMetadata :: Metadata -> String
prMetadata ms = concat $ intersperse ", " [a ++ "=" ++ v | (a,v) <- ms]

prWeight :: Weight -> String
prWeight w = "weight=" ++ show w ++ ","  -- thus looks like metadata, but is internally Double and not String





-------------------
-- utilities
-------------------

-- assume form word_expl_Cat which is a valid GF identifier
-- takes watch_supervise_V2 to (watch,[supervise],V2)
analyseFun :: String -> (String,[String],String)
analyseFun s = case words (map (\c -> if c == '_' then ' ' else c) s) of
  ws@(_:_:_) -> (head ws, tail (init ws), last ws)
  _          -> (s,[],"Word")  -- default category

fun2cat :: Fun -> Cat
fun2cat f = case analyseFun f of (_,_,c) -> c

fun2firstpart :: Fun -> Fun
fun2firstpart f = case analyseFun f of (f,_,_) -> f

fun2lexPart :: Fun -> Cat
fun2lexPart f = case analyseFun f of (f,gs,_) -> glueIdents $ f:gs

-- takes V2 and V2V to V
cat2supercat :: Cat -> Cat
cat2supercat = take 1

-- concat idents with "_" in between
glueIdents :: [String] -> String
glueIdents = concat . intersperse "_"

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

chop :: Eq a => [a] -> [a] -> [[a]]
chop cs xs = case break (flip elem cs) xs of
  (x1,_:x2@(_:_)) -> x1 : chop cs x2
  (x1,_)          -> [x1]

ignoreSegments :: Eq a => a -> a -> [a] -> [a]
ignoreSegments beg end s = case break (==beg) s of
  (x1,_:x2@(_:_)) -> x1 ++ case break (==end) x2 of 
     (x3,x4) -> ignoreSegments beg end $ drop 1 x4
  (x1,_)          -> x1

normalizeSpaces :: String -> String
normalizeSpaces = unwords . words

sample :: Int -> [a] -> [a]
sample k xs = [xs !! i | i <- [0,k .. length xs - 1]]

-----------------------------------
-- getting Dict by parsing GF files
-----------------------------------

---- TODO: use the real GF source file parser
 
-- from one-line GF rules: (lin) work_V = mkV "arbeta" ;
gfLine2lin :: String -> [(Fun,[Rule],Metadata)]
gfLine2lin = get . words where
  get ws = case ws of
    "lin":ww   -> get ww                                           -- drop leading lin
    _  :"=":"variants":('{':'}':_):_ -> []
    fun:"=":ww -> [(fun, map lin2rule (variants (unwords ww)), srcMetadata "gf")]    ---- metadata actually per rule, not per variant
    _ -> [] ---error ("cannot get lin rule from " ++ unwords ws)

-- this reads a GF lexicon file, accepting lines with '=' and ';'
---- TODO: should get opens and extends from the header, via parser
gfFile2dictionaryUpdate :: Dictionary -> Lang -> String -> Dictionary
gfFile2dictionaryUpdate dict lang s = 
  updateDictionary [(lang, concatMap gfLine2lin (filter isLinRule (lines s)))] dict ---- TODO: abstract metadata

isLinRule :: String -> Bool
isLinRule s = elem '=' s && elem ';' s

getDictFromGFFiles :: [FilePath] -> IO Dictionary
getDictFromGFFiles = foldM updateDictFromGFFile initDictionary

updateDictFromGFFile :: Dictionary -> FilePath -> IO Dictionary
updateDictFromGFFile dict0 file = do
  s <- readFile file
  let lang = getLang $ getModule file
  return $ gfFile2dictionaryUpdate dict0 lang s



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
  updateDictionary [(lang,[(fun,rs,meta) | (fun,_,rs,meta) <- ruls])] dict

analyseLineW :: String -> [(Fun, Cat, String, [String], Metadata)]
analyseLineW l = do
  let ws = words (ignoreSegments '/' '/' l)                  -- ignore pronunciation which is in / ... /
  if (notElem "::" ws || notElem '{' l)
      then fail l 
      else do                                            -- ignore incomplete or ill-formatted line
    let (ws1,c:ws2) = span ((/= '{') . head) ws              -- English lemma and cat
    let fun = mkIdent (unwords ws1)
    let cat = catW c
    let (disamb,_:ws3) = span (/="::") ws2                   -- disambiguation part and linearizations
    if null ws3 
        then fail l                                      -- ignore line with no lin parts
        else do
      let lins = chop ",;" (unwords ws3)                 -- comma-separated lins; semicolons seem to separate senses
      --- let lins = [unwords ws1]                                   --- use this to get Eng from any en-xx
      return (fun,cat,unwords disamb,lins,srcMetadata "Wiktionary")

-- build the final fun names and lin rules
linRulesW :: Lang -> [(Fun, Cat, String, [String], Metadata)] -> [(Fun,Cat,[Rule],Metadata)]
linRulesW lang = map buildRule . concatMap disambFuns . groupBy sameFunCat . concatMap getCats
  where
   getCats r = [r]  ---- TODO: look up subcategories of verbs and expand verb entries accordingly
   sameFunCat (f1,c1,_,_,_) (f2,c2,_,_,_) = f1==f2 && c1==c2
   disambFuns rs = case rs of
     [(f,c,d,ls,m)] -> [(f,glueIdents [f,c], c, d, ls, m)]
     _   -> [(f,disambiguateFun f c d i, c, d, ls, m) | ((f,c,d,ls,m),i) <- zip rs [1..]]   -- save the bare word in metadata
   buildRule (o,f,c,d,ls,m) = (f,c, map (initRuleLang lang c) ls, m ++ glossMetadata d ++ [("bare",o)])


---- TODO: specialize this to languages to handle multiwords, grammatical tags, etc
initRuleLang :: Lang -> Cat -> String -> Rule
initRuleLang lang cat = case lang of 
  _      -> initRule cat . 
            normalizeSpaces .
            ignoreSegments '(' ')' .      --- ignoring rhs disambiguation
            ignoreSegments '{' '}'        --- ignoring grammatical tags, e.g. {n} in Swe for neuter

                         
-- don't use the numbers after all, since they can differ from lang to lang
disambiguateFun :: Fun -> Cat -> String -> Int -> Fun
disambiguateFun f c d _ = glueIdents [f, mkIdent (take 12 (drop 1 d)),c]  --- take first 12 letters from disamb

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


getDictFromWiktionaries :: [(Lang,FilePath)] -> IO Dictionary
getDictFromWiktionaries = foldM updateDictFromWiktionary initDictionary

updateDictFromWiktionary :: Dictionary -> (Lang,FilePath) -> IO Dictionary
updateDictFromWiktionary dict0 (lang,file) = do
  s <- readFile file
  return $ wiktionary2dictionaryUpdate dict0 lang s

wiktionary2dictionaryUpdate :: Dictionary -> Lang -> String -> Dictionary
wiktionary2dictionaryUpdate dict lang s = dictW lang (lines s) dict



--------------------------
--- subcategory annotation
--------------------------

-- answer to question: which subcats are given to this fun-cat combination, e.g. to this verb
type FunMap = M.Map (Fun,Cat) [Cat]  -- e.g. walk_V -> V,V2

funs2funMap :: [Fun] -> FunMap
funs2funMap fs = M.fromListWith union [((glueIdents (h:gs), cat2supercat c),  [c]) | f <- fs, let (h,gs,c) = analyseFun f]

-- file with lines of form 'abbreviate_V2 : V2 ;' can be generated in GF with 'pg -funs'
getFunMap :: FilePath -> IO FunMap
getFunMap file = do
  s <- readFile file
  return $ funs2funMap [w | w:_ <- map words (lines s)]

getDictFunMap :: IO FunMap
getDictFunMap = getFunMap "dictfuns.txt"

-- [(i,length [wc| wc@(_,c) <- Data.Map.assocs fm, length c > i]) | i <- [1..]]
-- [(1,2964),(2,322),(3,118),(4,40),(5,9),(6,3),(7,2),(8,0)]

annotateSubcat :: FunMap -> Dictionary -> Dictionary
annotateSubcat funmap dict = dict {
  entries = M.mapWithKey addSubcats (entries dict)
  }
 where
  addSubcats f e = e {
    subcats = union (subcats e) (maybe [] id (M.lookup (funb f e) funmap))
    }
  funb f e = (fun2firstpart f,cat e) --- might miss a multiword fun in funmap



--------------------------------
-- Populate Dict from Wiktionary
--------------------------------

getDictFromWikt :: IO Dictionary
getDictFromWikt = do
  dict <- getDictFromGFFiles      [           myGFDictDir ++ "swedish/DictEngSwe.gf"]
  wikt <- getDictFromWiktionaries [("Swe",    myWiktDir ++ "en-sv-enwiktionary.txt")]
  return dict ----
