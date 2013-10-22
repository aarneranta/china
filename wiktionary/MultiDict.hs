import Data.List
import Data.Char
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
  metar     :: Metadata   -- source, status, date, author...
  }

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
  dict       :: M.Map Fun Entry,        -- the abstract and concrete syntaxes
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
  dict       = M.empty,
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



-------------------
-- queries
-------------------


abstractDict :: Dictionary -> M.Map Fun (Cat,Weight)
abstractDict d = M.fromList [(f,(cat e, weight e)) | (f,e) <- M.assocs (dict d)]

concreteLang :: Lang -> Dictionary -> M.Map Fun [Rule]
concreteLang lang d = M.fromList [(f,rs) | (f,e) <- M.assocs (dict d), Just rs <- [M.lookup lang (rules e)]]


allFuns :: Dictionary -> [Fun]
allFuns = M.keys . dict

allLanguages :: Dictionary -> [Lang]
allLanguages = M.keys . opens

lookIf :: Ord k => M.Map k [v] -> k -> [v]
lookIf m k = maybe [] id $ M.lookup k m


-------------------
-- printing Dict
-------------------

prDict :: Module -> Dictionary -> IO ()
prDict name d = do
  writeFile (gfFile name) (prAbstract name d)
  mapM_ (\lang -> writeFile (gfFile (langModule name lang)) (prConcrete name lang d)) (allLanguages d)

prAbstract :: Module -> Dictionary -> String
prAbstract name d = unlines $
  [unwords (["abstract",name,"="] ++ intersperse ", " (baseabs d) ++ ["**", "{"])] ++
  [prAbsRule f c w | (f,(c,w)) <- M.assocs (abstractDict d)] ++
  ["}"]

prConcrete :: Module -> Lang -> Dictionary -> String 
prConcrete name lang d = unlines $
  [unwords (["concrete",langModule name lang,"of", name, "="] ++ intersperse ", " (lookIf (basecncs d) lang))] ++
  [unwords (["**","open"] ++ intersperse ", " (lookIf (opens d) lang) ++ ["{"])] ++
  [prCncRule f rs | (f,rs) <- M.assocs (concreteLang lang d)] ++
  ["}"]

prAbsRule :: Fun -> Cat -> Weight -> String
prAbsRule f c w = unwords ["fun",f,":",c,";","--",show w]

prCncRule :: Fun -> [Rule] -> String
prCncRule f rs = unwords $ ["lin",f,"="] ++ (intersperse "|" (map lin rs)) ++ [";"]  ---- ignoring metadata




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


gfFile :: Module -> FilePath
gfFile m = m ++ ".gf"


