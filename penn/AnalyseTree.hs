module AnalyseTree where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import LexTree
import ParTree
import PrintTree
import AbsTree

import ErrM

-- utilities

parseGTree :: String -> GTree
parseGTree s = case pGTree (myLexer s) of
  Ok t -> t
  _ -> GTAtom GMeta 

chunkResolve :: GTree -> [GTree]
chunkResolve = map addUtt . snd . chunks . resolve

-- to print frequencies of ? types

analyseMetaFreq :: [GTree] -> String
analyseMetaFreq = unlines . metaFreqs . concatMap (metas "Phr")

analyseFunFreq :: [GTree] -> String
analyseFunFreq = unlines . funFreqs

-- to print chunkings of trees
analyseChunk :: [GTree] -> String
analyseChunk = unlines . map (unlines . map (printTree . addUtt) . snd . chunks) 

-- to resolve metavariables
analyseResolve :: [GTree] -> String
analyseResolve = unlines . map (printTree . resolve)

-- to normalize tree (to enable the use of diff)
normalizeTrees :: Treebank -> String
normalizeTrees (GB as) = unlines $ [printTree at | at <- as]

addUtt :: GTree -> GTree
addUtt t = case infer metaType t of
  "Conj" -> coerce "PConjConj" t
  ty -> case M.lookup (ty,"Utt") gratisMap of
    Just (f:_) -> coerce f t
    _ -> t
 where
   coerce f t = GTApp (GFun (Ident f)) [t]

metaFreqs sigs = map (\ (n,t) -> prSig t ++ "\t" ++ show n) $ reverse $ sort $ [(length ns,ty) | ns@(ty:_) <- group (sort sigs)]

funFreqs ts = map (\ (n,t) -> t ++ "\t" ++ show n) $ reverse $ sort $ [(length ns,ty) | ns@(ty:_) <- group (sort (concatMap getFuns ts))]

getFuns :: GTree -> [Fun]
getFuns t = case t of
  GTApp atom ts -> atFuns atom ++ concatMap getFuns ts
  GTAtom atom -> atFuns atom
 where
  atFuns atom = case atom of
    GFun (Ident f) -> [f]
    _ -> []

type Fun = String

type Type = String

infer :: Type -> GTree -> Type
infer exp t = case t of
  GTApp atom ts -> infer exp (GTAtom atom)
  GTAtom a -> case a of
    GFun f -> snd $ look f
    GMeta -> exp
    GStr _ -> "String"


data MTree = 
   MTApp GAtom [MTree]
 | MTMacro ([GTree] -> GTree) [MTree]
 | MTAtom GAtom


-- try to fill metavariables from MacroMap
resolve :: GTree -> GTree
resolve = mtree2gtree . gtree2mtree "Phr"

mtree2gtree :: MTree -> GTree
mtree2gtree t = case t of
  MTApp atom ts -> GTApp atom (map mtree2gtree ts)
  MTMacro f ts  -> f (map mtree2gtree ts)
  MTAtom atom -> GTAtom atom

gtree2mtree :: Type -> GTree -> MTree
gtree2mtree exp t = case t of
  GTApp GMeta ts -> let tys = map (infer metaType) ts in case M.lookup (tys,exp) macroMap of
    Just f -> MTMacro f [gtree2mtree ty tr | (ty,tr) <- zip tys ts]
    _ -> MTApp GMeta [gtree2mtree ty tr | (ty,tr) <- zip tys ts]
  GTApp (GFun f) ts -> let 
                         tys = [infer ty tr | (ty,tr) <- zip (fst (look f)) ts] 
                       in MTApp (GFun f) [gtree2mtree ty tr | (ty,tr) <- zip tys ts]
  GTAtom a -> MTAtom a

metaType = "?"

-- all types of metas in a given tree
metas :: Type -> GTree -> [Sig]
metas exp t = case t of
  GTApp atom ts -> case atom of
    GFun f -> concat [metas ty tr | (ty,tr) <- zip (fst (look f)) ts]
    GMeta -> (map (infer metaType) ts,exp) : concatMap (metas metaType) ts
    _ -> []
  GTAtom atom -> case atom of
    GMeta -> [([],exp)]
    _ -> []


chunks :: GTree -> (Bool,[GTree])
chunks t = case t of
  GTApp (GFun f) ts -> case unzip (map chunks ts) of 
    (bs,_) | and bs -> (True,[t])
    (_,cts) -> (False,concat cts)
  GTApp _ ts -> (False,concatMap (snd . chunks) ts)
  _ -> (True,[t])

look :: Ident -> Sig
look (Ident f) = case M.lookup f funs of
  Just sig -> sig
  _ -> ([],suffix f)

suffix = reverse . takeWhile (/='_') . reverse -- for lexical items

type Sig = ([String],String)

pSig s = let cats = [w | (i,w) <- zip [0..] (words s), even i] in (init cats, last cats)

prSig (xs,v) = unwords (intersperse "->" (xs ++ [v]))

cover :: Sig -> [GTree]
cover t@(xs,v) = case M.lookup t covers of
  Just fs -> [app f xs | f <- fs]

app f xs = GTApp (gFun f) [GTAtom (gFun x) | x <- xs]

gFun = GFun . Ident

covers :: M.Map Sig [String]
covers = M.fromListWith (++) [(t,[f]) | (f,t) <- funList]

funs :: M.Map String Sig
funs = M.fromList funList

apf :: String -> [GTree] -> GTree
apf f ts = GTApp (GFun (Ident f)) ts

atf :: String -> GTree
atf f = GTAtom (GFun (Ident f))


------------------------- the data from RGL+Parse abstract syntax

-- common patterns for ? functions in Penn
macroMap :: M.Map Sig ([GTree] -> GTree)
macroMap = M.fromList [
  ((["Quant","N"], "NP"),\ [q,n] -> apf "DetCN" [apf "DetQuant" [atf "NumSg", q], apf "UseN" [n]]), -- 3609
  ((["NP","VPS"], "S"),\ [np,vps] -> apf "PredVPS" [np,vps]),
  ((["N"],"NP"), \ [n] -> apf "MassNP" [apf "UseN" [n]]), -- 3193
  ((["NP"],"NP"), \ [n] -> n), -- 1312
  ((["Quant"], "NP"),\ [q] -> apf "DetNP" [apf "DetQuant" [atf "NumSg", q]]) -- 1021
 ]

-- functions that can have an empty linearization and are therefore usable for padding
nullableMap = M.fromList [("Ant",["ASimul"]),("Num",["NumPl"]),("PConj",["NoPConj"]),("Pol",["PPos"]),("Quant",["IndefArt"]),("Tense",["TPres"]),("Voc",["NoVoc"])] 

-- one-place coercions that can always be inserted
gratisMap = M.fromList [(("A","AP"),["PositA"]),(("A","AdA"),["PositAdAAdj"]),(("A","AdV"),["PositAdVAdj"]),(("A","Adv"),["PositAdvAdj"]),(("A2","AP"),["UseA2"]),(("AP","Comp"),["CompAP"]),(("AP","Utt"),["UttAP"]),(("AdV","Utt"),["UttAdV"]),(("Adv","Comp"),["CompAdv"]),(("Adv","Utt"),["UttAdv"]),(("CAdv","AdN"),["AdnCAdv"]),(("CN","Comp"),["CompCN"]),(("CN","NP"),["MassNP"]),(("CN","Utt"),["UttCN"]),(("Card","Num"),["NumCard"]),(("Card","Utt"),["UttCard"]),(("Cl","QCl"),["QuestCl"]),(("Cl","RCl"),["RelCl"]),(("ClSlash","RCl"),["EmptyRelSlash"]),(("Comp","VP"),["UseComp"]),(("Conj","PConj"),["PConjConj"]),(("Det","NP"),["DetNP"]),(("Dig","Digits"),["IDig"]),(("Digits","Card"),["NumDigits"]),(("Digits","Ord"),["OrdDigits"]),(("IAdv","IComp"),["CompIAdv"]),(("IAdv","Utt"),["UttIAdv"]),(("IDet","IP"),["IdetIP"]),(("IP","IComp"),["CompIP"]),(("IP","Utt"),["UttIP"]),(("Interj","Utt"),["UttInterj"]),(("N","CN"),["UseN"]),(("N2","CN"),["UseN2"]),(("N3","N2"),["Use2N3"]),(("NP","Comp"),["CompNP"]),(("NP","Utt"),["UttNP"]),(("NP","Voc"),["VocNP"]),(("Numeral","Card"),["NumNumeral"]),(("Numeral","Ord"),["OrdNumeral"]),(("Ord","AP"),["AdjOrd"]),(("PN","NP"),["UsePN"]),(("Pron","NP"),["UsePron"]),(("QS","Comp"),["CompQS"]),(("QS","SC"),["EmbedQS"]),(("QS","Utt"),["UttQS"]),(("S","Comp"),["CompS"]),(("S","SC"),["EmbedS"]),(("S","Utt"),["UttS"]),(("String","Symb"),["MkSymb"]),(("Symb","PN"),["SymbPN"]),(("V","VP"),["UseV"]),(("V2","AP"),["PastPartAP"]),(("V2","VPSlash"),["SlashV2a"]),(("VP","Imp"),["ImpVP"]),(("VP","SC"),["EmbedVP"]),(("VP","Utt"),["UttVP"]),(("VP","VP"),["ProgrVP"]),(("VP","VPI"),["MkVPI"])] 

funList = [("AAnter",([],"Ant")),("ASimul",([],"Ant")),("AdAP",(["AdA","AP"],"AP")),("AdAdV",(["AdA","AdV"],"AdV")),("AdAdv",(["AdA","Adv"],"Adv")),("AdNum",(["AdN","Card"],"Card")),("AdVVP",(["AdV","VP"],"VP")),("AdVVPSlash",(["AdV","VPSlash"],"VPSlash")),("AddAdvQVP",(["QVP","IAdv"],"QVP")),("AdjCN",(["AP","CN"],"CN")),("AdjOrd",(["Ord"],"AP")),("AdnCAdv",(["CAdv"],"AdN")),("AdvAP",(["AP","Adv"],"AP")),("AdvCN",(["CN","Adv"],"CN")),("AdvIAdv",(["IAdv","Adv"],"IAdv")),("AdvIP",(["IP","Adv"],"IP")),("AdvNP",(["NP","Adv"],"NP")),("AdvQVP",(["VP","IAdv"],"QVP")),("AdvS",(["Adv","S"],"S")),("AdvSlash",(["ClSlash","Adv"],"ClSlash")),("AdvVP",(["VP","Adv"],"VP")),("AdvVPSlash",(["VPSlash","Adv"],"VPSlash")),("ApposCN",(["CN","NP"],"CN")),("ApposNP",(["NP","NP"],"NP")),("BaseAP",(["AP","AP"],"ListAP")),("BaseAdv",(["Adv","Adv"],"ListAdv")),("BaseCN",(["CN","CN"],"ListCN")),("BaseIAdv",(["IAdv","IAdv"],"ListIAdv")),("BaseNP",(["NP","NP"],"ListNP")),("BaseRS",(["RS","RS"],"ListRS")),("BaseS",(["S","S"],"ListS")),("BaseVPI",(["VPI","VPI"],"ListVPI")),("BaseVPS",(["VPS","VPS"],"ListVPS")),("CAdvAP",(["CAdv","AP","NP"],"AP")),("CNNumNP",(["CN","Card"],"NP")),("CompAP",(["AP"],"Comp")),("CompAdv",(["Adv"],"Comp")),("CompCN",(["CN"],"Comp")),("CompIAdv",(["IAdv"],"IComp")),("CompIP",(["IP"],"IComp")),("CompNP",(["NP"],"Comp")),("CompQS",(["QS"],"Comp")),("CompS",(["S"],"Comp")),("CompVP",(["Ant","Pol","VP"],"Comp")),("ComparA",(["A","NP"],"AP")),("ComparAdvAdj",(["CAdv","A","NP"],"Adv")),("ComparAdvAdjS",(["CAdv","A","S"],"Adv")),("ComplA2",(["A2","NP"],"AP")),("ComplN2",(["N2","NP"],"CN")),("ComplN3",(["N3","NP"],"N2")),("ComplSlash",(["VPSlash","NP"],"VP")),("ComplSlashIP",(["VPSlash","IP"],"QVP")),("ComplVA",(["VA","AP"],"VP")),("ComplVPIVV",(["VV","VPI"],"VP")),("ComplVQ",(["VQ","QS"],"VP")),("ComplVS",(["VS","S"],"VP")),("ComplVV",(["VV","Ant","Pol","VP"],"VP")),("CompoundCN",(["Num","N","CN"],"CN")),("ConjAP",(["Conj","ListAP"],"AP")),("ConjAdv",(["Conj","ListAdv"],"Adv")),("ConjCN",(["Conj","ListCN"],"CN")),("ConjIAdv",(["Conj","ListIAdv"],"IAdv")),("ConjNP",(["Conj","ListNP"],"NP")),("ConjRS",(["Conj","ListRS"],"RS")),("ConjS",(["Conj","ListS"],"S")),("ConjVPI",(["Conj","ListVPI"],"VPI")),("ConjVPS",(["Conj","ListVPS"],"VPS")),("ConsAP",(["AP","ListAP"],"ListAP")),("ConsAdv",(["Adv","ListAdv"],"ListAdv")),("ConsCN",(["CN","ListCN"],"ListCN")),("ConsIAdv",(["IAdv","ListIAdv"],"ListIAdv")),("ConsNP",(["NP","ListNP"],"ListNP")),("ConsRS",(["RS","ListRS"],"ListRS")),("ConsS",(["S","ListS"],"ListS")),("ConsVPI",(["VPI","ListVPI"],"ListVPI")),("ConsVPS",(["VPS","ListVPS"],"ListVPS")),("CountNP",(["Det","NP"],"NP")),("D_0",([],"Dig")),("D_1",([],"Dig")),("D_2",([],"Dig")),("D_3",([],"Dig")),("D_4",([],"Dig")),("D_5",([],"Dig")),("D_6",([],"Dig")),("D_7",([],"Dig")),("D_8",([],"Dig")),("D_9",([],"Dig")),("DashCN",(["N","N"],"N")),("DefArt",([],"Quant")),("DetCN",(["Det","CN"],"NP")),("DetNP",(["Det"],"NP")),("DetQuant",(["Quant","Num"],"Det")),("DetQuantOrd",(["Quant","Num","Ord"],"Det")),("EmbedQS",(["QS"],"SC")),("EmbedS",(["S"],"SC")),("EmbedVP",(["VP"],"SC")),("EmptyRelSlash",(["ClSlash"],"RCl")),("ExistNP",(["NP"],"Cl")),("ExtAdvS",(["Adv","S"],"S")),("FunRP",(["Prep","NP","RP"],"RP")),("GenNP",(["NP"],"Quant")),("GenRP",(["Num","CN"],"RP")),("GerundAP",(["V"],"AP")),("GerundN",(["V"],"N")),("IDig",(["Dig"],"Digits")),("IIDig",(["Dig","Digits"],"Digits")),("IdRP",([],"RP")),("IdetCN",(["IDet","CN"],"IP")),("IdetIP",(["IDet"],"IP")),("IdetQuant",(["IQuant","Num"],"IDet")),("ImpVP",(["VP"],"Imp")),("IndefArt",([],"Quant")),("MassNP",(["CN"],"NP")),("MkSymb",(["String"],"Symb")),("MkVPI",(["VP"],"VPI")),("MkVPS",(["Temp","Pol","VP"],"VPS")),("NoPConj",([],"PConj")),("NoVoc",([],"Voc")),("NumCard",(["Card"],"Num")),("NumDigits",(["Digits"],"Card")),("NumNumeral",(["Numeral"],"Card")),("NumPl",([],"Num")),("NumSg",([],"Num")),("OrdCompar",(["A"],"Ord")),("OrdDigits",(["Digits"],"Ord")),("OrdNumeral",(["Numeral"],"Ord")),("OrdSuperl",(["A"],"Ord")),("PConjConj",(["Conj"],"PConj")),("PNeg",([],"Pol")),("PPos",([],"Pol")),("PartNP",(["CN","NP"],"CN")),("PassVPSlash",(["VPSlash"],"VP")),("PastPartAP",(["V2"],"AP")),("PastPartRS",(["Ant","Pol","VPSlash"],"RS")),("PhrUtt",(["PConj","Utt","Voc"],"Phr")),("PositA",(["A"],"AP")),("PositAdAAdj",(["A"],"AdA")),("PositAdVAdj",(["A"],"AdV")),("PositAdvAdj",(["A"],"Adv")),("PossNP",(["CN","NP"],"CN")),("PossPron",(["Pron"],"Quant")),("PredSCVP",(["SC","VP"],"Cl")),("PredVP",(["NP","VP"],"Cl")),("PredVPS",(["NP","VPS"],"S")),("PredVPosv",(["NP","VP"],"Cl")),("PredVPovs",(["NP","VP"],"Cl")),("PredetNP",(["Predet","NP"],"NP")),("PrepIP",(["Prep","IP"],"IAdv")),("PrepNP",(["Prep","NP"],"Adv")),("PresPartRS",(["Ant","Pol","VP"],"RS")),("ProgrVP",(["VP"],"VP")),("QuestCl",(["Cl"],"QCl")),("QuestIAdv",(["IAdv","Cl"],"QCl")),("QuestIComp",(["IComp","NP"],"QCl")),("QuestQVP",(["IP","QVP"],"QCl")),("QuestSlash",(["IP","ClSlash"],"QCl")),("QuestVP",(["IP","VP"],"QCl")),("ReflA2",(["A2"],"AP")),("ReflVP",(["VPSlash"],"VP")),("RelCN",(["CN","RS"],"CN")),("RelCl",(["Cl"],"RCl")),("RelNP",(["NP","RS"],"NP")),("RelS",(["S","RS"],"S")),("RelSlash",(["RP","ClSlash"],"RCl")),("RelVP",(["RP","VP"],"RCl")),("SSubjS",(["S","Subj","S"],"S")),("SentAP",(["AP","SC"],"AP")),("SentCN",(["CN","SC"],"CN")),("Slash2V3",(["V3","NP"],"VPSlash")),("Slash3V3",(["V3","NP"],"VPSlash")),("SlashPrep",(["Cl","Prep"],"ClSlash")),("SlashSlashV2V",(["V2V","Ant","Pol","VPSlash"],"VPSlash")),("SlashV2A",(["V2A","AP"],"VPSlash")),("SlashV2Q",(["V2Q","QS"],"VPSlash")),("SlashV2S",(["V2S","S"],"VPSlash")),("SlashV2V",(["V2V","Ant","Pol","VP"],"VPSlash")),("SlashV2VNP",(["V2V","NP","VPSlash"],"VPSlash")),("SlashV2a",(["V2"],"VPSlash")),("SlashVP",(["NP","VPSlash"],"ClSlash")),("SlashVPIV2V",(["V2V","Pol","VPI"],"VPSlash")),("SlashVS",(["NP","VS","SSlash"],"ClSlash")),("SlashVV",(["VV","VPSlash"],"VPSlash")),("SubjS",(["Subj","S"],"Adv")),("SymbPN",(["Symb"],"PN")),("TCond",([],"Tense")),("TFut",([],"Tense")),("TPast",([],"Tense")),("TPres",([],"Tense")),("TTAnt",(["Tense","Ant"],"Temp")),("UncNeg",([],"Pol")),("Use2N3",(["N3"],"N2")),("Use3N3",(["N3"],"N2")),("UseA2",(["A2"],"AP")),("UseCl",(["Temp","Pol","Cl"],"S")),("UseComp",(["Comp"],"VP")),("UseComparA",(["A"],"AP")),("UseN",(["N"],"CN")),("UseN2",(["N2"],"CN")),("UsePN",(["PN"],"NP")),("UsePron",(["Pron"],"NP")),("UseQCl",(["Temp","Pol","QCl"],"QS")),("UseQuantPN",(["Quant","PN"],"NP")),("UseRCl",(["Temp","Pol","RCl"],"RS")),("UseSlash",(["Temp","Pol","ClSlash"],"SSlash")),("UseV",(["V"],"VP")),("UttAP",(["AP"],"Utt")),("UttAdV",(["AdV"],"Utt")),("UttAdv",(["Adv"],"Utt")),("UttCN",(["CN"],"Utt")),("UttCard",(["Card"],"Utt")),("UttIAdv",(["IAdv"],"Utt")),("UttIP",(["IP"],"Utt")),("UttImpPl",(["Pol","Imp"],"Utt")),("UttImpPol",(["Pol","Imp"],"Utt")),("UttImpSg",(["Pol","Imp"],"Utt")),("UttInterj",(["Interj"],"Utt")),("UttNP",(["NP"],"Utt")),("UttQS",(["QS"],"Utt")),("UttS",(["S"],"Utt")),("UttVP",(["VP"],"Utt")),("VPSlashPrep",(["VP","Prep"],"VPSlash")),("VPSlashVS",(["VS","VP"],"VPSlash")),("VocNP",(["NP"],"Voc")),("herself_NP",([],"NP")),("himself_NP",([],"NP")),("itself_NP",([],"NP")),("myself_NP",([],"NP")),("n2",([],"Digit")),("n3",([],"Digit")),("n4",([],"Digit")),("n5",([],"Digit")),("n6",([],"Digit")),("n7",([],"Digit")),("n8",([],"Digit")),("n9",([],"Digit")),("num",(["Sub1000000"],"Numeral")),("ourself_NP",([],"NP")),("pot0",(["Digit"],"Sub10")),("pot01",([],"Sub10")),("pot0as1",(["Sub10"],"Sub100")),("pot1",(["Digit"],"Sub100")),("pot110",([],"Sub100")),("pot111",([],"Sub100")),("pot1as2",(["Sub100"],"Sub1000")),("pot1plus",(["Digit","Sub10"],"Sub100")),("pot1to19",(["Digit"],"Sub100")),("pot2",(["Sub10"],"Sub1000")),("pot2as3",(["Sub1000"],"Sub1000000")),("pot2plus",(["Sub10","Sub100"],"Sub1000")),("pot3",(["Sub1000"],"Sub1000000")),("pot3plus",(["Sub1000","Sub1000"],"Sub1000000")),("that_RP",([],"RP")),("themself_NP",([],"NP")),("themselves_NP",([],"NP")),("yourselfPl_NP",([],"NP")),("yourselfSg_NP",([],"NP"))]



----------- new stuff in GF Summer School

analyseSummarize :: [GTree] -> String
analyseSummarize = unlines . map (printTree . summarize)

summarize :: GTree -> GTree 
summarize t = case t of
  GTApp h@(GFun (Ident f)) ts -> case f of
    "AdjCN" -> summarize (ts !! 1)
    _ -> GTApp h (map summarize ts)

  GTApp h ts -> GTApp h (map summarize ts)
  _ -> t
