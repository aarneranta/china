concrete HSKGrammarChi of HSKGrammar = 

  GrammarChi [

   Phr, Utt, NP, VP, Cl, Adv, V2, AP, A, AdA, Det, S, QS, QCl, IP, IDet, IQuant,
   ClSlash, VPSlash, Pron, Pol, Temp, Tense, Ant, RS, RCl, RP, CN, Comp, VV, N, PN,
   Predet, Prep, Quant, IQuant, Num, Interj, Numeral,
   IAdv, TTAnt, AAnter, ASimul, TPres, PPos, PNeg,

   PredVP,     -- NP -> VP -> Cl
   SlashV2a,   -- V2 -> VPSlash
   ComplSlash, -- VPSlash -> NP -> VP
   AdvVP,      -- VP -> Adv -> VP
   CompAP,     -- AP -> Comp
   UseComp,    -- Comp -> VP
   PositA,     -- A -> AP
   AdAP,       -- AdA -> AP -> AP
   DetCN,      -- Det -> CN -> NP
   DetNP,      -- Det -> NP
   MassNP,     -- CN -> NP 
   UseN,       -- N -> CN
   UseCl,      -- Cl -> S            -- with bu/mei
   QuestCl,    -- Cl -> QS           -- with ma
   UseQCl,     -- QCl -> QS
   ComplVV,    -- VV -> VP -> VP 
   QuestSlash, -- IP -> ClSlash -> QCl
   SlashVP,    -- NP -> VPSlash -> ClSlash
   QuestVP,    -- IP -> VP -> QCl
   IdetCN,     -- IDet -> CN -> IP
   PossPron,   -- Pron -> Quant
   RelCN,      -- CN -> RS -> CN
   RelSlash,   -- RP -> ClSlash -> RCl
   UseRCl,     -- RCl -> RS
   UsePron,    -- Pron -> NP
   DetQuant,   -- Quant -> Num -> Det
   UttQS,      -- QS -> Utt
   UttS,       -- S -> Utt
   IdetQuant,  -- IQuant -> Num -> IDet
   IdetCN,     -- IDet -> CN -> IP
   AdvNP,      -- NP -> Adv -> NP
   QuestIAdv,  -- IAdv -> Cl -> QCl
   UseV,       -- V -> VP
   UsePN,      -- PN -> NP
   PrepNP,     -- Prep -> NP -> Adv
   AdjCN,      -- AP -> CN -> CN 
   UttInterj,  -- Interj -> Utt

   IdRP,
   NumSg,
   NumPl,
   
   i_Pron,
   youSg_Pron,
   we_Pron,
   he_Pron,
   she_Pron,
   too_AdA,
   all_Predet,
   this_Quant,
   that_Quant,
   which_IQuant,
   have_V2,
   much_Det,
   few_Det,
   how8much_IP,
   who_IP,
   here_Adv,
   there_Adv,
   where_IAdv
--   in_Prep
   ],

 NumeralChi - [pot2, pot2plus, pot3, pot3plus],  -- just 1--99

 LexiconChi [
   A, V2, N, V,

   love_V2,
   like_V2,
   buy_V2,
   good_A,
   friend_N,
   beautiful_A,
   small_A,
   big_A,
   book_N,
--   warm_A,
--   cold_A,
--   live_V,
   go_V,
   come_V,
   now_Adv

   ]

     ** open SyntaxChi, ParadigmsChi, (E = ExtraChi), (R = ResChi), (G = GrammarChi) 
       in {

flags coding = utf8 ;

lincat
  Day = PN ;
  Month = PN ;

lin
  PredDetNP det np = mkCl (lin NP {s = det.s}) (lin NP np) ;
  PredAP np ap = E.PredBareAP np ap ;
  leS cl = lin S {s = (mkS cl).s ++ "了"} ;
  neQS cl = lin QS {s = (mkS cl).s ++ "呢"} ;
  buQS cl = mkQS (E.QuestRepV cl) ;
  heNP x y = lin NP {s = x.s ++ "和" ++ y.s} ; 
  theNP cn = mkNP the_Det (lin CN cn) ;

  ji_IDet = how8many_IDet ;
  ji_Det = mkpDet "几" ;
  how_IAdv = how_IAdv ;
  how_about_IAdv = how_IAdv ;
  hen_AdA = very_AdA ;
  dou_Predet = all_Predet ;
  dou_NP = mkpNP "都" ;
  a_few_Det = mkpDet "些" ;
  verygood_AdA = lin AdA {s = "好"} ;
  extremely_AdA = lin mkAdA {s = "太"} ;

  money_N = mkN "钱" ;
  person_N = mkN "人" ;
  like_VV = mkVV "喜欢" ;
  happy_A = mkA "高兴" ;
  great_Interj = lin Interj {s = R.word "太好了"} ;
  clothes_N = mkN "衣服" ; ----

  the_weather_NP = mkpNP "天气" ;

  here_in_Adv = mkAdv "这儿" ;
  there_in_Adv = mkAdv "那儿" ;

  warm_A = mkA "热" ;
  cold_A = mkA "冷" ;
  live_V = mkV "住" ; -- reside
  in_Prep = mkPrep "在" ; -- in a city

  beijing_PN = mkPN "北京" ; 
  china_PN = mkPN "中国" ;

--- get 我在中国住, expected 我住在中国

  go_V2 = mkV2 "去" ;
  up_Adv = mkAdv "上" ;
  down_Adv = mkAdv "下" ;

  job_N = mkN "工作" ;
  work_V = mkV "工作" ;
  doctor_N = mkN "医生" ;
  hospital_N = mkN "医院" ;
  restaurant_N = mkN "饭馆" ;
  shop_N = mkN "商店" ;
  at_Prep = mkPrep "在" ;
  in_front_of = mkPrep "在" "的前面" ;
  behind_Prep = mkPrep "在" "的后面" ;
  ExistInCl adv cn = E.TopicAdvCl adv (mkCl (lin CN cn)) ;
  MisterPN pn = {s = pn.s ++ R.word "先生"} ;
  qian_PN = mkPN "钱" ;
  hello_Interj = mkInterj "你好" ;
  uttVocPhr utt np = mkPhr <utt : Utt> (mkVoc np) ;
  uttPhr utt = mkPhr <utt : Utt> ;

  numeralNP num cn = mkNP <num : Numeral> <cn : CN> ;

  time_N = mkN "时候" ;
  when_IAdv = lin IAdv {s = "么时候来"} ;
  yesterday_Adv = mkAdv "昨天" ;
  today_Adv = mkAdv "今天" ;
  tomorrow_Adv = mkAdv "明天" ;

  january_Month = mkPN "一月" ;
  february_Month = mkPN "二月" ;
  march_Month = mkPN "三月" ;
  april_Month = mkPN "四月" ;
  may_Month = mkPN "五月" ;
  june_Month = mkPN "六月" ;
  july_Month = mkPN "七月" ;
  august_Month = mkPN "八月" ;
  september_Month = mkPN "九月" ;
  october_Month = mkPN "十月" ;
  november_Month = mkPN "十一月" ;
  december_Month = mkPN "十二月" ;

  month_N = mkN "月" ;
  moon_N = mkN "月" ;
  sun_N = mkN "日" ;
  day_N = mkN "日" "天" ; ---
  year_N = mkN "年" [] ;
  week_N = mkN "星期" ;

  monday_Day = mkPN "星期一" ;
  tuesday_Day = mkPN "星期二" ;
  wednesday_Day = mkPN "星期三" ;
  thursday_Day = mkPN "星期四" ;
  friday_Day = mkPN "星期五" ;
  saturday_Day = mkPN "星期六" ;
  sunday_Day = mkPN "星期日" | mkPN "星期天" ;

  monthPN d = d ;
  dayPN d = d ;

  }