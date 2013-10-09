concrete HSKGrammarEng of HSKGrammar = 

  GrammarEng [

   Phr, Utt, NP, VP, Cl, Adv, V2, AP, A, AdA, Det, S, QS, QCl, IP, IDet, IQuant,
   ClSlash, VPSlash, Pron, Pol, Temp, Tense, Ant, RS, RCl, RP, CN, Comp, VV, N, V, PN,
   Predet, Prep, Quant, IQuant, Num, Interj, Numeral,
   IAdv, TTAnt, AAnter, ASimul, TPres, PPos, PNeg,

   PredVP,     -- NP -> VP -> Cl
   SlashV2a,   -- V2 -> VPSlash
   ComplSlash, -- VPSlash -> NP -> VP
   AdvVP,      -- VP -> Adv -> VP
   CompAP,     -- AP -> Comp
   UseComp,    -- Comp -> VP
   PositA,     -- A -> AP
   AdAP,       -- AdA -> A -> AP
   DetCN,      -- Det -> CN -> NP
   MassNP,     -- CN -> NP 
   DetNP,      -- Det -> NP 
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
   
--   IdRP,
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
   where_IAdv,
   in_Prep
   ],

 NumeralEng - [pot2, pot2plus, pot3, pot3plus],  -- just 1--99

 LexiconEng [
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
   warm_A,
   cold_A,
   live_V,
   go_V,
   come_V,
   now_Adv

   ]

     ** open SyntaxEng, ParadigmsEng, 
           (M = MorphoEng), (I = IrregEng), (G = GrammarEng), (E = ExtraEng) 
in {

lincat
  Day = PN ;
  Month = PN ;

lin
  PredDetNP det np = mkCl (mkNP det) np ;
  PredAP np ap = mkCl np ap ;
  leS cl = mkS anteriorAnt cl ;
  neQS cl = mkQS (mkQCl cl) ;
  buQS cl = mkQS (mkQCl cl ) ;
  heNP x y = mkNP and_Conj (mkNP x) (mkNP y) ;
  theNP cn = mkNP the_Det  (lin CN cn) ;

  IdRP = E.that_RP ;

  ji_IDet = how8many_IDet ;
  ji_Det = M.mkDeterminer plural "several" ;
  how_IAdv = how_IAdv ;
  how_about_IAdv = how_IAdv ;
  hen_AdA = very_AdA ;
  dou_Predet = all_Predet ;
  dou_NP = mkNP (M.mkDeterminer plural "all") ;
  a_few_Det = M.mkDeterminer plural "a few" ;
  verygood_AdA = very_AdA ;
  extremely_AdA = lin mkAdA {s = "extremely"} ;

  money_N = mkN "money" ;
  person_N = mkN "person" ;
  like_VV = mkVV (mkV "like") ;
  happy_A = mkA "happy" ;
  great_Interj = lin Interj {s = "great"} ;
  clothes_N = mkN "clothing" ; ----

  the_weather_NP = mkNP the_Det (mkN "weather") ;
  here_in_Adv = here_Adv ;
  there_in_Adv = there_Adv ;
  beijing_PN = mkPN "Beijing" ;
  china_PN = mkPN "China" ;
  go_V2 = mkV2 I.go_V "to" ;
  up_Adv = ParadigmsEng.mkAdv "up" ;
  down_Adv = ParadigmsEng.mkAdv "down" ;

  job_N = mkN "job" ;
  work_V = mkV "work" ;
  doctor_N = mkN "doctor" ;
  hospital_N = mkN "hospital" ;
  restaurant_N = mkN "restaurant" ;
  shop_N = mkN "shop" ;
  at_Prep = mkPrep "at" ;
  in_front_of = SyntaxEng.in8front_Prep ;
  behind_Prep = SyntaxEng.behind_Prep ;
  ExistInCl adv cn = mkCl <G.AdvCN cn adv : CN> ;
  MisterPN pn = mkNP (mkCN (mkN "Mr.") (mkNP pn)) ;
  qian_PN = mkPN "Qian" ;
  hello_Interj = mkInterj "hello" ;
  uttVocPhr utt np = mkPhr <utt : Utt> (mkVoc np) ;
  uttPhr utt = mkPhr <utt : Utt> ;

  numeralNP num cn = mkNP <num : Numeral> <cn : CN> ;

  time_N = mkN "time" ;
  when_IAdv = lin IAdv {s = "when"} ;
  yesterday_Adv = mkAdv "yesterday" ;
  today_Adv = mkAdv "today" ;
  tomorrow_Adv = mkAdv "tomorrow" ;

  january_Month = mkPN "January" ;
  february_Month = mkPN "February" ;
  march_Month = mkPN "March" ;
  april_Month = mkPN "April" ;
  may_Month = mkPN "May" ;
  june_Month = mkPN "June" ;
  july_Month = mkPN "July" ;
  august_Month = mkPN "August" ;
  september_Month = mkPN "September" ;
  october_Month = mkPN "October" ;
  november_Month = mkPN "November" ;
  december_Month = mkPN "December" ;

  month_N = mkN "month" ;
  moon_N = mkN "moon" ;
  sun_N = mkN "sun" ;
  day_N = mkN "day" ; 
  year_N = mkN "year" ;
  week_N = mkN "week" ;

  monday_Day = mkPN "Monday" ;
  tuesday_Day = mkPN "Tuesday" ;
  wednesday_Day = mkPN "Wednesday" ;
  thursday_Day = mkPN "Thursday" ;
  friday_Day = mkPN "Friday" ;
  saturday_Day = mkPN "Saturday" ;
  sunday_Day = mkPN "Sunday" ;

  monthPN d = d ;
  dayPN d = d ;

  }