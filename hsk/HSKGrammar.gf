abstract HSKGrammar = 

  Grammar [

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
   DetNP,      -- Det -> NP 
   MassNP,     -- CN -> NP 
   UseN,       -- N -> CN
   UseCl,      -- Cl -> S            -- with bu/mei
   QuestCl,    -- Cl -> QCl          -- with ma
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
   where_IAdv,
   in_Prep
   ],

 Numeral - [pot2, pot2plus, pot3, pot3plus],  -- just 1--99

 Lexicon [
   A, V2, N,V,

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
   live_V, -- reside
   go_V,
   come_V,
   now_Adv
   ]

     ** {

fun
  PredDetNP : Det -> NP -> Cl ;    -- this is me (zhe shi wo)
  PredAP : NP -> AP -> Cl ;        -- you (are) good
  leS  : Cl -> S ;             -- with le
  neQS : Cl -> QS ;            -- with ne
  buQS : Cl -> QS ;            -- with bu/mei V n V
  heNP : Pron -> Pron -> NP ;  -- x with/and y
  theNP : CN -> NP ;           -- the man

  ji_IDet : IDet ;
  ji_Det : Det ;
  how_IAdv : IAdv ;
  how_about_IAdv : IAdv ;
  hen_AdA : AdA ;
  dou_Predet : Predet ;
  dou_NP : NP ;
  a_few_Det : Det ;
  verygood_AdA : AdA ;
  extremely_AdA : AdA ;

  money_N : N ;
  person_N : N ;
  like_VV : VV ;
  happy_A : A ;
  great_Interj : Interj ;
  clothes_N : N ;

  the_weather_NP : NP ;
  here_in_Adv : Adv ;
  there_in_Adv : Adv ;

  beijing_PN : PN ;
  china_PN : PN ;
  go_V2 : V2 ;
  up_Adv : Adv ;
  down_Adv : Adv ;

  job_N : N ;
  work_V : V ;
  doctor_N : N ;
  hospital_N : N ;
  restaurant_N : N ;
  shop_N : N ;
  at_Prep : Prep ;
  in_front_of : Prep ;
  behind_Prep : Prep ;
  ExistInCl : Adv -> CN -> Cl ;  -- there is a shop behind the hospital
  MisterPN : PN -> NP ;
  qian_PN : PN ;
  hello_Interj : Interj ;
  uttVocPhr : Utt -> NP -> Phr ;
  uttPhr : Utt -> Phr ;

  numeralNP : Numeral -> CN -> NP ;

  }