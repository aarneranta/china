concrete HSKGrammarSwe of HSKGrammar = 

  GrammarSwe [

   Phr, Utt, NP, VP, Cl, Adv, V2, AP, A, AdA, Det, S, QS, QCl, IP, IDet, IQuant,N2,
   ClSlash, VPSlash, Pron, Pol, Temp, Tense, Ant, RS, RCl, RP, CN, Comp, VV, N, V, PN,
   Predet, Prep, Quant, IQuant, Num, Interj, Numeral, VQ, IComp, Imp, 
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
   QuestIAdv,  -- IAdv -> Cl -> QCl
   UseV,       -- V -> VP
   AdjCN,      -- AP -> CN -> CN 
--   UttInterj,  -- Interj -> Utt
   ProgrVP,    -- VP -> VP
   ComplN2,    -- N2 -> NP -> CN

   
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
   where_IAdv,
   want_VV,
   can8know_VV,
   can_VV
   ],

 NumeralSwe - [pot2, pot2plus, pot3, pot3plus],  -- just 1--99

 LexiconSwe [
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
   eat_V2,
   drink_V2,
  sit_V, 
  dog_N,
  cat_N,
  do_V2,
  fruit_N,
  apple_N,
  name_N,
  airplane_N,
  watch_V2,
  television_N,
  rain_V,
  school_N,
  university_N,
  student_N,
  teacher_N,
  listen_V2,
  write_V2,
  sleep_V

   ]

     ** open SyntaxSwe, ParadigmsSwe, 
           (M = MorphoSwe), (I = IrregSwe), (G = GrammarSwe), (E = ExtraSwe), (L = LexiconSwe), (MS = MakeStructuralSwe)
in {

flags coding = utf8 ;

lincat
  Day = PN ;
  Month = PN ;
  Place = NP ;
  PlacePrep = Prep ;
  PlaceAdv = Adv ;
  TimeAdv = Adv ;
  PersonName = PN ;
  Person = NP ;
  MassN = CN ;

lin
  massNP mn = mkNP mn ;
  massCN mn = mn ;
  water_N = mkCN L.water_N ;

  predDetNP det np = mkCl (mkNP (lin Det det)) (lin NP np) ;
  predAP np ap = mkCl (lin NP np) (lin AP ap) ;
  leS cl = mkS anteriorAnt cl ;
  neQS cl = mkQS (mkQCl cl) ;
  buQS cl = mkQS (mkQCl cl ) ;
  heNP x y = mkNP and_Conj (mkNP x) (mkNP y) ;
  theNP cn = mkNP the_Det  (lin CN cn) ;

  UttInterj i = lin Utt i ;

  ji_IDet = how8many_IDet ;
  ji_Det = MS.mkDet "flera" plural ;
  how_IAdv = how_IAdv ;
  how_about_IAdv = how_IAdv ;
  hen_AdA = very_AdA ;
  dou_Predet = all_Predet ;
  dou_NP = mkNP (MS.mkDet "alla" plural) ;
  a_few_Det = MS.mkDet "några" plural ; 
  verygood_AdA = very_AdA ;
  extremely_AdA = lin mkAdA {s = "extremt"} ;

  money_N = mkN "money" ;
  person_N = mkN "person" ;
  like_VV = mkVV (mkV "like") ;
  happy_A = mkA "happy" ;
  great_Interj = lin Interj {s = "utmärkt"} ;
  clothes_N = mkN "klädsel" "klädslar" ; ----

  the_weather_NP = mkNP the_Det (mkN "väder" "vädret") ;
  here_in_Adv = here_Adv ;
  there_in_Adv = there_Adv ;

  in_Prep = G.in_Prep ;
  here_Adv = G.here_Adv ;
  there_Adv = G.there_Adv ;
  now_Adv = L.now_Adv ;
  advPlace prep place = SyntaxSwe.mkAdv prep place ;
  inMonth m = SyntaxSwe.mkAdv in_Prep (mkNP m) ;
  onDay d = SyntaxSwe.mkAdv on_Prep (mkNP d) ;
  placeNPAdv prep np = SyntaxSwe.mkAdv prep np ;
  placeAdv a = a ;
  timeAdv a = a ;
  compPlaceAdv a = mkComp a ;
----  placeAdvNP np a = mkNP np a ;

  beijing_Place = mkNP (mkPN "Beijing") ;
  china_Place = mkNP (mkPN "Kina") ;
  go_V2 = mkV2 L.go_V to_Prep ;
  up_Adv = ParadigmsSwe.mkAdv "upp" ;
  down_Adv = ParadigmsSwe.mkAdv "ner" ;

  job_N = mkN "jobb" "jobb" ;
  work_V = mkV "jobba" ;
  doctor_N = mkN "läkare" "läkere" ;
  hospital_N = mkN "sjukhus" "sjukhus" ;
  restaurant_N = mkN "restaurang" "restauranger" ;
  shop_N = mkN "affär" "affärer" ;
  at_Prep = mkPrep "på" ;
  in_front_of = SyntaxSwe.in8front_Prep ;
  behind_Prep = SyntaxSwe.behind_Prep ;
  ExistInCl adv cn = mkCl <G.AdvCN cn adv : CN> ;
  MisterPN pn = mkNP (mkCN (mkN "herr") (mkNP (lin PN pn))) ;
  qian_PersonName = mkPN "Qian" ;
  namePerson n = mkNP n ;
  personNP p = p ;
  hello_Interj = mkInterj "hej" ;
  uttPhr utt = mkPhr <utt : Utt> ;

  impV v = mkImp v ;
  impV2 v np = mkImp v np ;
  phrVocImp imp p = mkPhr (mkUtt <imp : Imp>) (mkVoc p) ;
  phrVocInterj int p = mkPhr (lin Utt int) (mkVoc p) ;
  uttImp imp = mkUtt <imp : Imp> ;

  numeralNP num cn = mkNP <num : Numeral> <cn : CN> ;

  time_N = mkN "tid" "tider" ;
  minute_N = mkN "minut" "minuter" ;
  morning_N = mkN "morgon" "morgnar" ; 
  noon_N = mkN "middag" ; 
  afternoon_N = mkN "eftermiddag" ; 
  when_IAdv = lin IAdv {s = "när"} ;
  yesterday_Adv = mkAdv "i går" ;
  today_Adv = mkAdv "i dag" ;
  tomorrow_Adv = mkAdv "i morgon" ;

  january_Month = mkPN "januaryi" ;
  february_Month = mkPN "februari" ;
  march_Month = mkPN "mars" ;
  april_Month = mkPN "april" ;
  may_Month = mkPN "maj" ;
  june_Month = mkPN "juni" ;
  july_Month = mkPN "juli" ;
  august_Month = mkPN "augusti" ;
  september_Month = mkPN "september" ;
  october_Month = mkPN "oktober" ;
  november_Month = mkPN "november" ;
  december_Month = mkPN "december" ;

  month_N = mkN "månad" "månader" ;
  moon_N = mkN "måne" "månar" ;
  sun_N = mkN "sol" ;
  day_N = mkN "dag" ; 
  year_N = mkN "år" ;
  week_N = mkN "vecka" ;

  monday_Day = mkPN "måndag" ;
  tuesday_Day = mkPN "tisdag" ;
  wednesday_Day = mkPN "onsdag" ;
  thursday_Day = mkPN "torsdag" ;
  friday_Day = mkPN "fredag" ;
  saturday_Day = mkPN "lördag" ;
  sunday_Day = mkPN "söndag" ;

  tea_N = mkCN (mkN "te" "teen") ; 
  chinese_N = mkN "kines" "kineser" ;
  prostitute_N = mkN "glädjeflicka" ; ---
  dish_N = mkN "maträtt" "maträtter" ;
  vegetable_N = mkN "grönsak" "grönsaker" ;
  delicious_A = mkA "läcker" ;
  want_V2 = mkV2 "önska" ; ---
  rice_N = mkCN (mkN "ris" "ris") ;
  cup_N = mkN "kopp" ;
  glass_N = mkN "glas" "glas" ;
  invite_V2 = mkV2 (mkV "bjuda" "bjöd" "bjudit") ;
  pay_V = mkV "betala" ;
  please_Interj = mkInterj "var så god" ;
  chair_N = mkN "stol" ;
  table_N = mkN "bord" "bord" ;
  gorgeous_A = mkA "praktfull" ;
  thanks_Interj = mkInterj "tack" ;
  you_are_welcome_Interj = mkInterj "det var så lite" ;
  meeting_N = mkN "möte" ;
  cook_V = mkV (mkV "laga") "mat" ;
  have_name_Cl pron pn = mkCl (mkNP pron) (mkV2 (mkV "heta" "hette" "hetat")) (mkNP pn) ; 
  have_age_Cl np nu = mkCl np (mkNP (mkDet <nu : Numeral>)) ;
  how_old_QS np = mkQS (mkQCl (E.ICompAP (mkAP (mkA "gammal"))) <np : NP>) ;
  mum_N2 = mkN2 (mkN "mamma") to_Prep ;
  dad_N2 = mkN2 (mkN "pappa") to_Prep ;
  son_N2 = mkN2 (mkN "son" "söner") to_Prep ;
  daughter_N2 = mkN2 (mkN "dotter" "döttrar") to_Prep ;
  family_N = mkN "familj" "familjer" ;
  miss_V2 = mkV2 "sakna" ;
  think_VQ = mkVQ (mkV "tänker") ;
  return_V = mkV L.come_V "tillbaka" ;
  see_V2 = L.see_V2 ;
  train_station_N = mkN "järnvägsstation" "järnvägsstationer" ;
  taxi_N = mkN "taxi" "taxibilar" ;
  see_you_tomorrow_Interj = mkInterj "vi ses i morgon" ;
  see_you_Interj = mkInterj "vi ses" ;
  movie_N = mkN "film" "filmer" ;
  computer_N = mkN "dator" "datorer" ;
  go_to_bed_V = partV L.go_V "till sängs" ;
  make_a_phone_call_V = mkV "ringer" ;
  feed_V2 = mkV2 "mata" ;
  helloPhone_Interj = mkInterj "hallå" ;
  sorry_Interj = mkInterj "ursäkta" ;
  know_V2 = mkV2 "känner" ;
  it_doesnt_matter_Interj = mkInterj "det gör ingenting" ;
  study_V = mkV "studera" ;
  study_V2 = mkV2 "studera" ;
  chinese_NP = mkNP (mkPN "kinesiska") ;
  classmate_N = mkN "klasskompis" ;
  chat_V = mkV "chatta" ;
  character_N = mkN "tecken" "tecken" ;

  oper
    mkInterj : Str -> Interj = \s -> lin Interj {s = s} ;

  }