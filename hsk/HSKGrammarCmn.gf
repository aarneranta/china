--# -path=src/chinese/pinyin:src/chinese:src/api:.:alltenses

concrete HSKGrammarCmn of HSKGrammar = 

  GrammarChi [

   Phr, Utt, NP, VP, Cl, Adv, V2, AP, A, AdA, Det, S, QS, QCl, IP, IDet, IQuant,N2,
   ClSlash, VPSlash, Pron, Pol, Temp, Tense, Ant, RS, RCl, RP, CN, Comp, VV, N, PN,
   Predet, Prep, Quant, IQuant, Num, Interj, Numeral, VQ, IComp, Imp,
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
   UttInterj,  -- Interj -> Utt
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
--   in_Prep,
   want_VV,
   can8know_VV,
   can_VV
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

     ** open SyntaxChi, ParadigmsChi, (E = ExtraChi), (R = ResChi), (G = GrammarChi), (L = LexiconChi)
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
  predDetNP det np = mkCl (lin NP {s = det.s}) (lin NP np) ;
  predAP np ap = E.PredBareAP np ap ;
  leS cl = lin S {s = (mkS cl).s ++ "le"} ;
  neQS cl = lin QS {s = (mkS cl).s ++ "ne"} ;
  buQS cl = mkQS (E.QuestRepV cl) ;
  heNP x y = lin NP {s = x.s ++ "he2" ++ y.s} ; 
  theNP cn = mkNP the_Det (lin CN cn) ;

  ji_IDet = how8many_IDet ;
  ji_Det = mkpDet "ji1" ;
  how_IAdv = how_IAdv ;
  how_about_IAdv = how_IAdv ;
  hen_AdA = very_AdA ;
  dou_Predet = all_Predet ;
  dou_NP = mkpNP "dou1" ;
  a_few_Det = mkpDet "xie1" ;
  verygood_AdA = lin AdA {s = "hao3"} ;
  extremely_AdA = lin mkAdA {s = "tai4"} ;

  money_N = mkN "qian2" ;
  person_N = mkN "ren2" ;
  like_VV = mkVV "xi3huan1" ;
  happy_A = mkA "gao1xing1" ;
  great_Interj = lin Interj {s = R.word "tai4hao3le"} ;
  clothes_N = mkN "yi1fu2" ; ----

  the_weather_NP = mkpNP "tian1qi4" ;
  here_in_Adv = mkAdv "zhe4er2" ;
  there_in_Adv = mkAdv "na3er2" ;

  here_Adv = G.here_Adv ;
  there_Adv = G.there_Adv ;
  now_Adv = L.now_Adv ;
  advPlace prep place = SyntaxChi.mkAdv prep place ;
  inMonth m = SyntaxChi.mkAdv (mkPrep [] [] timeAdvType) m  ;
  onDay d = SyntaxChi.mkAdv (mkPrep [] [] timeAdvType) d ;
  placeNPAdv prep np = SyntaxChi.mkAdv prep np ;
  placeAdv a = a ;
  timeAdv a = lin Adv {s = a.s ; advType = timeAdvType} ;
  compPlaceAdv a = mkComp a ;

  warm_A = mkA "re4" ;
  cold_A = mkA "leng3" ;
  live_V = mkV "zhu4" ; -- reside
  in_Prep = mkPrep "zai4" ; -- in a city

  beijing_Place = mkPN "bei3jing1" ; 
  china_Place = mkPN "zhong1guo2" ;

--- get 我在中国住, expected 我住在中国

  go_V2 = mkV2 "qu4" ;
  up_Adv = mkAdv "shang4" ;
  down_Adv = mkAdv "xia4" ;

  job_N = mkN "gong1zuo4" ;
  work_V = mkV "gong1zuo4" ;
  doctor_N = mkN "yi1sheng1" ;
  hospital_N = mkN "yi1yuan4" ;
  restaurant_N = mkN "fan4guan3" ;
  shop_N = mkN "shang1dian4" ;
  at_Prep = mkPrep "zai4" ;
  in_front_of = mkPrep "zai4" "deqian2mian4" ;
  behind_Prep = mkPrep "zai4" "dehou4mian4" ;
  ExistInCl adv cn = E.TopicAdvCl adv (mkCl (lin CN cn)) ;
  MisterPN pn = lin NP {s = pn.s ++ R.word "xian1sheng1"} ;
  qian_PersonName = mkPN "qian2" ;
  namePerson n = mkNP n ;
  personNP p = p ;
  hello_Interj = mkInterj "ni3hao3" ;
  uttPhr utt = mkPhr <utt : Utt> ;

  impV v = mkImp v ;
  impV2 v np = mkImp v np ;
  phrVocImp imp p = mkPhr (mkUtt <imp : Imp>) (mkVoc p) ;
  phrVocInterj int p = mkPhr (G.UttInterj (lin Interj int)) (mkVoc p) ;
  uttImp imp = mkUtt <imp : Imp> ;

  numeralNP num cn = mkNP <num : Numeral> <cn : CN> ;

  time_N = mkN "shi2hou4" ;
  minute_N = mkN "fen1zhong1" [] ;
  morning_N = mkN "shang4wu3" ; ---- classifier
  noon_N = mkN "zhong1wu3" ; ---- classifier
  afternoon_N = mkN "xia4wu3" ; ---- classifier
  when_IAdv = lin IAdv {s = R.word "mashi2hou4lai2"} ;
  yesterday_Adv = mkAdv "zuo2tian1" ;
  today_Adv = mkAdv "jin1tian1" ;
  tomorrow_Adv = mkAdv "ming2tian1" ;

  january_Month = mkPN "yi1yue4" ;
  february_Month = mkPN "er4yue4" ;
  march_Month = mkPN "san1yue4" ;
  april_Month = mkPN "si4yue4" ;
  may_Month = mkPN "wu3yue4" ;
  june_Month = mkPN "liu4yue4" ;
  july_Month = mkPN "qi1yue4" ;
  august_Month = mkPN "ba1yue4" ;
  september_Month = mkPN "jiu3yue4" ;
  october_Month = mkPN "shi2yue4" ;
  november_Month = mkPN "shi2yi1yue4" ;
  december_Month = mkPN "shi2er4yue4" ;

  month_N = mkN "yue4" ;
  moon_N = mkN "yue4" ;
  sun_N = mkN "ri4" ;
  day_N = mkN "ri4" "tian1" ; ---
  year_N = mkN "nian2" [] ;
  week_N = mkN "xing1qi1" ;

  monday_Day = mkPN "xing1qi1yi1" ;
  tuesday_Day = mkPN "xing1qi1er4" ;
  wednesday_Day = mkPN "xing1qi1san1" ;
  thursday_Day = mkPN "xing1qi1si4" ;
  friday_Day = mkPN "xing1qi1wu3" ;
  saturday_Day = mkPN "xing1qi1liu4" ;
  sunday_Day = mkPN "xing1qi1ri4" | mkPN "xing1qi1tian1" ;

  monthPN d = d ;
  dayPN d = d ;

  tea_N = mkCN (mkN "cha2" "bei1zi3") ; 
  chinese_N = mkN "zhong1guo2ren2" ;
  prostitute_N = mkN "xiao3jie3" ;
  dish_N = mkN "cai4" ;
  vegetable_N = mkN "cai4" ;
  delicious_A = mkA "hao3chi1" ;
  want_V2 = mkV2 "yao1" ;
  rice_N = mkCN (mkN "mi3fan4") ;
  cup_N = mkN "bei1zi3" [] ;
  glass_N = mkN "bei1zi3" [] ;
  invite_V2 = mkV2 "qing3" ;
  pay_V = mkV "lai2" ;
  please_Interj = mkInterj "qing3" ;
  chair_N = mkN "yi3zi3" ;
  table_N = mkN "zhuo1zi3" ;
  gorgeous_A = mkA "piao1liang4" ;
  thanks_Interj = mkInterj "xie4xie4" | mkInterj "xie4xie4ni3" ;
  you_are_welcome_Interj = mkInterj "bu4ke4qi4" ;
  meeting_N = mkN "hui4" ;
  cook_V = mkV "zuo4fan4" ;
  have_name_Cl np pn = R.mkClause np.s (mkV "jiao4") pn.s ;
  have_age_Cl np nu = R.mkClause (np.s ++ nu.s) (mkV "sui4") ;
  how_old_QS np = {s = np.s ++ (R.word "ji1sui4" | R.word "duo1da4")} ; ---
  mum_N2 = mkN2 "ma1ma1" ;
  dad_N2 = mkN2 "ba4ba4" ;
  son_N2 = mkN2 "er2zi3" ;
  daughter_N2 = mkN2 "nu:3er2" ;
  family_N = mkN "jia1" [] ;
  miss_V2 = mkV2 "xiang3" ;
  think_VQ = mkVQ "xiang3" ;
  return_V = mkV "hui2" ;
  see_V2 = mkV2 "kan1jian4" ;
  train_station_N = mkN "huo3che1zhan4" ;
  taxi_N = mkN "chu1zu1che1" ;
  see_you_tomorrow_Interj = mkInterj "ming2tian1jian4" ;
  see_you_Interj = mkInterj "zai4jian4" ;
  movie_N = mkN "dian4ying3" ;
  computer_N = mkN "dian4nao3" ;
  go_to_bed_V = mkV "shui4jiao4" ;
  make_a_phone_call_V = mkV "da2dian4" ;
  feed_V2 = mkV2 "wei4" ;
  helloPhone_Interj = mkInterj "wei4" ;
  sorry_Interj = mkInterj "dui4bu4qi3" ;
  know_V2 = mkV2 "ren4shi2" ;
  it_doesnt_matter_Interj = mkInterj "mei2guan1xi4" ;
  study_V = mkV "dou4" ;
  study_V2 = mkV2 "xue2xi2" ;
  chinese_NP = mkpNP "han4yu3" ;
  classmate_N = mkN "tong2xue2" ;
  chat_V = mkV "shui4hua4" ;
  character_N = mkN "zi4" ;

  }