concrete HSKGrammarChi of HSKGrammar = 

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

  warm_A = mkA "热" ;
  cold_A = mkA "冷" ;
  live_V = mkV "住" ; -- reside
  in_Prep = mkPrep "在" ; -- in a city

  beijing_Place = mkPN "北京" ; 
  china_Place = mkPN "中国" ;

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
  MisterPN pn = lin NP {s = pn.s ++ R.word "先生"} ;
  qian_PersonName = mkPN "钱" ;
  namePerson n = mkNP n ;
  personNP p = p ;
  hello_Interj = mkInterj "你好" ;
  uttPhr utt = mkPhr <utt : Utt> ;

  impV v = mkImp v ;
  impV2 v np = mkImp v np ;
  phrVocImp imp p = mkPhr (mkUtt <imp : Imp>) (mkVoc p) ;
  phrVocInterj int p = mkPhr (G.UttInterj (lin Interj int)) (mkVoc p) ;
  uttImp imp = mkUtt <imp : Imp> ;

  numeralNP num cn = mkNP <num : Numeral> <cn : CN> ;

  time_N = mkN "时候" ;
  minute_N = mkN "分钟" [] ;
  morning_N = mkN "上午" ; ---- classifier
  noon_N = mkN "中午" ; ---- classifier
  afternoon_N = mkN "下午" ; ---- classifier
  when_IAdv = lin IAdv {s = R.word "么时候来"} ;
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

  tea_N = mkCN (mkN "茶" "杯子") ; 
  chinese_N = mkN "中国人" ;
  prostitute_N = mkN "小姐" ;
  dish_N = mkN "菜" ;
  vegetable_N = mkN "菜" ;
  delicious_A = mkA "好吃" ;
  want_V2 = mkV2 "要" ;
  rice_N = mkCN (mkN "米饭") ;
  cup_N = mkN "杯子" [] ;
  glass_N = mkN "杯子" [] ;
  invite_V2 = mkV2 "请" ;
  pay_V = mkV "来" ;
  please_Interj = mkInterj "请" ;
  chair_N = mkN "椅子" ;
  table_N = mkN "桌子" ;
  gorgeous_A = mkA "漂亮" ;
  thanks_Interj = mkInterj "谢谢" | mkInterj "谢谢你" ;
  you_are_welcome_Interj = mkInterj "不客气" ;
  meeting_N = mkN "会" ;
  cook_V = mkV "做饭" ;
  have_name_Cl np pn = R.mkClause np.s (mkV "叫") pn.s ;
  have_age_Cl np nu = R.mkClause (np.s ++ nu.s) (mkV "岁") ;
  how_old_QS np = {s = np.s ++ (R.word "几岁" | R.word "多大")} ; ---
  mum_N2 = mkN2 "妈妈" ;
  dad_N2 = mkN2 "爸爸" ;
  son_N2 = mkN2 "儿子" ;
  daughter_N2 = mkN2 "女儿" ;
  family_N = mkN "家" [] ;
  miss_V2 = mkV2 "想" ;
  think_VQ = mkVQ "想" ;
  return_V = mkV "回" ;
  see_V2 = mkV2 "看见" ;
  train_station_N = mkN "火车站" ;
  taxi_N = mkN "出租车" ;
  see_you_tomorrow_Interj = mkInterj "明天见" ;
  see_you_Interj = mkInterj "再见" ;
  movie_N = mkN "电影" ;
  computer_N = mkN "电脑" ;
  go_to_bed_V = mkV "睡觉" ;
  make_a_phone_call_V = mkV "打电" ;
  feed_V2 = mkV2 "喂" ;
  helloPhone_Interj = mkInterj "喂" ;
  sorry_Interj = mkInterj "对不起" ;
  know_V2 = mkV2 "认识" ;
  it_doesnt_matter_Interj = mkInterj "没关系" ;
  study_V = mkV "读" ;
  study_V2 = mkV2 "学习" ;
  chinese_NP = mkpNP "汉语" ;
  classmate_N = mkN "同学" ;
  chat_V = mkV "说话" ;
  character_N = mkN "字" ;

  }