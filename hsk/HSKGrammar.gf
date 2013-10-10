abstract HSKGrammar = 

  Grammar [

   Phr, Utt, NP, VP, Cl, Adv, V2, AP, A, AdA, Det, S, QS, QCl, IP, IDet, IQuant, N2,
   ClSlash, VPSlash, Pron, Pol, Temp, Tense, Ant, RS, RCl, RP, CN, Comp, VV, N, V, PN,
   Predet, Prep, Quant, IQuant, Num, Interj, Numeral, VQ, IComp,
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
   here_Adv,
   there_Adv,
   where_IAdv,
   in_Prep,
   want_VV,
   can8know_VV,
   can_VV
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
   now_Adv,
   eat_V2,
   drink_V2,
  sit_V, 
  dog_N,
  cat_N,
  do_V2,
  water_N,
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

     ** {

flags startcat = Phr ;

cat
  Month ;
  Day ;

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

  time_N : N ; -- 时候
  minute_N : N ; -- 分钟 []
  morning_N : N ; -- 上午
  noon_N : N ; -- 中午
  afternoon_N : N ; -- 下午
  when_IAdv : IAdv ; -- 么时候来
  yesterday_Adv : Adv ; -- 昨天
  today_Adv : Adv ; -- 今天
  tomorrow_Adv : Adv ; -- 明天

  january_Month : Month ; -- 一月
  february_Month : Month ;  -- 二月
  march_Month : Month ; -- 三月
  april_Month : Month ; -- 四月
  may_Month : Month ; -- 五月
  june_Month : Month ; -- 六月
  july_Month : Month ; -- 七月
  august_Month : Month ; -- 八月
  september_Month : Month ; -- 九月
  october_Month : Month ; -- 十月
  november_Month : Month ; -- 十一月
  december_Month : Month ; -- 十二月

  month_N : N ; -- 月 
  moon_N : N ; -- 月 
  sun_N : N ; -- 日
  day_N : N ; -- 日  天
  year_N : N ; -- 年 []
  week_N : N ; -- 星期

  monday_Day : Day ; -- 星期一
  tuesday_Day : Day ; -- 星期二
  wednesday_Day : Day ; -- 星期三
  thursday_Day : Day ; -- 星期四
  friday_Day : Day ; -- 星期五
  saturday_Day : Day ; -- 星期六
  sunday_Day : Day ; -- 星期日 | 星期天

  monthPN : Month -> PN ;
  dayPN : Day -> PN ;

  tea_N : N ; -- 茶 -- 杯子 
  chinese_N : N ; -- 中国人
  prostitute_N : N ; -- 小姐
  dish_N : N ; -- 菜 
  vegetable_N : N ; -- 菜
  delicious_A : A ; -- 好吃
  want_V2 : V2 ; -- 要 (yào)
  rice_N : N ; -- 米饭 (mǐ fàn) 
  cup_N : N ; -- 杯子 []
  glass_N : N ; -- 杯子 []
  invite_V2 : V2 ; -- 请
  pay_V : V ; -- 来
  please_Interj : Interj ; -- 请
  chair_N : N ; -- 椅子
  table_N : N ; -- 桌子
  gorgeous_A : A ; -- 漂亮
  thanks_Interj : Interj ; -- 谢谢 | 谢谢你
  you_are_welcome_Interj : Interj ; -- 不客气
  meeting_N : N ; -- 会
  cook_V : V ; -- 做饭
  have_name_Cl : Pron -> PN -> Cl ; -- 叫
  have_age_Cl : NP -> Numeral -> Cl ; -- 我24岁
  how_old_QS : NP -> QS ; -- 你几岁 | 你多大?
  mum_N2 : N2 ; -- 妈妈 []
  dad_N2 : N2 ; -- 爸爸 []
  son_N2 : N2 ; -- 儿子 []
  daughter_N2 : N2 ; -- 女儿
  family_N : N ; -- 家 []
  miss_V2 : V2 ; -- 想
  think_VQ : VQ ; -- 想
  return_V : V ; -- 回
  see_V2 : V2 ; -- 看见
  train_station_N : N ; -- 火车站
  taxi_N : N ; -- 出租车
  see_you_tomorrow_Interj : Interj ; -- 明天见
  see_you_Interj : Interj ; -- 再见
  movie_N : N ; -- 电影
  computer_N : N ; -- 电脑
  go_to_bed_V : V ; -- 睡觉
  make_a_phone_call_V : V ; -- 打电
  feed_V2 : V2 ; -- 喂
  helloPhone_Interj : Interj ; -- 喂
  sorry_Interj : Interj ; -- 对不起
  know_V2 : V2 ; -- 认识
  it_doesnt_matter_Interj : Interj ; -- 没关系
  study_V : V ; -- 读
  study_V2 : V2 ; -- 学习
  chinese_NP : NP ; -- 汉语
  classmate_N : N ; -- 同学
  chat_V : V ; -- 说话
  character_N : N ; -- 字

  }