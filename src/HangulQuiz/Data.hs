module HangulQuiz.Data (initials, medials, finals, romanize) where

initials :: [String]
initials =
  [ "g","gg","n","d","dd","r","m","b","bb","s","ss","","j","jj","ch","k","t","p","h" ]

medials :: [String]
medials =
  [ "a","ae","ya","yae","eo","e","yeo","ye","o","wa","wae","oe","yo","u","wo","we","wi","yu","eu","ui","i" ]

finals :: [String]
finals =
  [ "","k","k","ks","n","nj","nh","t","l","lk","lm","lb","ls","lt","lp","lh","m","p","ps","t","t","ng","t","t","k","t","p","t" ]

romanize :: Int -> String
romanize off =
  let (l, mf) = off `divMod` 588
      (m, f) = mf `divMod` 28
   in initials !! l ++ medials !! m ++ finals !! f
