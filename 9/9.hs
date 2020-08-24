cup ml = \_ -> ml

coffeeCup = cup 500

getMl aCup = aCup (\ml -> ml)

drink aCup mlDrank = cup (ml - mlDrank)
  where ml = getMl aCup

afterASip = drink coffeeCup 30
-- чтобы проверить результат
-- getMl afterSip

afterManySip = foldl drink coffeeCup [30, 50, 40]
