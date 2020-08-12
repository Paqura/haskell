-- возводим во 2 степерь аргументы, умноженные на 2
squareDoubleWithLambda x y = (\doubleArgs -> doubleArgs^2)
  ((x * 2) + (y * 2))

squereDoubleWithLet x y = let doubleArgs = ((x * 2) + (y * 2))
  in
    if doubleArgs > 10
      then doubleArgs
    else 0