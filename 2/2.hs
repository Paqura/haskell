checkOnEven n =
  if even n then evenN
  else nonEvenN

  where evenN = n - 2
        nonEvenN = 3 * n + 1