{- HLINT ignore-}

assignToGroups n aList = zip groups aList
  where groups = cycle [1..n]

isFirstHalf el aList = if el `elem` firstHalf
  then True
  else False

  where
        middleIdx = length aList `div` 2
        firstHalf = take middleIdx aList
