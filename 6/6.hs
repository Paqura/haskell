{- HLINT ignore-}

assignToGroups n aList = zip groups aList
  where groups = cycle [1..n]
