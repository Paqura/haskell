data Square = Square {
    sideWidth :: Int
}

data Rect = Rect {
    width :: Int
  , height :: Int
}

data Shape = SquareCreator Square
  | RectCreator Rect

getPerimeter :: Shape -> Int

getPerimeter (SquareCreator square) = sideWidth square * 4
getPerimeter (RectCreator rect) = 2*(width rect) + 2*(height rect)