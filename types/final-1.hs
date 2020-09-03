data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotateN :: (Bounded a, Enum a) => Int -> a -> a
rotateN alphabetSize letter = toEnum rotation
  where half = alphabetSize `div` 2
        offset = fromEnum letter + half
        rotation = offset `mod` alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

smallestCharNumber :: Int
smallestCharNumber = fromEnum (minBound :: Char)

rotateEnChar :: Char -> Char
rotateEnChar charToEncrypt = rotateN size charToEncrypt
  where size = 1 + largestCharNumber
