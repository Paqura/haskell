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

message :: [FourLetterAlphabet]
message = [L1, L3, L1, L4, L2, L4]

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder message = map rotN message
  where size = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rotN = rotateN size

-- всё это работает rotateN только с четными алфавитами
-- вот для нечетных

rotateNImproved size l = toEnum rotation
  where half = size `div` 2
        offset = if even size 
                   then fromEnum l + half
                 else 1 + fromEnum l + half
        rotation = offset `mod` size   
