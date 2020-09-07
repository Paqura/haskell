xorBool :: Bool -> Bool -> Bool
xorBool val1 val2 = (val1 || val2) && (not (val1 && val2))

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
-- zip делает из 2-ух массивов пары
-- zip [2, 5][7,5] -> [(2, 7), (5, 6)]
-- если неравное кол-во элем-ов они игнорируются
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

-- вспомогательная функций, поэтому с '
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]

intToBits' n = if (remainder == 0)
              then False : intToBits' nextVal
              else True : intToBits' nextVal
      where remainder = n `mod` 2
            nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

-- основная функий, которая переворачивает массив и задает длину
intToBits n = leadingFalses ++ reversedBits
  where reversedBits = reverse (intToBits' n)
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)

  where size = length bits
        indices = [size-1, size-2 .. 0]
        trueLocations = filter (\x -> fst x == True) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText =
  map (\pair -> (fst pair) `xor` (snd pair)) (zip padBits plainTextBits)
    where padBits = map charToBits pad
          plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bitList
  where bitList = applyOTP' pad plainText


myPad :: String
myPad = "Shasgaslksk"

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

