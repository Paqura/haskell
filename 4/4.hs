import Data.List

astLocation = "Astrahan 414015"
mskLocation = "Moscow 35343"

getDefaultOfficeResponse name = fst name ++ " " ++ snd name
fullName name = fst name ++ " " ++ snd name

astOffice name = fullName name ++ " - " ++ astLocation
mskOffice name = fullName name ++ " - " ++ mskLocation

addressLetter name location =
  case location of
    "ast" -> astOffice name
    "msk" -> mskOffice name
    _ -> getDefaultOfficeResponse name



