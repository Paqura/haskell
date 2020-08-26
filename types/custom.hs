{- HLINT ignore -}

-- type PatientName = (String, String)

-- firstName :: PatientName -> String
-- firstName patient = fst patient

-- lastName :: PatientName -> String
-- lastName patient = snd patient

----------

-- type FirstName = String
-- type LastName = String
-- type MiddleName = String

-- data Name = Name FirstName LastName
--   | MiddleName FirstName MiddleName LastName

-- showName :: Name -> String
-- showName (Name f l) = f ++ " " ++ l

-- shownName (Name "Vasya" "Ivanov") -- "Vasya Ivanov"

----------

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
   | MiddleName FirstName MiddleName LastName

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'W'

data Patient = Patient Name Sex

createPacient :: Patient -> Patient
createPacient patient = patient

