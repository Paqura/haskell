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

-- type FirstName = String
-- type LastName = String
-- type MiddleName = String

-- data Name = Name FirstName LastName
--    | MiddleName FirstName MiddleName LastName

-- data Sex = Male | Female

-- sexInitial :: Sex -> Char
-- sexInitial Male = 'M'
-- sexInitial Female = 'W'

-- showName :: Name -> String
-- showName (Name f s) = f ++ " " ++  s

-- showSex :: Sex -> Char
-- showSex sex = sexInitial sex

-- data User = User Name Sex

-- createUser :: FirstName -> LastName -> Sex -> User
-- createUser fName lName sex = User (Name fName lName) sex

-- printUser :: User -> String
-- printUser (User (Name f l) sex) = f ++ ", " ++ l ++ ", sex: " ++ show (sexInitial sex)

-- user = createUser "slava" "popov" Male

-- -- printUser user // "slava, popov, sex: Male"

----------

type FirstName = String
type LastName = String

data Sex = Male | Female

initSex :: Sex -> Char
initSex Male = 'M'
initSex Female = 'W'

data Name = Name FirstName LastName
data Patient = Patient Name Sex

createName :: FirstName -> LastName -> Name

createName fName lName = Name fName lName

createPatient :: Name -> Sex -> Patient

createPatient name sex = Patient name sex

showPatientName (Patient (Name f l) _) = f ++ " " ++ l
showPatientSex (Patient _ sex) = initSex sex
showPatientInfo (Patient (Name f l) sex) = f ++ " " ++ l ++ "\n" ++ ", sex: " ++ show (initSex sex)