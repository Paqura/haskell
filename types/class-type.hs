
-- data Sex = Male | Female

-- пишем собственную реализацию вывода (show)
-- instance Show Sex where
--  show Male = "Man"
--  show Female = "Woman"

-- Пример с кубиком
-- data SixSidesCube = S1 | S2 | S3 | S4 | S5 | S6

-- instance Show SixSidesCube where
--   show S1 = "1"
--   show S2 = "2"
--   show S3 = "3"
--   show S4 = "4"
--   show S5 = "5"
--   show S6 = "6"

-- -- хаскель автоматически поймёт что делать для (/=)
-- instance Eq SixSidesCube where
--   (==) S1 S1 = True
--   (==) S2 S2 = True
--   (==) S3 S3 = True
--   (==) S4 S4 = True
--   (==) S5 S5 = True
--   (==) S6 S6 = True
--   (==) _ _   = False

-- Пример с кубиком версия 2

-- здесь мы доверяем хаскелю создать для нас инстансы,
-- но например метод show лучше реализовать здесь самому
data SixSidesCube = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show, Eq, Ord)

