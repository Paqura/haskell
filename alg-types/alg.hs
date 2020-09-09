type FirstName = String
type LastName = String

data Name = Name FirstName LastName

data Author = Author Name
data Artist = Person Name | Band String

data Creator = AuthorCreate Author | ArtistCreate Artist

data Book = Book {
    author :: Creator
  , isbn :: String
  , bookTitle :: String
  , year :: Int
  , bookPrice :: Double
}

data VinylRecord = VinylRecord {
    artist :: Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
    name :: String
  , description :: String
  , toyPrice :: Double
}

data Pamphlet = Pamphlet {
    pamphletName :: String
  , descr :: String
  , info :: String
}

data StoreItem =
  BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | PamphletItem Pamphlet

price :: StoreItem -> Double

price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy

getInfo :: StoreItem -> String
getInfo (PamphletItem pamphlet) = info pamphlet

-- пример использования

bookAuthor :: Creator
bookAuthor = AuthorCreate (Author (Name "Slava" "Avals"))

book :: StoreItem
book = BookItem (Book bookAuthor "12312" "book title" 2020 1200)

-- price book -> 1200.0