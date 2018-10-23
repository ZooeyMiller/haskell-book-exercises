newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)


data Person = Person {
    humanName :: HumanName
    , dogName :: DogName
    , address :: Address
} deriving (Eq, Show)

data Dog = Dog {
    dogsName      :: DogName
    , dogsAddress :: Address
} deriving (Eq, Show)

zooey :: Person
zooey = Person
        (HumanName "Zooey")
        (DogName "Murdock")
        (Address "Secret")

other :: Person
other = Person
        (HumanName "Jack")
        (DogName "Spot")
        (Address "France")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)


getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address
