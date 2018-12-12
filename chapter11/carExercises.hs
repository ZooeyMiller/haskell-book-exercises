data Price =
    Price Integer deriving (Eq, Show)

data Size =
    Size Integer deriving (Eq, Show)

data Manufacturer =
    Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 1000)

isCar :: Vehicle -> Bool
isCar (Car x y) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane x y) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m _) = Just m
getManu _         = Nothing
