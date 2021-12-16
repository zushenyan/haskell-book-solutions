module Ch11.Vehicles where

data Size = Size Integer deriving (Show, Eq)

data Price = Price Integer deriving (Show, Eq)

data Manufacturer = Mini | Mazda | Tata deriving (Show, Eq)

data Airline = PapuAir | CatapultsR'Us | TakeYourChanceUnited deriving (Show, Eq)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Show, Eq)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir (Size 123)

-- 1.
-- ans: Vehicle

-- 2.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3.
getManu :: Vehicle -> Manufacturer
getManu (Car a _) = a

-- 4.
-- ans: runtime exception

-- 5.
-- ans: implementation shown as above