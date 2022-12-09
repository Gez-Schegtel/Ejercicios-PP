
davc :: Double -> Double -> Double -> Double
davc x y z = (x + y + z) / 3

iavuc :: (Integer, Integer, Integer) -> Integer 
iavuc (x, y, z) = div (x + y + z) 3

pavc :: Fractional x => x -> x -> x -> x
pavc a b c = (a + b + c) / 3

xor :: Bool -> Bool -> Bool 
xor x y = (not x && y) || (x && not y)

par :: Integral x => x -> Bool
par a = mod a 2 == 0

sayhi :: [Char]
sayhi = "Hi, how are you doing?"

-- iszero :: (Eq a, Num a) => a -> Bool
-- iszero x = if x == 0 then True else False

-- iszero :: (Eq x, Num x) => x -> Bool
-- iszero 0 = True 
-- iszero _ = False 

iszero :: (Eq a, Num a) => a -> Bool
iszero x = x == 0

firsttuple :: (x, y) -> x
firsttuple (a , _) = a

-- emptylist :: Eq a => [a] -> Bool
-- emptylist x = x == []

emptylist :: [x] -> Bool 
emptylist [] = True 
emptylist _ = False

-- Definir una función que devuelva el factorial de un número. --

-- factorial :: Integer -> Integer  (Definición alternativa de la función.)
-- factorial :: (Eq x, Num x) => x -> x 
-- factorial a = if a == 0 then 1 else a * factorial (a - 1)

-- factorial :: Integer -> Integer  (Definición alternativa de la función.)
factorial :: (Eq x, Num x) => x -> x 
factorial 0 = 1
factorial a = a * factorial (a - 1)

deslist :: [a] -> String
deslist xs = "La lista es " ++ case xs of
            [] -> "una lista vacía."
            [x] -> "una lista unitaria."
            xs -> "una lista de varios elementos."

ftuple :: (a, b) -> a
ftuple (x, _) = x

stuple :: (a, b) -> b
stuple (_, y) = y 

ttuple :: (a, b, c) -> c
ttuple (_, _, z) = z

sumatuple :: Num a => (a, a) -> a
sumatuple (a, b) = a + b 

-- Sólo se pueden añadir elementos a una lista desde el comienzo de la misma. --
howlistworks :: [Integer] 
howlistworks = 1:(2:(3:[]))

-- prodadd :: (Eq t, Num t, Num p) => p -> t -> p
-- prodadd x 0 = 0
-- prodadd x y = x + prodadd x (y - 1)

prodadd :: Integer -> Integer -> Integer
prodadd x y
    | y == 0 = 0
    | otherwise = x + prodadd x (y - 1)

divsub :: Integer -> Integer -> Integer
divsub x y
    | x < y = 0
    | otherwise = 1 + divsub (x - y) y

joyDivision :: Integral a => a -> Int
joyDivision n = length [x | x <- [1..n], n `mod` x == 0]



twiceFive :: (Eq p, Num p) => p -> p
twiceFive x = if x == 5 then x * 2 else x

listHead :: [p] -> p
listHead [] = error "La lista está vacía."
listHead (x: xs) = x

listTail :: [p] -> p
listTail [] = error "La lista está vacía."
listTail [x] = x
listTail (x: xs) = listTail xs

myDrop :: (Eq t, Num t) => t -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop y (x: xs) = myDrop (y-1) xs

-- Opción 1 --
-- takeFive :: (Eq a, Num p, Num a) => [a] -> p
-- takeFive [] = 0
-- takeFive (x : xs) = if x == 5 then 1 + takeFive xs else takeFive xs

-- Opción 2 --
-- takeFive :: (Eq a, Num p, Num a) => [a] -> p
-- takeFive [] = 0
-- takeFive (5: xs) = 1 + takeFive xs
-- takeFive (_: xs) = takeFive xs

-- Opción 3 --
takeFive :: (Eq a, Num p, Num a) => [a] -> p
takeFive [] = 0
takeFive (x: xs) | x == 5 = 1 + takeFive xs | otherwise = takeFive xs

