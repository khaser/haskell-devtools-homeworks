module Part1.Tasks where

import Util(notImplementedYet)
import Data.List -- for sort

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

normalizeAngle angle = angle - 2 * pi * fromIntegral (floor (angle / (2 * pi)))

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sinT (normalizeAngle x) 30
  where
    sinT x 0 = 0
    sinT x n = sinT x (n - 1) + ((-1) ^ (n-1)) * (x ^ (2*n-1)) / fromIntegral (fact (2*n-1))

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = cosT (normalizeAngle x) 30
  where
    cosT x 0 = 0
    cosT x n = cosT x (n - 1) + ((-1) ^ (n-1)) * (x ^ (2*n-2)) / fromIntegral (fact (2*n-2))

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b
    | a == 0 = abs b
    | a < 0 = myGCD b (-a)
    | a <= b = myGCD (b `mod` a) a
    | otherwise = myGCD b a

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
    | year < 1 || month < 1 || month > 12 || day < 1 = False
    | otherwise = day <= daysInMonth month year
  where
    daysInMonth 2 year
        | isLeapYear year = 29
        | otherwise = 28
    daysInMonth m _
        | m `elem` [4,6,9,11] = 30
        | otherwise = 31
    isLeapYear y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x n
    | n == 0 = 1
    | odd n = x * myPow x (n - 1)
    | even n = let t = myPow x (n`div`2) in t * t

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n = checkDivisors 2
  where
    checkDivisors d
        | d * d > n = True
        | n `mod` d == 0 = False
        | otherwise = checkDivisors (d + 1)


type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = abs s / 2
  where
    points' = points ++ [head points]
    s = sum [x * y' - x' * y | ((x, y), (x', y')) <- zip points (tail points')]

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | not (x > 0 && x + y > z) = -1
    | abs (x^2 + y^2 - z^2) < epsilon = 2
    | x^2 + y^2 < z^2 - epsilon = 0
    | otherwise = 1
  where
    [x, y, z] = sort [a, b, c]
    epsilon = 1e-10
