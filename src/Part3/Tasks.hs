module Part3.Tasks where

import Util (notImplementedYet)
import Data.List;

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq nums = fst (maximumBy compareCount digitCounts)
  where
    digitsOf 0 = []
    digitsOf x = (x `mod` 10) : digitsOf (x `div` 10)
    digitCounts = [(digit, count (concatMap digitsOf nums) digit)  | digit <- [0..9]]
    count lst x = length (filter (== x) lst)
    compareCount (_, c1) (_, c2) = compare c1 c2

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst = map (\key -> (key, filter (\x -> f x == key) lst)) (uniq $ map f lst)
