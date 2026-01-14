{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                          sparseMatrixWidth :: Int,
                          sparseMatrixHeight :: Int,
                          sparseMatrixElements :: Map (Int, Int) a
                      } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    width :: mx -> Int
    height :: mx -> Int
    getElem :: mx -> Int -> Int -> Int
    eyeM :: Int -> mx
    zeroM :: Int -> Int -> mx
    arbitaryM :: Int -> Int -> (Int -> Int -> Int) -> mx
    subMatrix :: mx  -> Int -> Int -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    width  _ = 1
    height _ = 1
    getElem x _ _ = x
    eyeM _               = 1
    zeroM _ _            = 0
    arbitaryM 1 1 filler = filler 0 0
    subMatrix m 0 0 = m

instance Matrix [[Int]] where
    width [] = 0
    width (r : _) = length r
    height = length
    getElem lst row col = (lst !! row) !! col
    eyeM n = [[if i == j then 1 else 0 | j <- [0..n-1]] | i <- [0..n-1]]
    zeroM w h = replicate h (replicate w 0)
    arbitaryM w h filler = [[filler col row | col <- [0..w-1]] | row <- [0..h-1]]
    subMatrix m skipRow skipCol = [[getElem m r c
                                        | c <- [0..width m - 1], c /= skipCol]
                                        | r <- [0..height m - 1], r /= skipRow]

instance Matrix (SparseMatrix Int) where
    width  = sparseMatrixWidth
    height = sparseMatrixHeight
    getElem m row col = findWithDefault 0 (row, col) (sparseMatrixElements m)
    eyeM n = SparseMatrix n n $ fromList [((i,i), 1) | i <- [0..n-1]]
    zeroM w h = SparseMatrix w h mempty
    arbitaryM w h filler = SparseMatrix w h (fromList values)
      where
        coords = [(row, col) | row <- [0..h-1], col <- [0..w-1]]
        values = [((row, col), v) | (row, col) <- coords, let v = filler col row, v /= 0]

    subMatrix m skipRow skipCol = SparseMatrix (width m - 1) (height m - 1) (fromList lst)
      where
        lst = [((if r > skipRow then r - 1 else r, if c > skipCol then c - 1 else c), v)
                | ((r, c), v) <- toList (sparseMatrixElements m), r /= skipRow, c /= skipCol]

fromList2D :: [[Int]] -> SparseMatrix Int
fromList2D lst = SparseMatrix w h $ fromList [((r, c), lst !! r !! c) | c <- [0..w - 1], r <- [0..h - 1]]
    where
        w = width lst
        h = height lst

createMatrix :: Matrix m => [[Int]] -> m
createMatrix []  = zeroM 0 0
createMatrix lst = arbitaryM (length (head lst)) (length lst) (\col row -> lst !! row !! col)

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye = eyeM

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = zeroM

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix l r = arbitaryM (width r) (height l) getEl
  where
    getEl x y = sum [getElem l y k * getElem r k x | k <- [0 .. width l - 1]]

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m
  | width m == 0 = 1
  | width m == 1 = getElem m 0 0
  | otherwise = sum [(-1) ^ col * getElem m 0 col * determinant (subMatrix m 0 col)
                      | col <- [0..width m - 1]]
