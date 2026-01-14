module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 4 |+|
infixl 4 |-|
infixl 6 |*|
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (BinaryTerm op lhv rhv) =
    BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)
replaceVar varName replacement (Variable varName')
    | varName' == varName = replacement
replaceVar varName replacement expression = expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm op (IntConstant a) (IntConstant b)) = IntConstant $ case op of
    Plus -> a + b
    Minus -> a - b
    Times -> a * b
evaluate (BinaryTerm op a b) = case (a', b') of
    (IntConstant _, IntConstant _) -> evaluate (BinaryTerm op a' b')
    otherswise -> BinaryTerm op a b
    where
        a' = evaluate a
        b' = evaluate b
evaluate term = term
