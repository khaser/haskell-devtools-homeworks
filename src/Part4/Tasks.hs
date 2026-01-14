module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl (:<) REmpty

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show = show . rlistToList

instance Eq a => Eq (ReverseList a) where
    (==) a b = rlistToList a == rlistToList b
    -- (/=) = notImplementedYet

instance Semigroup (ReverseList a) where
    (<>) xs REmpty = xs
    (<>) xs (ys :< y) = (xs <> ys) :< y

instance Monoid (ReverseList a) where
    mempty = REmpty
    mappend = (<>)

instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (xs :< x) = fmap f xs :< f x

instance Applicative ReverseList where
    pure = (:<) REmpty
    (<*>) REmpty _ = REmpty
    (<*>) _ REmpty = REmpty
    (<*>) (fs :< f) xs = (fs <*> xs) <> fmap f xs

instance Monad ReverseList where
    return = pure
    REmpty >>= _ = REmpty
    (>>=) (xs :< x) f = (xs >>= f) <> f x

