module Std (module Std, module Relude, module Relude.Extra.Foldable1) where

import Relude hiding (intercalate)
import Relude.Extra.Foldable1

(!=) :: (Eq a) => a -> a -> Bool
(!=) = (/=)

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

data Foldr1Semigroup a = Foldr1Semigroup (a -> a -> a) a
unFoldr1Semigroup :: Foldr1Semigroup a -> a
unFoldr1Semigroup (Foldr1Semigroup _ a) = a

instance Semigroup (Foldr1Semigroup a) where
  Foldr1Semigroup _ a0 <> Foldr1Semigroup f a1 =
    Foldr1Semigroup f (f a0 a1)

foldr1 :: (Foldable1 f) => (a -> a -> a) -> f a -> a
foldr1 f = unFoldr1Semigroup . foldMap1 (Foldr1Semigroup f)

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = uncurry f =<< liftA2 (,) a b

fromTo :: (Enum a) => a -> a -> [a]
fromTo lower upper = enumFromTo lower (pred upper)

intercalate :: (Monoid a) => a -> [a] -> a
intercalate = fold .: intersperse
