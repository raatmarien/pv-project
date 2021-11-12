{-# language FlexibleContexts #-}

module Std
  (
    module Std,
    module Relude,
    module Relude.Extra.Foldable1,
    module Control.Exception.Safe
  )
  where

import Control.Exception.Safe
import Relude hiding (intercalate, some)
import Relude.Extra.Foldable1
import Data.List.NonEmpty qualified as N

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

intercalate :: (Monoid a, Foldable t) => a -> t a -> a
intercalate a = fold . intersperse a . toList

some :: Alternative f => f a -> f (NonEmpty a)
some v = (:|) <$> v <*> many v

readFileUtf8 ::
  (ConvertUtf8 text ByteString, MonadIO m) => FilePath -> m text
readFileUtf8 =
  (=<<) (either (liftIO . throw) pure) .
  fmap decodeUtf8Strict .
  readFileBS

writeFileUtf8 ::
  (ConvertUtf8 text ByteString, MonadIO m) => FilePath -> text -> m ()
writeFileUtf8 filePath = writeFileBS filePath . encodeUtf8

sortNonEmptyOn :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortNonEmptyOn f =
  fmap snd .
  N.sortBy (comparing fst) .
  fmap (\a -> let b = f a in b `seq` (b, a))
