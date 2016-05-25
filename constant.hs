{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Constant a b = Constant { getConstant :: a }
                     deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance forall a . Monoid a => Applicative (Constant a) where
  pure :: b -> Constant a b
  pure _ = Constant mempty -- pure is trying to give us b
                           -- a -> f a ... is the last type b
  (<*>) :: Constant a (x -> b)
        -> Constant a x
        -> Constant a b
  (<*>) (Constant a) (Constant a')= Constant (a `mappend`a')

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return (Constant a)

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

main :: IO ()
main = do
  let c :: Constant String (String, String, String)
      c = Constant "blah"
  quickBatch (applicative c)
