module QC where

import Test.QuickCheck

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)
  -- pattern for Arbitrary

genTuple' :: Gen a -> Gen b -> Gen (a,b)
genTuple' genA genB = do
  a <- genA
  b <- genB
  return (a,b)

data Trivial =
  Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

prop_additionGreater :: Int -> Bool
prop_additionGreater y = y + 1 > y

half :: Fractional a => a -> a
half y = y / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . halfIdentity

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity d = halfIdentity d == d

runQC :: IO ()
runQC = quickCheck (prop_halfIdentity :: Double -> Bool)

main = putStrLn "HI"
