import           Control.Monad

data PhhhbbtttEither b a = PLeft a | PRight b

instance Functor ( PhhhbbtttEither b ) where
  fmap = liftM

instance Applicative (PhhhbbtttEither b) where
  pure = return
  (<*>) = ap

instance Monad (PhhhbbtttEither b) where
  return a = PLeft a
  (>>=) ma f = case ma of
    PRight a -> PRight a
    PLeft a  -> f a
