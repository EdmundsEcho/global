import           Control.Monad

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join.fmap f $ ma

   -- join (fmap f a)
   -- (join .) . fmap

   -- write bind using fmap and join
