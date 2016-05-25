
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT ma) =  ReaderT $
                        (fmap.fmap) f ma

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ pure $ pure a
  (<*>) (ReaderT f) (ReaderT v) =  ReaderT $
                                  ((<*>) <$> f) <*> v
                                  -- runReaderT will give you a fn
                                  -- to which takes r

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r -> do
                                a <- rma r
                                runReaderT (f a) r

-- mr = ReaderT (\r -> [3+r,7+r])
