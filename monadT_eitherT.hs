
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $
                        (fmap.fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ pure $ pure a
  (<*>) (EitherT f) (EitherT v) = EitherT $
                                  ((<*>) <$> f) <*> v

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f = EitherT $ do
    v <- mea
    case v of
      Left e -> return (Left e)
      Right a -> runEitherT (f a)
