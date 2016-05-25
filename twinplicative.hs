{-# LANGUAGE InstanceSigs #-}

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap .fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  --                    f   g
  pure a = Compose $ pure $ pure a
  -- pure tells me we are going to have two applies (App)
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $
                                fmap (<*>) f <*> a
                                -- where is application taking place
                                -- fmap or lifted <*> or <*>
