data Two a b = Two a b deriving (Eq,Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Or a b = First a | Second b deriving (Eq,Show)

instance Functor (Or a) where
  fmap f (Second b) = Second ( f b )
  fmap _ (First a) = First a

data Pair a b = Pair a b deriving (Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

--  data Escaped
--  data Unescaped
--
--  newtype Template a = Template Text

-- render :: Template Escaped
--        -> IO ()
