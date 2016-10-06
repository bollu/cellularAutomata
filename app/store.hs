-- This language extension helps write signatues for typeclass instances
{-# LANGUAGE InstanceSigs#-}

class Comonad w where
  cojoin :: w a -> w (w a)

  cobind :: w a -> (w a -> b) -> w b
  cobind = (=>>)

  (=>>) :: w a -> (w a -> b) -> w b
  (=>>) = cobind

newtype Store s a = Store (s, s -> a)

extract :: Store s a -> a
extract (Store (s, f)) = f $ s


instance Functor (Store s) where
  fmap :: (a -> b) -> Store s a -> Store s b
  fmap f (Store (s, extract)) = Store (s, f . extract)

instance Comonad (Store s) where
  cojoin :: Store s a -> Store s (Store s a)
  cojoin (Store (s, f)) = Store (s, f') where
          f' s' = (Store (s', f))

  cobind :: Store s a -> (Store s a -> b) -> Store s b
  cobind store extract_fn = fmap extract_fn (cojoin store)

