module DurationStringMonad where

-- Defining a monad (the duration monad) --

data DS a = DS (Int, String, a) deriving Show

getDuration :: DS a -> Int
getDuration (DS (d,s,x)) = d

getValue :: DS a -> a
getValue (DS (d,s,x)) = x

getString :: DS a -> String
getString (DS (d,s,x)) = s

instance Functor DS where
  fmap f (DS (i,s,x)) = DS (i,s,f x)

instance Applicative DS where
  pure x = (DS (0,"",x))
  (DS (i,s,f)) <*> (DS (j,s',x)) = (DS (i+j,s++s',f x))
  
instance Monad DS where
    (DS (i,s,x)) >>= k = DS (i + (getDuration (k x)), s ++ (getString (k x)) ,getValue (k x))
    return x = (DS (0,"",x))

wait1 :: DS a -> DS a
wait1 (DS (d,s,x)) = DS (d+1,s,x)

wait :: Int -> DS a -> DS a
wait i (DS (d,s,x)) = DS (i + d,s, x) 
