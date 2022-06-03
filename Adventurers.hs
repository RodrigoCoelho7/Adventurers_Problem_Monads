{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import DurationMonad

-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
-- Adventurers + the lantern
type Objects = Either Adventurer ()

-- The time that each adventurer needs to cross the bridge
-- To implement 
getTimeAdv :: Adventurer -> Int
getTimeAdv P1 = 1
getTimeAdv P2 = 2
getTimeAdv P5 = 5
getTimeAdv P10 = 10

{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state
of the game, with all adventurers and the lantern on the left side of
the bridge. Similarly, the function (const True) represents the end
state of the game, with all adventurers and the lantern on the right
side of the bridge.  --}
type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]



-- The initial state of the game
gInit :: State
gInit = const False

-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)

-- Changes the state of the game of a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os
                               

{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
-- To implement
verify_side :: [Objects] -> State -> Bool
verify_side (h:t) s = let b = s h in all (\x->s x==b) t

obtain_adventure :: Objects -> Adventurer
obtain_adventure (Left a) = a

cross_Adventures :: [Objects] -> State -> Duration State
cross_Adventures a s= let o = [(Right ())]++a
                          a_ = map obtain_adventure a  
                          t_ = map getTimeAdv a_
                          b = verify_side o s in if b then  Duration (maximum t_ ,(mChangeState o s)) else return s

oneAdventurer = [(Left P1),(Left P2),(Left P5),(Left P10)]

obtain_twoAdveturers :: [Objects] -> [[Objects]]
obtain_twoAdveturers [] = []
obtain_twoAdveturers (h:t) = (map (\x -> [h,x]) t) ++ (obtain_twoAdveturers t)

twoAdventurers = obtain_twoAdveturers oneAdventurer


allValidPlays :: State -> ListDur State
allValidPlays s = let oneL = map (\x->cross_Adventures [x] s) oneAdventurer
                      twoL = map (\x->cross_Adventures x s) twoAdventurers in LD (oneL++twoL)


{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
-- To implement 
exec :: Int -> State -> ListDur State
exec 0 s = return s
exec n s = do s1 <- allValidPlays s
              exec (n-1) s1

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement

verify_result :: Int -> ListDur State -> Bool
verify_result n ld = let o = oneAdventurer ++ [(Right ())]
                         ld_ = remLD ld
                         d = map remDur ld_ 
                         b_ = map (\(t,s)-> (t<=n && all (\y->s y ==True) o)) d in all (\x->x==True) b_

leqN :: Int -> Int -> Bool
leqN n_ 0 = False
leqN n_ n = let ld = exec n gInit
                b = verify_result n_ ld in b || leqN n_ (n-1)

leq17 :: Bool
leq17 = leqN 17 5

{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = leqN 16 5


--------------------------------------------------------------------------
{-- Implementation of the monad used for the problem of the adventurers.
Recall the Knight's quest --}

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

-- To implement

remDur :: Duration a -> (Int,a)
remDur x = (getDuration x, getValue x)

insert_dur :: (Int,a) -> Duration a
insert_dur (n,x) = Duration (n,x)

instance Functor ListDur where
   fmap f = let f' = \(n,x) -> (n, f x) in 
                   LD . (map insert_dur) . (map f') . (map remDur) . remLD

-- To implement
instance Applicative ListDur where
   pure x = LD [Duration (0,x)]
   l1 <*> l2 = LD $ do x <- map remDur (remLD l1)
                       y <- map remDur (remLD l2)
                       g(x,y) where g((n,f),(n',x)) = return (Duration (n+n',f x))

-- To implement
instance Monad ListDur where
   return = pure
   l >>= k = LD $ do x <- map remDur (remLD l)
                     g x where
                        g(n,x) = let u = (map remDur (remLD (k x))) in map (\(n',x) -> Duration (n + n', x)) u


manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)
--------------------------------------------------------------------------
