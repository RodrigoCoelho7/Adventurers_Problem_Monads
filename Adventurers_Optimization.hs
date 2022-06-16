{-# LANGUAGE FlexibleInstances #-}
module Adventurers_Optimization where

import DurationStringMonad

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

obtain_adventurer :: Objects -> Adventurer
obtain_adventurer (Left a) = a

getString_aux :: [Objects] -> State -> String
getString_aux [] s = ""
getString_aux (h:t) s = " "++(show (obtain_adventurer h))++ (getString_aux t s)++" "

getSide :: Objects -> State -> String
getSide a s = let b = s a in if b then "L" else "R"

getStrings :: [Objects] -> State -> String
getStrings (h:t) s = " -"++(getSide h s)++"[ "++(getString_aux (h:t) s)++ "]"

cross_Adventures :: [Objects] -> State -> (DS State,Bool)
cross_Adventures a s= let o = [(Right ())]++a
                          a_ = map obtain_adventurer a  
                          t_ = map getTimeAdv a_
                          b = verify_side o s in if b then (DS (maximum t_ , (getStrings a s),(mChangeState o s)),b) else (DS (0,"",s),b)

oneAdventurer = [(Left P1),(Left P2),(Left P5),(Left P10)]

obtain_twoAdventurers :: [Objects] -> [[Objects]]
obtain_twoAdventurers [] = []
obtain_twoAdventurers (h:t) = (map (\x -> [h,x]) t) ++ (obtain_twoAdventurers t)

twoAdventurers = obtain_twoAdventurers oneAdventurer

removeBool :: (DS a, Bool) -> DS a
removeBool (d,b) = d

allValidPlays :: State -> ListDur State
allValidPlays s = let oneL = map (\x->cross_Adventures [x] s) oneAdventurer
                      twoL = map (\x->cross_Adventures x s) twoAdventurers in LD (map removeBool (filter (\(d,b)->b) (oneL++twoL)))
 

{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
-- To implement 

calc_length :: ListDur a -> Int
calc_length ld = length $ remLD ld

exec_evaluate_aux ::  Int -> Int -> Int -> State -> Int
exec_evaluate_aux  i n t s = if i == (n+1) then 0 else (calc_length (exec i t s)) + exec_evaluate_aux (i+1) n t s

exec_evaluate :: Int -> Int -> State -> Int
exec_evaluate n t s = exec_evaluate_aux 1 n t s

exec :: Int -> Int -> State -> ListDur State
exec 0 _ s = return s
exec n t s = teste t $ do s1 <- allValidPlays s
                          exec (n-1) t s1
   
teste :: Int ->  ListDur State -> ListDur State
teste t l = LD $filter (\x->(getDuration x) <= t) $remLD l

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement

verify_result :: Int -> ListDur State -> Bool
verify_result n ld = let o = oneAdventurer ++ [(Right ())]
                         ld_ = remLD ld
                         d = map remDur ld_ 
                         b_ = map (\(t,st,s)-> (t<=n && all (\y->s y ==True) o)) d in any (\x->x==True) b_

leqN :: Int -> Int -> Bool
leqN n_ 0 = False
leqN n_ n = let ld = exec n n_ gInit
                b = verify_result n_ ld in b || leqN n_ (n-1)

findNaux :: Int -> Int -> Int -> Maybe (ListDur State)
findNaux i t n = if i==n+1 then Nothing else let o = oneAdventurer ++ [(Right ())]
                                                 b = leqN t i
                                                 l = exec i t gInit 
                                                 d = map remDur (remLD l) in if b then Just (LD (map insert_dur (filter (\(t',st,s)-> (t'<=t && all (\y->s y ==True) o)) d))) else findNaux (i+1) t n

findN :: Int -> Int -> Maybe (ListDur State)
findN t n = findNaux 1 t n

leq17 :: Bool
leq17 = leqN 17 5

{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
--Esta função não está bem implementada, é preciso provar que nunca pode ser menor do que 17 independentemente do numero de jogadas 
-- To implement
l17 :: Bool
l17 = leqN 16 5


--------------------------------------------------------------------------
{-- Implementation of the monad used for the problem of the adventurers.
Recall the Knight's quest --}

data ListDur a = LD [DS a] deriving Show

remLD :: ListDur a -> [DS a]
remLD (LD x) = x

-- To implement

remDur :: DS a -> (Int,String,a)
remDur x = (getDuration x, getString x, getValue x)

insert_dur :: (Int,String,a) -> DS a
insert_dur (n,s,x) = DS (n,s,x)

instance Functor ListDur where
   fmap f = let f' = \(n,s,x) -> (n, s, f x) in 
                   LD . (map insert_dur) . (map f') . (map remDur) . remLD

-- To implement
instance Applicative ListDur where
   pure x = LD [DS (0,"",x)]
   l1 <*> l2 = LD $ do x <- map remDur (remLD l1)
                       y <- map remDur (remLD l2)
                       g(x,y) where g((n,s,f),(n',s',x)) = return (DS (n+n',s++s',f x))

-- To implement
instance Monad ListDur where
   return = pure
   l >>= k = LD $ do x <- map remDur (remLD l)
                     g x where
                        g(n,s,a) = let u = (map remDur (remLD (k a))) in map (\(n',s',a) -> DS (n + n', s++s',a)) u


manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)
--------------------------------------------------------------------------
