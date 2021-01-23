-- A q learning algorithm for optimising efficacy from mixed vaccine dosages.
-- This might be useful for investigating the efficacy of mixing the type of vaccine given in the first and
-- second doses if this leads to faster vaccination of the population given limited supply.

import Data.List
import Data.Ord
import System.Random

-- Actions represent the different available vaccines (Oxford / Pfizer / Moderna / Sinovac)
data Action = O | P | M | S | None deriving (Show, Read, Ord, Eq)
type State = (Action, Action)
-- Represents an association of a state/action pair (S, A) with Q(S, A)
type ActionQ = ((State, Action), Double)

actions = [ O, P, M, S, None ]

initStates = [ (a1, a2) | a1 <- actions, a2 <- actions ]

initQs = [ ((s, a), 0.0) | s <- initStates, a <- actions ]

-- | Administers vaccines with epsilon greedy strategy.
-- Completes t loops (t = population size).
administer :: [ ActionQ ] -> State -> Double -> Double -> Double -> Int -> IO [ ActionQ ]
administer qs s gamma epsilon eta 0 = return qs
administer qs s gamma epsilon eta t = do 
    a <- selectAction qs s epsilon
    (s', r) <- takeAction (fst . fst $ a) (snd . fst $ a)
    administer (updateQ qs a s' r eta gamma) s' gamma epsilon eta $ t - 1 

-- | Selects actions using an epsilon greedy strategy
selectAction :: [ ActionQ ] -> State -> Double -> IO ActionQ
selectAction qs s epsilon = do

    p <- randomIO :: IO Double

    if p > epsilon
       then return bestAction
       else pickRand $ randomRIO (0, length qs - 2)

    where filtered = filter ((== s) . fst . fst) qs
          bestAction = maximumBy (comparing snd) filtered 
          pickRand mr = do
              r <- mr
              return $ delete bestAction qs !! r

-- | Updates estimate for Q(A) : Q(A) <- Q(A) + (1 / N(A)) * (R - Q(A))
updateQ :: [ ActionQ ] -> ActionQ -> State -> Double -> Double -> Double -> [ ActionQ ]
updateQ qs ((s, a), q) s' r eta gamma = ((s, a), q') : delete ((s, a), q) qs
    where nextStates = getStates s a
          bestNextAction | null nextStates = 0.0
                         | otherwise = snd . maximumBy (comparing snd) $ filter ((`elem` getStates s a) . fst) qs 
          q' = q + eta * (r + gamma * bestNextAction - q)

-- | Computes states reachable from current state after taking action a
getStates :: State -> Action -> [ (State, Action) ]
getStates (None, None) a = [ ((a, b), b) | b <- actions ]
getStates (_, _) _ = []


-- | Provides a lookup for records with a runtime error (that should never occur)
-- in the event the record doesn't exist.
safeLookup :: Eq a => a -> [ (a, b) ] -> b
safeLookup x xs = case lookup x xs of
                    Just y -> y
                    Nothing -> error "Failed to lookup"

-- | Provides a reward function given a choice of action                                    
-- This function is 'plug and play'. Replace with an input of real data (or a better model).
-- You can ignore this function otherwise, it is merely for testing.                        
takeAction :: State -> Action -> IO (State, Double)                                                           
takeAction (None, None) a = do r <- rew a
                               return ((a, None), r)
takeAction (s, None) a = do r <- rew a
                            return ((s, a), r)
takeAction (a, b) _ = return ((a, b), 0.0)
rew O = randomRIO (0.33, 0.66)                                                       
rew P = randomRIO (0.70, 0.9)                                                        
rew M = randomRIO (0.6, 0.95)                                                        
rew S = randomRIO (0.4, 0.6)
rew None = return 0.0
