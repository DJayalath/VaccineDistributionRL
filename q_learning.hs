-- A q learning algorithm for optimising efficacy from mixed vaccine doses.
-- This might be useful for investigating the efficacy of mixing the type of vaccine given in the first and
-- second doses if this leads to faster vaccination of the population given limited supply.

import Data.List
import Data.Ord
import System.Random

-- Actions represent the different available vaccines that can be given (Oxford / Pfizer / Moderna / Sinovac)
data Action = O | P | M | S | None deriving (Show, Read, Ord, Eq)
-- Represents the vaccine given in the first and second dose
type State = (Action, Action)
-- Represents an association of a state/action pair (S, A) with Q(S, A)
type ActionQ = ((State, Action), Double)

actions = [ O, P, M, S, None ]

initStates = [ (a1, a2) | a1 <- actions, a2 <- actions ]

-- Initial value of all states is zero (pass this to administer initially).
initQs = [ ((s, a), 0.0) | s <- initStates, a <- actions ]

-- | Administers vaccines with epsilon greedy strategy.
-- Completes t loops (t = number of doses to distribute).
administer :: [ ActionQ ] -> State -> Double -> Double -> Double -> Int -> IO [ ActionQ ]
administer qs s gamma epsilon eta 0 = return qs
administer qs s gamma epsilon eta t = do 
    a <- selectAction qs s epsilon
    (s', r) <- uncurry takeAction $ fst a
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

-- | Updates estimate for Q(S, A) : Q(S, A) <- Q(S, A) + eta * (R(S') + gamma * max Q(S', A) - Q(S, A))
updateQ :: [ ActionQ ] -> ActionQ -> State -> Double -> Double -> Double -> [ ActionQ ]
updateQ qs ((s, a), q) s' r eta gamma = ((s, a), q') : delete ((s, a), q) qs
    where reachable = nextStates s a
          qBestNext | null reachable = 0.0
                    | otherwise = snd . maximumBy (comparing snd) $ filter ((`elem` nextStates s a) . fst) qs 
          q' = q + eta * (r + gamma * qBestNext - q)

-- | Computes states reachable from current state after taking action a
nextStates :: State -> Action -> [ (State, Action) ]
nextStates (None, None) a = [ ((a, b), b) | b <- actions ]
nextStates (_, _) _ = []

-- | Provides a reward function given a choice of action from a state
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
