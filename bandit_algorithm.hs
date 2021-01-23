-- A reinforcement learning bandit algorithm model (single state markov decision process)
-- of coronavirus vaccine administration choice.
-- This models the optimal delivery of vaccines to maximise overall efficacy within a population
-- given unknown efficacy of each individual vaccine.
-- The algorithm uses an epsilon greedy strategy for exploration vs exploitation balancing

import Data.List
import Data.Ord
import System.Random

-- Actions represent the different available vaccines (Oxford / Pfizer / Moderna / Sinovac)
data Action = O | P | M | S deriving (Show, Read, Ord, Eq)
-- Represents an association of an action A with Q(A) and N(A)
type ActionQN = (Action, (Double, Double))

initAqns = [ (O, (0.0, 0.0)), (P, (0.0, 0.0)), (M, (0.0, 0.0)), (S, (0.0, 0.0))  ]

-- | Administers vaccines with epsilon greedy strategy.
-- Completes t loops (t = population size).
administer :: [ ActionQN ] -> Double -> Int -> IO [ ActionQN ]
administer aqns epsilon 0 = return aqns
administer aqns epsilon t = do 
    a <- selectAction aqns epsilon
    r <- takeAction . fst $ a
    administer (updateQ r $ updateN aqns a) epsilon $ t - 1 

-- | Selects actions using an epsilon greedy strategy
selectAction :: [ ActionQN ] -> Double -> IO ActionQN
selectAction aqns epsilon = do

    p <- randomIO :: IO Double
    if p > epsilon
       then return bestAction
       else pickRand $ randomRIO (0, length aqns - 2)

    where bestAction = maximumBy (comparing $ fst . snd) aqns
          pickRand mr = do
              r <- mr
              return $ delete bestAction aqns !! r

-- | Updates estimate for Q(A) : Q(A) <- Q(A) + (1 / N(A)) * (R - Q(A))
updateQ :: Double -> (ActionQN, [ ActionQN ])-> [ ActionQN ]
updateQ r ((a, (q, n)), aqns) = (a, (q', n)) : delete (a, (q, n)) aqns
    where q' = q + (1.0 / n) * (r - q)

-- | Updates N(A) : N(A) <- N(A) + 1
updateN :: [ ActionQN ] -> ActionQN -> (ActionQN, [ ActionQN ])
updateN aqns (a, (q, n)) = (aqn, aqn : delete (a, (q, n)) aqns)
    where aqn = (a, (q, n + 1))

-- | Provides a lookup for records with a runtime error (that should never occur)
-- in the event the record doesn't exist.
safeLookup :: Eq a => a -> [ (a, b) ] -> b
safeLookup x xs = case lookup x xs of
                    Just y -> y
                    Nothing -> error "Failed to lookup"

-- | Provides a reward function given a choice of action                                    
-- This function is 'plug and play'. Replace with an input of real data (or a better model).
-- You can ignore this function otherwise, it is merely for testing.                        
takeAction :: Action -> IO Double                                                           
takeAction O = randomRIO (0.33, 0.66)                                                       
takeAction P = randomRIO (0.70, 0.9)                                                        
takeAction M = randomRIO (0.6, 0.95)                                                        
takeAction S = randomRIO (0.4, 0.6)                                                         
                                                                                            
