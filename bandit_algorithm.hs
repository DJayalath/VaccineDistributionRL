-- A reinforcement learning bandit algorithm model (single state markov decision process)
-- of coronavirus vaccine administration choice.
-- This models the optimal delivery of vaccines to maximise overall efficacy within a population
-- given unknown efficacy of each individual vaccine.
-- The algorithm uses an epsilon greedy strategy to attempt to balance exploration vs exploitation and
-- reach convergence with the true optimal distribution as quickly as possible.

import Data.List
import System.Random

-- Actions represent the different available vaccines (Oxford / Pfizer / Moderna / Sinovac)
data Action = O | P | M | S deriving (Show, Read, Ord, Eq)

-- | Maximises long term efficacy from administering selection of vaccine.
-- Takes a list of estimated rewards, number of times given, epsilon, and number of vaccines to administer.
administer :: [ (Action, Double) ] -> [ (Action, Double) ] -> Double -> Int -> IO [ (Action, Double) ]
administer qs _ _ 0 = return qs
administer qs ns epsilon t = do 
    a <- selectAction qs epsilon
    r <- takeAction a
    let ns' = updateN ns a
    let qs' = updateQ qs ns' a r
    administer qs' ns' epsilon $ t - 1 

-- | Provides a reward function given a choice of action
-- This function is 'plug and play'. Replace with an input of real data (or a better model)
takeAction :: Action -> IO Double
takeAction O = randomRIO (0.33, 0.66)
takeAction P = randomRIO (0.70, 0.9)
takeAction M = randomRIO (0.6, 0.95)
takeAction S = randomRIO (0.4, 0.6)

-- | Selects actions using an epsilon greedy strategy
selectAction :: [ (Action, Double) ] -> Double -> IO Action
selectAction qs epsilon = do
    p <- randomIO :: IO Double
    if p > epsilon then
        return qBest
    else
        do i <- randomRIO (0, -1 + length qs')
           return . fst $ qs' !! i
    where best = maximum $ map snd qs
          qBest = revLookup best qs
          qBestPair = lookupPair qBest qs
          qs' = delete qBestPair qs

-- | Updates estimate for Q(A) : Q(A) <- Q(A) + (1 / N(A)) * (R - Q(A))
updateQ :: [ (Action, Double) ] -> [ (Action, Double) ] -> Action -> Double -> [ (Action, Double) ]
updateQ qs ns a r = (a, d + delta) : delete q qs
    where q@(_, d) = lookupPair a qs
          delta = (1.0 / (qLookup a ns)) * (r - d)

-- | Updates N(A) : N(A) <- N(A) + 1
updateN :: [ (Action, Double) ] -> Action -> [ (Action, Double) ]
updateN ns a = (a, i + 1) : delete n ns
    where n@(_, i) = lookupPair a ns

-- | Looks up a pair using the first element
lookupPair :: Eq a => a -> [ (a, b) ] -> (a, b)
lookupPair v = head . filter (\(x, y) -> x == v)

-- | Looks up the second element of a pair using the first
qLookup :: Eq a => a -> [ (a, b) ] -> b
qLookup v = snd . lookupPair v

-- | Looks up the first element of a pair using the second
revLookup :: Eq b => b -> [ (a, b) ] -> a
revLookup v = qLookup v . map (\(x, y) -> (y, x))
