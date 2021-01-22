-- Bandit algorithm is a one-state MDP
-- Described below is a bandit algorithm for vaccine administration given
-- a choice between Oxford (AstraZeneca) / Pfizer / Moderna / Sinovac

-- Patients are administered a choice of vaccine. A reward is given if it is determined
-- that the patient did not catch the virus after 1 month.

import Data.List
import System.Random

data Action = O | P | M | S deriving (Show, Read, Ord, Eq)

-- Produces expected rewards
banditAlgorithm :: [ (Action, Double) ] -> [ (Action, Double) ] -> Int -> IO [ (Action, Double) ]
banditAlgorithm qs ns 0 = return qs
banditAlgorithm qs ns t = do 
    let a = selectAction qs
    r <- takeAction a
    let ns' = incrementAction ns a
    let qs' = updateQ qs ns' a r
    banditAlgorithm qs' ns' $ t - 1 

takeAction :: Action -> IO Double
takeAction O = randomRIO (0.33, 0.66)
takeAction P = randomRIO (0.70, 0.9)
takeAction M = randomRIO (0.6, 0.95)
takeAction S = randomRIO (0.4, 0.6)

selectAction :: [ (Action, Double) ] -> Action
selectAction qs = revLookup best qs
    where best = maximum $ map snd qs

updateQ :: [ (Action, Double) ] -> [ (Action, Double) ] -> Action -> Double -> [ (Action, Double) ]
updateQ qs ns a r = (a, d + delta) : delete q qs
    where q@(_, d) = lookupPair a qs
          delta = (1.0 / (qLookup a ns)) * (r - d)

incrementAction :: [ (Action, Double) ] -> Action -> [ (Action, Double) ]
incrementAction ns a = (a, i + 1) : delete n ns
    where n@(_, i) = lookupPair a ns

lookupPair :: Eq a => a -> [ (a, b) ] -> (a, b)
lookupPair v = head . filter (\(x, y) -> x == v)

qLookup :: Eq a => a -> [ (a, b) ] -> b
qLookup v = snd . lookupPair v

revLookup :: Eq b => b -> [ (a, b) ] -> a
revLookup v = qLookup v . map (\(x, y) -> (y, x))
