# RL Algorithms for Vaccine Distribution
Attempts at modelling optimal vaccine distribution using reinforcement learning. Solutions are basic prototypes for
implementing ideas below.

## Problem 1 : Distribution of vaccine types
A government is in posession of several different vaccines (e.g. Pfizer, Moderna, Oxford, Sinovac) but does not
know the true efficacy of these vaccines. How should they be distributed among the population to optimise
efficacy?

### Idea : Bandit Algorithm
Use a bandit algorithm (single state Markov Decision Process) to learn the efficacy and quickly converge to an
optimal distribution of vaccines, balancing exploration vs exploitation.

### Issues
The reward function can't be exactly modelled as in a typical bandit algorithm. We can't know how effective a vaccine
is on a particular individual before vaccinating the next without waiting a few weeks. It may however be effective to
start the model with an n week delay and use the rewards from n weeks past for each action.

We cannot account for the supply of vaccines available.

## Problem 2 : Distribution of first and second doses
If due to supply constraints we have to mix vaccine types in both doses to minimise vaccination time, how can we
determine the most effective way to mix the vaccine types distributed if we don't know how effective each permutation
is?

### Idea : Markov Decision Process -> Q Learning
Use a q learning algorithm to learn Q(S, A) where the states are the different ways we can give people two doses.

### Issues
Same delayed reward problem as with the bandit algorithm.
