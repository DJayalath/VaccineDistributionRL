# RL Algorithms for Vaccine Distribution
Attempts at optimising vaccine distribution using reinforcement learning.

## Problem
A government is in posession of several different vaccines (e.g. Pfizer, Moderna, Oxford, Sinovac) but does not
know the true efficacy of these vaccines. How should they be distributed among the population to optimise
efficacy?

## Idea 1: Bandit Algorithm
Use a bandit algorithm (single state Markov Decision Process) to learn the efficacy and quickly converge to an
optimal distribution of vaccines, balancing exploration vs exploitation.

### Issues
The reward function can't be exactly modelled as in a typical bandit algorithm. We can't know how effective a vaccine
is on a particular individual before vaccinating the next without waiting a few weeks. It may however be effective to
start the model with an n week delay and use the rewards from n weeks past for each action.
