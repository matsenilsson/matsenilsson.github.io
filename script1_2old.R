# Script1. Randomized experiment (under construction) 
#
# The simulated scenario is a simple randomized experiment on the effect of extra 
# music lessons on math performance.
#
# Simulated variables:
#  1. Independent variable: At the start of grade 6, all children at a specific 
#     school are randomly assigned to receive either extra music lessons (treatment) 
#     or no extra music lessons (control).    
#  2. Background (or pre-treatment) variable: Socioeconomic status, defined based 
#     on residential area (either high-income or low-income area). 
#  3. Outcome variable: Score on math test taken at the end of the semester.
#
# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2018-11-03

# To be started at REX001 ...


## Clear things and set random seed --------------------------------------------
rm(list = ls())  # Clears the global environment
graphics.off()  # Clear all plots
cat("\014")  # Clears the console
set.seed(123)  # Seet sed of random number generator
## -----------------------------------------------------------------------------


## Simulate data ---------------------------------------------------------------
n <- 600  # Sample size

id <- 1:n # Unique id number for each participant

# Random assignment, Bernoulli trial method
rand_assign <- sample(c(0, 1), replace = TRUE, size = n) 

# Music lessons (for the moment assuming 100 % compliance)
music_lessons <- rand_assign

# Performance math test (for the moment assuming it is independent of treatment)
math <- rnorm(n, m = 50, sd = 10)  # Math test, mean = 50; sd = 10

# Make data set
d <- data.frame(id, rand_assign, music_lessons, math)
# ------------------------------------------------------------------------------


# To be continued at REX002 ...