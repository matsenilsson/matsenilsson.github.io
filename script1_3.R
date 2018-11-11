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
# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2018-11-11



# To be started at REX001 ...

## Clear things and set random seed --------------------------------------------
rm(list = ls())
graphics.off()
cat("\014")
set.seed(123)
## -----------------------------------------------------------------------------


## Simulate data ---------------------------------------------------------------
n <- 600 # Sample size

id <- 1:n # Unique id number for each participant

# Random assignment
rand_assign <- sample(c(0, 1), replace = TRUE, size = n) # Bernoulli trial method

# Music lessons (for the moment assuming 100 % compliance)
music_lessons <- rand_assign

# Socoeconomic status
ses <- sample(c(0, 1), prob = c(0.7, 0.3), size = n, replace = TRUE)

# Performance math test (dependent on SES but independent of treatment)
math <- rnorm(n, m = 50, sd = 10) + 10 * ses # Math test, mean = 50; sd = 10

# Make data set
d <- data.frame(id, rand_assign, ses, music_lessons, math)

# Remove varibles, as they now are in data frame d
rm(id, rand_assign, ses, music_lessons, math)
## -----------------------------------------------------------------------------


## Some checks -----------------------------------------------------------------
# Balance check
balance_table <- table(rand_assign = d$rand_assign, ses = d$ses)  
balance_table_prop <- prop.table(balance_table, margin = 1)
round(balance_table_prop, 2)

# Effect of ses on math
boxplot(d$math ~ d$ses)
t.test(d$math ~ d$ses) 

# Effect of music lessons on math
boxplot(d$math ~ d$music_lessons)
t.test(d$math ~ d$music_lessons)
## -----------------------------------------------------------------------------




# To be contined at REX003 ...



