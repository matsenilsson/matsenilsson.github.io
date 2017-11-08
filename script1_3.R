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
# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2016-10-29



# To be started at REX001 ...

# Clear things and set random seed ---------------------------------------------
rm(list = ls())
graphics.off()
cat("\014")
set.seed(123)
# ------------------------------------------------------------------------------


# Simulate data ----------------------------------------------------------------
n <- 600  # Sample size

id <- 1:n # Unique id number for each participant

# Random assignment
rand_assign <- sample(c(0, 1), replace = TRUE, size = n) # Bernoulli trial method

# Music lessons (for the moment assuming 100 % compliance)
music_lessons <- rand_assign

# Performance math test (for the moment assuming it is independent of treatment)
math <- rnorm(n, m = 50, sd = 10)  # Math test, mean = 50; sd = 10

# Make data set
d <- data.frame(id, rand_assign, music_lessons, math)


# To be contined at REX002 ...


# Define socio-economic status (SES)
d$ses <- sample(c(0, 1), prob = c(0.7, 0.3), size = n, replace = TRUE)

# Define nevertaker (how does this work?)
nt_hi <-      d$ses  * sample(c(0, 1), size = n, replace = TRUE, prob = c(0.9, 0.1)) 
nt_lo <- (1 - d$ses) * sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5))
d$u_nevertaker <- nt_hi + nt_lo 

# Redefine music-lessons: always zero for nevertakers
d$music_lessons[d$u_nevertaker == 1] <- 0

# Redefine mathscore to be dependent on ses
d$math <- rnorm(n, m = 50, sd = 10) + 10 * d$ses
# ------------------------------------------------------------------------------


# To be contined at REX003 ...


