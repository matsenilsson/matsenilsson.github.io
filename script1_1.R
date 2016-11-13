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
# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2016-11-03


# Clear things and set random seed ---------------------------------------------
rm(list = ls())
graphics.off()
cat("\014")
set.seed(123)
# ------------------------------------------------------------------------------


# Set these parameters ---------------------------------------------------------
n <- 6e2  # Sample size
p_ses <- c(0.7, 0.3)  # Proportions ses = lo and ses = hi
# ------------------------------------------------------------------------------


# Simulate pre-treatment variables ---------------------------------------------
id <- seq(from = 1, to = n)  # Participant id-numbers

# Family socio-economic status (ses): low = 0, high = 1
ses <- sample(c(0, 1), replace = TRUE, size = n, prob = p_ses)
table(ses) / n  # Sanity check
# ------------------------------------------------------------------------------


# To be continued at REX002 ...
