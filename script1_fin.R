# Script1. Randomized experiment (final) 
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
# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2016-09-28



# Clear memory and set random seed ---------------------------------------------
rm(list = ls()) # clear memory
graphics.off()  # clear all plots
cat("\014")     # clear console (same as Ctrl-L in console)
set.seed(123)   # Makes your simulation reproducable, use any number as input
#-------------------------------------------------------------------------------


# Set these parameters ---------------------------------------------------------
n <- 6e6  # Sample size
p_ses <- c(0.7, 0.3)  # Proportions ses = lo or hi
base <- c(50, 10)  # N(mean, sd) math at 'baseline' (no hi-ses and no treatment)
cses <- c(6, 2)  # N(mean, sd) casual effect of ses on math
ctreat <- c(2, 1)  # N(mean, sd) casual effect of treatment (music) on math

# P(complier, always-taker, never-taker, defier) among ses = hi 
pcomp_seshi <- c(0.95, 0, 0.05, 0)  # Sum to 1 

# P(complier, always-taker, never-taker, defier) among ses = lo 
pcomp_seslo <- c(0.65, 0, 0.35, 0)   # Sum to 1 
# ------------------------------------------------------------------------------


# Simulate pre-treatment variables ---------------------------------------------
id <- seq(from = 1, to = n)  # Participant id-numbers, to mimic real data

# Family socio-economic status (ses): low = 0, high = 1, with P(hi = 0.3)
ses <- sample(c(0, 1), replace = TRUE, size = n, prob = p_ses)
table(ses) / n  # Sanity check of ses simulation 
# ------------------------------------------------------------------------------


# Create data-frame ------------------------------------------------------------
d <- data.frame(id, ses)
rm(id, ses)  # Not necessary, but it doesn't hurt to clean up
# ------------------------------------------------------------------------------


# Simulate potential outcomes --------------------------------------------------
# Potential outcome if untreated
d$y0 <- rnorm(n, base[1], base[2]) + d$ses * rnorm(n, cses[1], cses[2])
# Potential outcome if treated
d$y1 <- d$y0 + rnorm(n, ctreat[1], ctreat[2])
d$unit_effect <- d$y1 - d$y0  # Single unit causal effect 
summary(d$unit_effect)
# ------------------------------------------------------------------------------


# Randomly assign to treatmen and control --------------------------------------
d$assign <- sample(c(0, 1), replace = TRUE, size = n) # Bernoulli trial method
table(d$assign, dnn = 'assignment')
# Balance check
table(d$ses, d$assign, dnn = c('ses', 'assign'))
# ------------------------------------------------------------------------------


# Received treatment (with 100 % compliers) -------------------------------------
d$received <- d$assign  # This code is for scenario without non-compliance
# ------------------------------------------------------------------------------


# Received treatment (with non-compliance) -------------------------------------
# Simulate complier status for ses == hi
comp_seshi <- d$ses * sample(c(1, 2, 3, 4), size = n, replace = TRUE, 
                             prob = pcomp_seshi)

# imulate complier status for ses == lo
comp_seslo <- (1 - d$ses) * sample(c(1, 2, 3, 4), size = n, replace = TRUE, 
                                   prob = pcomp_seslo)

# Define complier status (how does this work?), and make it a factor
d$complier_status <- comp_seshi + comp_seslo 
d$complier_status <- factor(d$complier_status, levels = c(1, 2, 3, 4),
                             labels = c('c', 'a', 'n', 'd'))

# Sanity check
table(d$complier_status[d$ses == 1])  # freq. should agree with pcomp_seshi
table(d$complier_status[d$ses == 0])  # freq. should agree with pcomp_seslo

# Potential received treatment given assignment
d$d0 <- 1 * (d$complier_status == 'a' | d$complier_status == 'd')
d$d1 <- 1 * (d$complier_status == 'c' | d$complier_status == 'a')

# Received treatment variable
d$received <- d$assign * d$d1 + (1 - d$assign) * d$d0

# Sanity checks
table(d$assign, d$complier_status, dnn = c('Assigned', 'Complier'))
table(d$assign, d$received, dnn = c('Assigned', 'Received'))
# ------------------------------------------------------------------------------


# Observed outcome -------------------------------------------------------------
d$math <- d$received * d$y1 + (1 - d$received) * d$y0  # Observed outcome
# ------------------------------------------------------------------------------


# Result analysis --------------------------------------------------------------
# True average effects (derived from true, and unobserved, single-unit effects)
ATE <- mean(d$unit_effect)
ATT <- mean(d$unit_effect[d$received == 1])
ATC <- mean(d$unit_effect[d$received == 0])
LATE <- mean(d$unit_effect[d$complier_status == 'c'])

# Estimated effects (derived from observed data)
as_treated <- mean(d$math[d$received == 1]) - mean(d$math[d$received == 0])
intent_to_treat <- mean(d$math[d$assign == 1]) - mean(d$math[d$assign == 0])
fi <- mean(d$received[d$assign == 1]) - mean(d$received[d$assign == 0])
rho <- intent_to_treat
lambda <- rho / fi  # fi: 'first stage', rho: 'reduced form', lambda: LATE

# Compare true and observed effects
effects <- c(ATE, ATT, ATC, LATE, as_treated, intent_to_treat, lambda)
names(effects) <- c('ATE', 'ATT', 'ATC', 'LATE', 
                    'as-treated', 'intent_to_treat', 'iv-analysis')
print(effects)
# ------------------------------------------------------------------------------
