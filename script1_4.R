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
# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2016-11-16


# Clear things and set random seed ---------------------------------------------
rm(list = ls())
graphics.off()
cat("\014")
set.seed(123)
# ------------------------------------------------------------------------------


# Set these parameters ---------------------------------------------------------
n <- 600  # Sample size
p_ses <- c(0.7, 0.3)  # Proportions ses = lo and ses = hi
base <- c(50, 10)  # N(mean, sd) math at 'baseline' (no hi-ses and no treatment)
cses <- c(6, 2)  # N(mean, sd) casual effect of ses on math
ctreat <- c(2, 1)  # N(mean, sd) casual effect of treatment (music) on math
# ------------------------------------------------------------------------------


# Simulate pre-treatment variables ---------------------------------------------
id <- seq(from = 1, to = n)  # Participant id-numbers

# Family socio-economic status (ses): low = 0, high = 1
ses <- sample(c(0, 1), replace = TRUE, size = n, prob = p_ses)
table(ses) / n  # Sanity check
# ------------------------------------------------------------------------------


# Create data-frame ------------------------------------------------------------
d <- data.frame(id, ses)
rm(id, ses)  # Not necssary, but it doesnt hurt to clean up
# ------------------------------------------------------------------------------


# Randomly assign to treat and control -----------------------------------------
d$assign <- sample(c(0, 1), replace = TRUE, size = n)  # Bernoulli trial method
table(d$assign, dnn = "group")
#Balance check
table(d$ses, d$assign, dnn = c('ses', 'group'))
# ------------------------------------------------------------------------------


# Simulate potential outcomes --------------------------------------------------
# Potential outcome if untreated
d$y0 <- rnorm(n, base[1], base[2]) + d$ses * rnorm(n, cses[1], cses[2])
# Potential outcome if treated
d$y1 <- d$y0 + rnorm(n, ctreat[1], ctreat[2])
d$unit_effect <- d$y1 - d$y0  # Single unit causal effect 
summary(d$unit_effect)
# ------------------------------------------------------------------------------


# Received treatment (with 100 % compliers) -------------------------------------
d$received <- d$assign  # This code is for scenario without non-compliance
d$complier_status <- rep(1, length(d$received))
d$complier_status <- factor(d$complier_status, levels <- c(1, 2, 3, 4),
                            labels = c('c', 'a', 'n', 'd'))
# ------------------------------------------------------------------------------


# Observed outcome -------------------------------------------------------------
d$math <- d$received * d$y1 + (1 - d$received) * d$y0  # Observed outcome
# ------------------------------------------------------------------------------


# Result analysis --------------------------------------------------------------
# True effects
ACE <- mean(d$unit_effect)  # True Average causal effect
ATT <- mean(d$unit_effect[d$received == 1])
ATC <- mean(d$unit_effect[d$received == 0])

# Estimated effects
as_treated <- mean(d$math[d$received == 1]) - mean(d$math[d$received == 0])
intent_to_treat <- mean(d$math[d$assign == 1]) - mean(d$math[d$assign == 0])

t_out1 <- t.test(d$math ~ d$received)



# To be continued at REX005 ...