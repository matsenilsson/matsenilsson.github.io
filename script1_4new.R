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
# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2017-11-14



# To be started at REX001 ...

# Clear things and set random seed ---------------------------------------------
rm(list = ls())
graphics.off()
cat("\014")
set.seed(123)
# ------------------------------------------------------------------------------


# Set these parameters ---------------------------------------------------------
n <- 600 # Sample size
phi_ses <- 0.3  # Proportion high SES
pnt_hi <- 0.1  # Proportion nevertakers among high-SES children
pnt_lo <- 0.5  # Proportion nevertakers among low-SES children
causal_effect <- 2  # Mean causal effect
sd_causal_effect <- 1  # Standard deviation of causal effect
# ------------------------------------------------------------------------------


# Simulate data ----------------------------------------------------------------
id <- 1:n # Unique id number for each participant

# Define socio-economic status (SES)
ses <- sample(c(0, 1), prob = c(1 - phi_ses, phi_ses), size = n, replace = TRUE)

# Define complier (how does this work?). 1 = nevertaker, 0 = complier. We are
# assuming no always-takers and no defiers
nt_hi <-      ses  * sample(c(0, 1), size = n, replace = TRUE, 
                              prob = c(1 - pnt_hi, pnt_hi)) 
nt_lo <- (1 - ses) * sample(c(0, 1), size = n, replace = TRUE, 
                              prob = c(1 - pnt_lo, pnt_lo))
u_nevertaker <- nt_hi + nt_lo 

# Potential outcomes
u_Y0 <- rnorm(n, m = 50, sd = 10) + 10 * ses
u_Y1 <- u_Y0 + rnorm(n, m = causal_effect, sd = sd_causal_effect)
#u_Y1[u_nevertaker == 1] <- u_Y0[u_nevertaker == 1]  # Never-takers have the same outcome under both

# Single unit causal effect (delta)
u_delta <- u_Y1 - u_Y0

# Random assignment
rand_assign <- sample(c(0, 1), replace = TRUE, size = n) # Bernoulli trial method

# Recieved treatment
music_lessons <- rand_assign * (1 - u_nevertaker)  # No music lessons for nevertakers

# Observed scores
math <- u_Y0  # Math scores untreated
math[music_lessons == 1] <- u_Y1[music_lessons == 1]  # Math score treated

# Make data set
d <- data.frame(id, u_Y0, u_Y1, u_delta, u_nevertaker, ses, rand_assign, music_lessons, math)
# ------------------------------------------------------------------------------


# To be contined at REX003 ...

# Intent to treat analysis (ITT) -----------------------------------------------
itt_balance <- table('ses' = d$ses, 'assignment' = d$rand_assign)
prop.table(itt_balance, 2)

assign_control <- mean(d$math[d$rand_assign == 0])
assign_treat <- mean(d$math[d$rand_assign == 1])
itt_estimate <- assign_treat - assign_control

itt_lm <- lm(d$math ~ d$rand_assign)
summary(itt_lm)
# ------------------------------------------------------------------------------


# As-treated analysis (AT) -----------------------------------------------------
at_balance <- table('ses' = d$ses, 'received' = d$music_lessons)
prop.table(at_balance, 2)

received_control <- mean(d$math[d$music_lessons == 0])
received_treat <- mean(d$math[d$music_lessons == 1])
at_estimate <- received_treat - received_control

at_lm <- lm(d$math ~ d$music_lessons)
summary(at_lm)
# ------------------------------------------------------------------------------


# Per protocol (pp) analysis ---------------------------------------------------
dd <- d[d$rand_assign == d$music_lessons, ]

pp_balance <- table('ses' = dd$ses, 'assignment' = dd$rand_assign)
prop.table(pp_balance, 2)

pp_control <- mean(dd$math[dd$rand_assign == 0])
pp_treat <- mean(dd$math[dd$rand_assign == 1])
pp_estimate <- pp_treat - pp_control

pp_lm <- lm(dd$math ~ dd$rand_assign)
summary(pp_lm)
# ------------------------------------------------------------------------------


# To be contined at REX004 ...


