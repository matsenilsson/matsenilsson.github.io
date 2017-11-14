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
d$math <- rnorm(n, m = 50, sd = 10) + 10 * d$ses  + 2 * d$music_lessons
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


