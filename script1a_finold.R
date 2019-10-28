# Script1. Randomized experiment
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


## Clear things and set random seed --------------------------------------------
rm(list = ls())
graphics.off()
cat("\014")
set.seed(123)
## -----------------------------------------------------------------------------


## Play with these parameters --------------------------------------------------
n <- 6e2  # Sample size
ce <- 2  # Average of single-unit causal effects of music lessons on math scores
sdce <- 1  # Standard deviation of single-unit causal effects

# Note that math scores has a sd of about 10 units
## -----------------------------------------------------------------------------


## Simulate data ---------------------------------------------------------------

id <- 1:n # Unique id number for each participant

# Socoeconomic status
ses <- sample(c(0, 1), prob = c(0.7, 0.3), size = n, replace = TRUE)

# Random assignment
rand_assign <- sample(c(0, 1), replace = TRUE, size = n) # Bernoulli trial method

# Music lessons
music_lessons <- rand_assign # Temporarily assuming 100 % compliance

# Nevertaker, related to SES
nt_hi <-      ses  * sample(c(0, 1), size = n, replace = TRUE, 
                            prob = c(0.9, 0.1))  # 10 % nevertakers among high SES

nt_lo <- (1 - ses) * sample(c(0, 1), size = n, replace = TRUE,  
                            prob = c(0.5, 0.5)) # 50 % nevertakers among low SES

u_nevertaker <- nt_hi + nt_lo  # Unobserved variable: nevertaker = 1

# Redefine music-lessons: always zero for nevertakers
music_lessons[u_nevertaker == 1] <- 0

# Potential outcomes
u_math0 <- rnorm(n, m = 50, sd = 10) + 10 * ses  # If no music lessons
u_singleunit_ce <- rnorm(n, mean = ce, sd = sdce) # Single-unit causal effect
u_math1 <- u_math0 +  u_singleunit_ce   # If music lessons

# observed outcome
math <- (1 - music_lessons) * u_math0 + music_lessons * u_math1

# Make data set
d <- data.frame(id, ses, rand_assign, u_nevertaker, u_math0, u_math1, 
                u_singleunit_ce, music_lessons, math)

# Clean up by removing varibles; they now are in data frame d
rm(id, ses, rand_assign, u_nevertaker, u_math0, u_math1, u_singleunit_ce,
   music_lessons, math,  nt_hi, nt_lo)
## -----------------------------------------------------------------------------


## Some checks (please uncommented to run these checks) ------------------------
# Balance check
# balance_table <- table(rand_assign = d$rand_assign, ses = d$ses)  
# balance_table_prop <- prop.table(balance_table, margin = 1)
# round(balance_table_prop, 2)

# Effect of ses on math
# t.test(d$math ~ d$ses) 
## -----------------------------------------------------------------------------


## True effects ----------------------------------------------------------------
# Average single-unit causal effect
ATE <- mean(d$u_singleunit_ce)
# Average single-unit causal effect among compliers, estimated by 
# the Instrumental variable analys.
LATE <- mean(d$u_singleunit_ce[d$music_lessons == d$rand_assign])
## -----------------------------------------------------------------------------


## Estimating the causal effect (ce) -------------------------------------------
# As treated analysis (not recommended!)
received_control <- mean(d$math[d$music_lessons == 0])
received_treat <- mean(d$math[d$music_lessons == 1])
at_estimate <- received_treat - received_control

# Per protocol (not recommended!)
dd <- d[d$rand_assign == d$music_lessons, ]  # Only include compliers
pp_control <- mean(dd$math[dd$rand_assign == 0])
pp_treat <- mean(dd$math[dd$rand_assign == 1])
pp_estimate <- pp_treat - pp_control

# Intention to treat analysis (better, but will underestimate due to nevertakers)
assign_control <- mean(d$math[d$rand_assign == 0])
assign_treat <- mean(d$math[d$rand_assign == 1])
itt_estimate <- assign_treat - assign_control

# Instrumental variable analyses. Read article by Angrist & Pischke (2015), 
# see especially Eq. 3.1.
first_stage <- mean(d$music_lessons[d$rand_assign == 1]) - 
               mean(d$music_lessons[d$rand_assign == 0])
reduced_form <- itt_estimate
iv_estimate <- itt_estimate / first_stage
## -----------------------------------------------------------------------------


## Main results of simulation --------------------------------------------------
effects <- c(ATE, LATE, at_estimate, pp_estimate, itt_estimate, iv_estimate)
names(effects) <- c('ATE', 'LATE', 'at', 'pp', 'itt', 'iv')
effects
## -----------------------------------------------------------------------------


