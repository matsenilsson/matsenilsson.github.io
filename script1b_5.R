# Script1b. Simulating data consistent with a Directed Acyclical Graph(DAG):
# Violent video games (VVG) as exposure and Adult aggression (AGG) as outcome.
# Covariates: Physical activity (PA), Parental support (Support), 
# School problems (SProb), BMI (BMI), Genetic factor (Genetic).
# 
# DAG:
# VVG -> AGG
# VVG -> PA -> AGG
# PA -> BMI
# VVG <- Support -> SProb -> AGG
# SProb <- Genetic -> AGG



## Clear things and set random seed --------------------------------------------
rm(list = ls())
graphics.off()
cat("\014")
set.seed(123)
## -----------------------------------------------------------------------------


## Simulate data ---------------------------------------------------------------
n = 1e5  # Sample size

# Define total causal effect of vvg on agg
dir_ec <- 0.3  # Direct causal effect vvg on agg
bvvg_pa <- -1  # Causal effect vvg on pa
bpa_agg <- -0.3 # Causal effect pa pn agg
ind_ec <- bvvg_pa * bpa_agg  # Indirect causal effect
total_ce <- dir_ec + ind_ec # This is what we want to estimate!

# Exogenous variables
support <- rnorm(n)  # Standardized scores, drawn from the Standard normal distr.
genetic <- rnorm(n)

# Endogenous Variable
vvg <-   rnorm(n) + 0.5 * support
pa <-    rnorm(n) + bvvg_pa * vvg
bmi <-   rnorm(n) + 1.2 * pa
sprob <- rnorm(n) + 0.8 * support  + 0.5 * genetic
agg <-   rnorm(n) + 0.3 * vvg      + bpa_agg * pa   + 1.5 * sprob + 1 * genetic
## -----------------------------------------------------------------------------


## Estimate causal effect ------------------------------------------------------
total_ce 
crude <- lm(agg ~ vvg)
adj1 <- lm(agg ~ vvg + support)
## -----------------------------------------------------------------------------


# To be continued at REX005 ...
