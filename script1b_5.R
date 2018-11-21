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
n = 1e3  # Sample size

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
vvg <-   rnorm(n) - 0.5 * support
pa <-    rnorm(n) + bvvg_pa * vvg
bmi <-   rnorm(n) + -1.2 * pa
sprob <- rnorm(n) + -0.8 * support  + 0.5 * genetic
agg <-   rnorm(n) + 0.3 * vvg      + bpa_agg * pa   + 1.5 * sprob + 1 * genetic
## -----------------------------------------------------------------------------


## Estimate causal effect ------------------------------------------------------
total_ce  # To be estimated = True effect

adj0 <- lm(agg ~ vvg)  # Bias: Omitted variable bias (= Confounding bias)

adj1 <- lm(agg ~ vvg + support) # No bias

adj2 <- lm(agg ~ vvg + sprob) # Bias: Collider bias

adj3 <- lm(agg ~ vvg + support + sprob) # No bias

adj4 <- lm(agg ~ vvg + pa + support) # Bias: Over-adjustment bias

adj5 <- lm(agg ~ vvg + bmi + support) # Bias: Over-adjustment bias
## -----------------------------------------------------------------------------


## Illustrate causal estimate --------------------------------------------------
# This function add points with confidence interval to an existing plot
add_estimate <- function(model, xpos) {
  # Estimnate fo causal effect
  estimate <- model$coefficients['vvg']
  # It's 95 % CI
  ci <- confint(model)['vvg',]
  
  # Draw CI in plot
  arrows(x0 = xpos, x1 = xpos, y0 = ci[1], y1 = ci[2], 
         length = 0.05, angle = 90, code = 3)
  # Draw point
  points(xpos, estimate, pch = 21, bg = 'grey')
}

# Make plot
plot(0, xlim = c(0, 5), ylim = c(-0.1, 1.5), pch = '', xlab = 'Model', 
     ylab = 'Causal estimate')
# Add line at true causal effect
lines(c(-1, 6), c(total_ce, total_ce), lty = 2)
# Add line at zero causal effect
lines(c(-1, 6), c(0, 0), lty = 1, col = 'grey')

# Use function to add data points and error bars (95 % CIs)
add_estimate(adj0, 0)
add_estimate(adj1, 1)
add_estimate(adj2, 2)
add_estimate(adj3, 3)
add_estimate(adj4, 4)
add_estimate(adj5, 5)
## -----------------------------------------------------------------------------