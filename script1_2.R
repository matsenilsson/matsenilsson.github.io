# Script1. Here we will extend the simple scenario from last time (bivariate) 
# by adding a confounding variable, and then 
# analyze and visualize it using linear regression.
# 
# Independent variable -- Average number of steps (x1000) taken per day during 
#                         study period (measure of physical activity)
# Dependent variable   -- Waist circumference (cm) at end of study
# Covariate            -- Average number of calories (x1000) per day 
#                         (estimated from diaries during study period)

# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2019-10-28



## Clear things to start clean! ------------------------------------------------
rm(list = ls())  # Clear the global environment
graphics.off()  # Clear all plots
cat("\014")  # Clear the console
## -----------------------------------------------------------------------------


## Simulation with a covariate (potential confounder) --------------------------
set.seed(999)  # Any number may be used

# Number of observations
n <- 1e3

# Causal effects
ce <- -0.4 # Causal effect of independent on dependent variable 
dk <-  0.0 # Causal effect of covariate on independent variable
dw <-  5.0 # Causal effect of covariate on dependent variable

# Confounder, calories x 1000 per day from diet (2 is 2000 calories)
diet <- rnorm(n, mean = 2, sd = 0.5)

# Independet variable (exposure): Average 1000-steps per day during study
ksteps <- rnorm(n, mean = 6, sd = 2.5) + dk * diet

# Dependent variable (outcome): Waist circumference at end of study
waistcm <- rnorm(n, 100, 5) + ce * ksteps + dw * diet
## -----------------------------------------------------------------------------


## Analyze simulated data ------------------------------------------------------
# Linear regression model
crude <- lm(waistcm ~ ksteps)  # Crude model
adjusted <- lm(waistcm ~ ksteps + diet)  # Crude model
summary(crude)
summary(adjusted)

# Simple scattergram with regression line
plot(ksteps, waistcm)
abline(crude)

# Scatterplot matrix (lower.panel = panel.smooth add smooth line)
pairs(~ waistcm + ksteps + diet, 
      lower.panel = panel.smooth)
## -----------------------------------------------------------------------------


## Simple result plot ----------------------------------------------------------
# Extract regression results from the two models
b_crude <- crude$coefficients[2]  
ci_crude <- confint(crude)[2, ] 
b_adj <- adjusted$coefficients[2]   
ci_adj <- confint(adjusted)[2, ] 

# Empty plot (NA = missing data!)
plot(NA,
     xlim = c(0, 3), ylim = c(-1, 1), 
     xlab = 'Model', ylab = 'Causal effect estimate [cm/ksteps]', xaxt ='n') 

# Add ci (precision) for independent variable
arrows(x0 = 1, x1 = 1, y0 = ci_crude[1], y1 = ci_crude[2], 
       code = 3, length = 0.1, angle = 90)
arrows(x0 = 2, x1 = 2, y0 = ci_adj[1], y1 = ci_adj[2], 
       code = 3, length = 0.1, angle = 90) 

# Add point estimate for independent variable
points(1, b_crude, pch = 21, bg = 'grey')
points(2, b_adj, pch = 21, bg = 'grey') # Crude

# Add line at no effect
lines(c(0, 3), c(0, 0))

# Add line at true effect
lines(c(0, 3), c(ce, ce), lty = 3, col = 'grey')
## -----------------------------------------------------------------------------
