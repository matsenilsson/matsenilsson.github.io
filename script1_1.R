# Script1. Here we will simulate a simple bivariate relationship and then analyze
# and visualize it using linear regression.
# 
# Independent variable -- Average number of steps (x1000) taken per day during 
#                         study period (measure of physical activity)
# Dependent variable   -- Waist circumference (cm) at end of study

# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2019-10-28



## Clear things to start clean! ------------------------------------------------
rm(list = ls())  # Clear the global environment
graphics.off()  # Clear all plots
cat("\014")  # Clear the console
## -----------------------------------------------------------------------------


## Simulate data ---------------------------------------------------------------
set.seed(999)  # Any number may be used

# Number of observations
n <- 50

# Causal effect
ce <- -0.4  # Dependent variable increase per unit independent-variable increase

# Independet variable (exposure): Average 1000-steps per day during study
ksteps <- rnorm(n, mean = 6, sd = 2.5)

# Dependent variable (outcome): Waist circumference at end of study
waistcm <- rnorm(n, 100, 5) + ce * ksteps 
## -----------------------------------------------------------------------------


## Analyze simulated data ------------------------------------------------------
# Linear regression model, called crude
crude <- lm(waistcm ~ ksteps)  # Model output
summary(crude)  # Show model output

# Simple scattergram with regression line
plot(ksteps, waistcm)
abline(crude)
## -----------------------------------------------------------------------------


## Simple result plot ----------------------------------------------------------
# Extract regression results
b <- crude$coefficients  # Regression coefficients 
ci <- confint(crude) # 95 % CI around b

# Empty plot (NA = missing data!)
plot(NA,
     xlim = c(0, 2), ylim = c(-1, 1), 
     xlab = 'Crude', ylab = 'Causal effect estimate [cm/ksteps]', xaxt ='n') 

# Add ci (precision) for independent variable
arrows(x0 = 1, x1 = 1, y0 = ci[2, 1], y1 = ci[2, 2], 
       code = 3, length = 0.1, angle = 90)  

# Add point estimate for independent variable
points(1, b[2], pch = 21, bg = 'grey') 

# Add line at no effect
lines(c(0, 2), c(0, 0))

# Add line at true effect
lines(c(0, 2), c(ce, ce), lty = 3, col = 'grey')
## -----------------------------------------------------------------------------
