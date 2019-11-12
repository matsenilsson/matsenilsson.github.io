# Script1. Here we will extend the simple scenario from last time (bivariate) 
# by adding a third variable, and then 
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


## Simulation with a mediator = diet  ------------------------------------------
set.seed(999)  # Any number may be used

# Number of observations
n <- 5e3

# Causal effects
kw <- -0.4 # Causal effect of independent on dependent variable 
kd <- -0.2 # Causal effect of independent variable on mediator
dw <-  1.0 # Causal effect of mediator on dependent variable

# total causal effect of ksteps on waistcm
ate <- kw + kd * dw  # ate for "average total effect"

# Independet variable (exposure): Average 1000-steps per day during study
ksteps <- rnorm(n, mean = 6, sd = 2.5)

# Mediator, calories x 1000 per day from diet (2 is 2000 calories)
diet <- rnorm(n, mean = 2, sd = 0.5) + kd * ksteps

# Dependent variable (outcome): Waist circumference at end of study
waistcm <- rnorm(n, 100, 5) + kw * ksteps + dw * diet

# Put in data frame
d <- data.frame(diet, ksteps, waistcm)

# Remove stuff (not necessary, but doesn't hurt)
rm(diet, ksteps, waistcm)
## -----------------------------------------------------------------------------


## Analyze simulated data ------------------------------------------------------
# Linear regression model
crude <- lm(d$waistcm ~ d$ksteps)  # Crude model
adjusted <- lm(d$waistcm ~ d$ksteps + d$diet)  # Adjusted model
summary(crude)
summary(adjusted)

# Simple scattergram with regression line
plot(d$ksteps, d$waistcm)
abline(crude)

# Scatterplot matrix. NB!: Takes time for large n!
#pairs(~ d$ksteps + d$diet+ d$waistcm, 
#      lower.panel = panel.smooth)
## -----------------------------------------------------------------------------


## Simple result plot ----------------------------------------------------------

# Extract regression results from the two models
b_crude <- crude$coefficients[2]  
ci_crude <- confint(crude)[2, ] 
b_adj <- adjusted$coefficients[2]   
ci_adj <- confint(adjusted)[2, ] 

# Decide range of axis (from minimum to maximum value)
xlim = c(0.5, 2.5)  
ylim = c(-1, 1)     

# Empty plot (NA = missing data!)
plot(NA,
     xlim = xlim, ylim = ylim, 
     xlab = 'Model', ylab = 'Causal effect estimate [cm/ksteps]', axes = FALSE) 

# Add ci (precision) for independent variable
arrows(x0 = 1, x1 = 1, y0 = ci_crude[1], y1 = ci_crude[2], 
       code = 3, length = 0.1, angle = 90)
arrows(x0 = 2, x1 = 2, y0 = ci_adj[1], y1 = ci_adj[2], 
       code = 3, length = 0.1, angle = 90) 

# Add point estimate for independent variable
points(1, b_crude, pch = 21, bg = 'grey') # Crude, at x = 1
points(2, b_adj, pch = 21, bg = 'grey')  # Adjusted, at x = 2

# Add line at no effect
lines(xlim, c(0, 0))

# Add line at true effect
lines(xlim, c(kw, kw), lty = 3, col = 'grey')

# Add x-axis
axis(1, at = c(0.5, 1, 2, 2.5), labels = c('', 'Crude', 'Adjusted', ''), 
     tck = 0.01, pos = ylim[1])
# Add y-axis
axis(2, las = 2, tck = 0.01, pos = xlim[1])
## -----------------------------------------------------------------------------


## Simple result plot ----------------------------------------------------------

# Extract regression results from the two models
b_crude <- crude$coefficients[2]  
ci_crude <- confint(crude)[2, ] 
b_adj <- adjusted$coefficients[2]   
ci_adj <- confint(adjusted)[2, ] 

# Decide range of axis (from minimum to maximum value)
xlim = c(0.5, 2.5)  
ylim = c(-1, 1)     

# Empty plot (NA = missing data!)
plot(NA,
     xlim = xlim, ylim = ylim, 
     xlab = 'Model', ylab = 'Causal effect estimate [cm/ksteps]', axes = FALSE) 

# Add ci (precision) for independent variable
arrows(x0 = 1, x1 = 1, y0 = ci_crude[1], y1 = ci_crude[2], 
       code = 3, length = 0.1, angle = 90)
arrows(x0 = 2, x1 = 2, y0 = ci_adj[1], y1 = ci_adj[2], 
       code = 3, length = 0.1, angle = 90) 

# Add point estimate for independent variable
points(1, b_crude, pch = 21, bg = 'grey') # Crude, at x = 1
points(2, b_adj, pch = 21, bg = 'grey')  # Adjusted, at x = 2

# Add line at no effect
lines(xlim, c(0, 0))

# Add line at true effect
lines(xlim, c(ate, ate), lty = 3, col = 'grey')

# Add x-axis
axis(1, at = c(0.5, 1, 2, 2.5), labels = c('', 'Crude', 'Adjusted', ''), 
     tck = 0.01, pos = ylim[1])
# Add y-axis
axis(2, las = 2, tck = 0.01, pos = xlim[1])
## -----------------------------------------------------------------------------