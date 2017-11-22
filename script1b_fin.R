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



# Clear things and set random seed ---------------------------------------------
rm(list = ls())
graphics.off()
cat("\014")
set.seed(123)
# ------------------------------------------------------------------------------

# To be started at REX005 ...




# Simulate: All variables continous, normally distirbuted and on a standardized scale,
# all relationships linear.

n = 1e5


vvg <- rnorm(n)
pa <- rnorm(n) + 0.5 * vvg
agg <- rnorm(n) + 0.5 * vvg + 0.5 * pa


bmi <- rnorm(n) + 0.5 * pa

lm(agg ~ vvg)



# To be continued and finalized at REX006 ...

# Simulate full DAG ------------------------------------------------------------
support <- rnorm(n)
genetic <- rnorm(n)
sprob   <- rnorm(n) + 0.5 * support + 0.5 * genetic
vvg     <- rnorm(n) + 0.5 * support
pa      <- rnorm(n) + 0.5 * vvg
bmi     <- rnorm(n) + 0.5 * pa
agg     <- rnorm(n) + 0.5 * vvg + 0.5 * pa + 0.5 * sprob + 0.5 * genetic
# ------------------------------------------------------------------------------


# Do some regression modelling -------------------------------------------------
model1 <- lm(agg ~ vvg)  # confounding bias
model2 <- lm(agg ~ vvg + pa + support)  # over-adjustment bias
model3 <- lm(agg ~ vvg + bmi + support) # over-adjustment bias, but less so
model4 <- lm(agg ~ vvg + sprob)  # collider bias
model5 <- lm(agg ~ vvg + support) # unbiased
model6 <- lm(agg ~ vvg + support + sprob) # unbiased
# ------------------------------------------------------------------------------


# Extracting model estimates ---------------------------------------------------
crude <- model1$coefficients[2]
with_sprob <- model4$coefficients[2]
with_support <- model5$coefficients[2]
my_estimates <- c(crude, with_sprob, with_support)  # Combine in a vector
names(my_estimates) <- c('Crude', '+Sprob', '+Support')
# ------------------------------------------------------------------------------


# Plot of model estimates ------------------------------------------------------
plot(c(1, 2, 3), my_estimates, ylim = c(0.5, 1), xaxt = 'n', xlab = 'Model',
     ylab = 'Causal effect estimate')
lines(c(1, 3), c(0.75, 0.75), lty = 2)
axis(1, at = 1:3, labels <- names(my_estimates))
# ------------------------------------------------------------------------------









