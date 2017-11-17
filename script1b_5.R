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
