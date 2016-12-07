# Script2. Analyzing data from a psychoacoustic experiment (under construction) 
#
# The data is from a listening experiment involving blind and sighted 
# listeners, conducted at the Gösta Ekman Laboratory in 2016. The experiment measured 
# auditory thresholds for abilities potentially used in human echolocation.
# The data is stored in two files. The first contain threshold data, 
# the second background data (age, sex, ...). Please see the file codebooks.txt
# for details on the variables stored in the data files.
#
# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2016-12-07


# Clear memory and set random seed ---------------------------------------------
rm(list = ls()) # clear memory
graphics.off()  # clear all plots
cat("\014")     # clear console (same as Ctrl-L in console)
set.seed(123)   # Makes it reproducable also if random number generators are used
#-------------------------------------------------------------------------------

# Import data ------------------------------------------------------------------
setwd('c:/Users/MATNI/Dropbox/_MN2015/undervisning/rex_website/stat1')
th_long <- read.table('threshold_data.txt', header = TRUE, sep = ',')
bg <- read.table('background_data.txt', header = TRUE, sep = ',')
# ------------------------------------------------------------------------------


# Threshold data from long format to wide format -------------------------------
th_wide <- reshape(th_long[, -4], v.names = 'th', timevar = "cond", 
               idvar = "id", direction = "wide")
# ------------------------------------------------------------------------------


# Merge two data sets into one new, called d -----------------------------------
bg$id2 <- bg$id  # Add id-variable as a security check
# This merges th and (part of) bg into a new data frame called d
d <- merge(th_wide, bg[, c(1:3, 20)], by = 'id')  
d$id - d$id2  # This is the check: Should be all zeros
d$id2 <- NULL  # Delete d$id2 (not necessary)
rm(bg, th_long, th_wide)  # Remove data frame from memory (not necessary)
# ------------------------------------------------------------------------------


# Create some factor variables -------------------------------------------------
d$group <- factor(d$group, levels = c(1, 2, 3), 
                        labels = c('young', 'match', 'blind'))
d$sex <- factor(d$sex, levels = c(1, 2), 
                 labels = c('female', 'male'))
# ------------------------------------------------------------------------------


# Create variable with matched-pair numbers ------------------------------------
d$pairs <- as.character(d$id)
d$pairs <- substr(d$pairs, 3, 4)  # Keep two last digits only
d$pairs <- as.numeric(d$pairs)
d$pairs[d$group == 'young'] <- 0  # set non-matched individuals to 0
table(d$pairs)  # Check that it worked
# ------------------------------------------------------------------------------

# To be continued at REX102 ...

