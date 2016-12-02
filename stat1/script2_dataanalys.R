# Script2. Analyzing data from a psychoacoustic experiment (under construction) 
#
# The data is from a listening experiment (ALEXIS 106) involving blind and sighted 
# listeners, conducted at the Gösta Ekman Laboratory in 2016. The experiment measured 
# auditory thresholds for abilities potentially important for human echolocation.
# The data is stored in two files. The first contain threshold data, 
# the second background data (age, sex, ...). Please see the file codebooks.txt
# for details on the variables stored in the data files.
#
# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2016-12-01

# To be started at REX101 ...



# Clear memory and set random seed ---------------------------------------------
rm(list = ls()) # clear memory
graphics.off()  # clear all plots
cat("\014")     # clear console (same as Ctrl-L in console)
set.seed(123)   # Makes anayses involving random numbers reproducable
#-------------------------------------------------------------------------------


# Import data ------------------------------------------------------------------
setwd("c:/Users/MATNI/Dropbox/_MN2015/undervisning/rex_website/stat1")
th <- read.delim('threshold_data.txt', header = TRUE, sep = ',')
bg <- read.delim('background_data.txt', header = TRUE, sep = ',')
# ------------------------------------------------------------------------------


# Transform id-numbers so all in group 2 have id starting with 20nn) -----------
bg$group2_1000 <- (bg$id < 2000 & bg$group == 2)
bg$id2 <- bg$group2_1000 * (bg$id + 1000) + (1 - bg$group2_1000) * bg$id

# Check age of matched pairs
blind <- bg[bg$group == 3,]
blind <- blind[order(blind$id),]
match <- bg[bg$group == 2,]
match <- match[order(match$id2),]

age_diff <- blind$age - match$age
# ------------------------------------------------------------------------------


