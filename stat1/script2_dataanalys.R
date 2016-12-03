# Script2. Analyzing data from a psychoacoustic experiment (under construction) 
#
# The data is from a listening experiment involving blind and sighted 
# listeners, conducted at the Gösta Ekman Laboratory in 2016. The experiment measured 
# auditory thresholds for abilities potentially used in human echolocation.
# The data is stored in two files. The first contain threshold data, 
# the second background data (age, sex, ...). Please see the file codebooks.txt
# for details on the variables stored in the data files.
#
# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2016-12-02

# To be started at REX101 ...


# Clear memory and set random seed ---------------------------------------------
rm(list = ls()) # clear memory
graphics.off()  # clear all plots
cat("\014")     # clear console (same as Ctrl-L in console)
set.seed(123)   # Makes it reproducable also if random number generators are used
#-------------------------------------------------------------------------------


# Import data ------------------------------------------------------------------
setwd("c:/Users/MATNI/Dropbox/_MN2015/undervisning/rex_website/stat1")
th_long <- read.table('threshold_data.txt', header = TRUE, sep = ',')
bg <- read.table('background_data.txt', header = TRUE, sep = ',')
# ------------------------------------------------------------------------------


# Create factor of group variable ----------------------------------------------
th_long$group <- factor(th_long$group, levels = c(1, 2, 3), 
                   labels = c('young', 'match', 'blind'))
bg$group <- factor(bg$group, levels = c(1, 2, 3), 
                   labels = c('young', 'match', 'blind'))
# ------------------------------------------------------------------------------


# From long to wide ------------------------------------------------------------
th_wide <- reshape(th_long[, -4], v.names = 'th', timevar = "cond", 
               idvar = "id", direction = "wide")
# ------------------------------------------------------------------------------


# Merge to new data frame, d ---------------------------------------------------
bg$id2 <- bg$id  # Add id-variable as a check
# This merges th and part of bg into a new data frame called d
d <- merge(th_wide, bg[, c(1:3, 20)], by = 'id')  
d$id - d$id2  # This is the check: Should be all zeros
bg$id2 <- NULL  # Delete bg$id2
d$id2 <- NULL  # Delete d$id2
# ------------------------------------------------------------------------------





# ------------------------------------------------------------------------------
plot(d$age, d$th.1)
model1 <- lm(d$th.1 ~ d$age)
abline(model1)
summary(model1)

plot(d$age, d$th.2)
model2 <- lm(d$th.2 ~ d$age)
abline(model2)
summary(model2)

plot(d$age, d$th.3)
model3 <- lm(d$th.3 ~ d$age)
abline(model3)
summary(model3)

plot(d$age, log2(d$th.3))
model3log <- lm(log2(d$th.3) ~ d$age)
abline(model3log)
summary(model3log)
lines(lowess(log2(d$th.3) ~ d$age, f = 2/3), col = "black", lty = 2)

plot(d$age, d$th.4)
model4 <- lm(d$th.4 ~ d$age)
abline(model4)
summary(model4)
# ------------------------------------------------------------------------------


# Absolute threshold in best ear -----------------------------------------------
bg$b250 <- apply(cbind(bg$right250, bg$left250), 1, max, na.rm = TRUE)
bg$b500 <- apply(cbind(bg$right500, bg$left500), 1, max, na.rm = TRUE)
bg$b1000 <- apply(cbind(bg$right1000, bg$left1000), 1, max, na.rm = TRUE)
bg$b2000 <- apply(cbind(bg$right2000, bg$left2000), 1, max, na.rm = TRUE)
bg$b3000 <- apply(cbind(bg$right3000, bg$left3000), 1, max, na.rm = TRUE)
bg$b4000 <- apply(cbind(bg$right4000, bg$left4000), 1, max, na.rm = TRUE)
bg$b6000 <- apply(cbind(bg$right6000, bg$left6000), 1, max, na.rm = TRUE)
bg$b6000[bg$b6000 == -Inf] <- NA  # Missing data in both ears, set to NA
bg$PTmean <- (bg$b250 + bg$b500 + bg$b1000 + bg$b2000 + 
             bg$b2000 + bg$b4000 + bg$b6000) / 7
boxplot(bg$PTmean ~ bg$group)
aggregate(bg$PTmean, by = list(bg$group, bg$pairs), mean)
# ------------------------------------------------------------------------------


# Create variable with matched-pair numbers ------------------------------------
bg$pairs <- as.character(bg$id)
bg$pairs <- substr(bg$pairs, 3, 4)  # Keep two last digits only
bg$pairs <- as.numeric(bg$pairs)
bg$pairs[bg$group == 'young'] <- 0  # set non-matched individuals to 0
table(bg$pairs)  # Check that it worked
# ------------------------------------------------------------------------------


# Check age of matched pairs ---------------------------------------------------
median(bg$age[bg$pairs > 0 & bg$group == 3])
median(bg$age[bg$pairs > 0 & bg$group == 2])
# ------------------------------------------------------------------------------


