# Script2. Analyzing data from a psychoacoustic experiment (under construction) 
#
# The data is from a listening experiment involving blind and sighted 
# listeners, conducted at the G�sta Ekman Laboratory in 2016. The experiment  
# measured auditory thresholds for abilities potentially used in human 
# echolocation. The data is stored in two files. The first contain threshold data, 
# the second background data (age, sex, ...). Please see the file codebooks.txt
# for details on the variables stored in the data files.
#
# Mats Nilsson (mats.nilsson@psychology.su.se), revised: 2016-12-10

# To be started at REX101 ...

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


# From long to wide format -----------------------------------------------------
th_wide <- reshape(th_long[, -4], v.names = 'th', timevar = "cond", 
                   idvar = "id", direction = "wide")
# ------------------------------------------------------------------------------


# Merge to new data frame, d ---------------------------------------------------
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
d$pairs[d$group == 'young'] <- 0  # Set non-matched individuals to 0
table(d$pairs)  # Check that it worked
# ------------------------------------------------------------------------------

# To be continued at REX102 ...

# Stats and plot function for threshold data -----------------------------------
screen_th <- function(x) { 
  # Produces summary stats and plots for the threshold data, separately for 
  # each listener group
  # Arguments: 
  #   x -- Numeric vector with threshold data (d$th.1, d$th.2, d$th.3 or d$th.4)
  # Returns: 
  #   Various stats and plots
  
  # Five-numenr summaries and means
  summary(x)  # WHole data set
  aggregate(x, list(d$group), summary)  # For each listener group
  
  # Dotchart
  dotchart(x, labels = d$id, cex = 0.7, groups = d$group)
  mdn <- median(x)
  m <- mean(x)
  lines(c(m, m), c(0, 98), col = 'grey')  # Grey line vertical line at the mean
  lines(c(mdn, mdn), c(0, 98), col = 'black')  # Black vertical line at the median
  
  # Bokplot
  boxplot(x ~ d$group)
  
  # Histogram (all data), with kernel density plots (per group) 
  hist(x, breaks = 25, freq = FALSE, main = '')
  
  blind <- x[d$group == 'blind']
  amatch <- x[d$group == 'match']  # "amatch" not to collide with R's match()
  young <- x[d$group == 'young']
  
  polygon(density(blind), col = rgb(0, 1, 0, 0.3))  # Green for blind
  polygon(density(young), col = rgb(1, 0, 0, 0.3))  # Red for young
  polygon(density(amatch), col = rgb(0, 0, 1, 0.3))  # Blue for amatch
  
  # Percentile plot
  plot(ecdf(blind), pch = '', verticals = TRUE, col = 'green', main = '')
  plot(ecdf(young), pch = '', verticals = TRUE, add = TRUE, col = 'red')
  plot(ecdf(amatch), pch = '', verticals = TRUE, add = TRUE, col = 'blue')
}
# ------------------------------------------------------------------------------

# To be continued at REX103 ...

# Group comparison functions: means, 95 % CI's, t-tests for threshold data -----

my_ttest <- function(g1, g2) {
  # Conducts an independent samples t-test (Welch's method), and returns selected
  # output.
  # Arguments: 
  #   g1 -- Numeric vector with data for first group
  #   g2 -- Numeric vector with data for second group
  # Returns: 
  #   mdiff -- Numeric vector with mean-diff, 95% CI, and p-value
  
  tout <- t.test(g1, g2)
  mdiff <- c(mean(g1) - mean(g2), tout$conf.int, tout$p.value)
  names(mdiff) <- c('meandiff', 'ci95lo', 'ci95hi', 'p_welch')
  mdiff
}

mean_table <- function(x) {
  # Conducts three independent samples t-tests (Welch's method) using my_ttest()
  # defined above. 
  # Arguments: 
  #   x -- Numeric vector with threshold data (d$th.1, d$th.2, d$th.3 or d$th.4)
  # Returns: 
  #   mtable -- Numeric matrix with statistics for three between-group  
  #             comparisons: blind-young, blind-agematched, and young-agematched
  
  blind <- x[d$group == 'blind']
  amatch <- x[d$group == 'match']  # "amatch" not to collide with R's match()
  young <- x[d$group == 'young']
  mtable <- matrix(c(my_ttest(blind, young),
                     my_ttest(blind, amatch),
                     my_ttest(young, amatch)),
                   nrow = 3, ncol = 4, byrow = TRUE)
  colnames(mtable) <- c('meandiff', 'ci95lo', 'ci95hi', 'p_welch')
  rownames(mtable) <- c('blind-young', 'blind-agematch', 'young-agematch')
  mtable
}
# ------------------------------------------------------------------------------

# To be continued at REX104 ...
