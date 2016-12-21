# Script2. Analyzing data from a psychoacoustic experiment (under construction) 
#
# The data is from a listening experiment involving blind and sighted 
# listeners, conducted at the Gösta Ekman Laboratory in 2016. The experiment  
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

# Log-transform of d$th.3 (Why? Look for your self: screen_th(d$th.3) ----------
d$log2th.3 <- log2(d$th.3)  # Any log base will do, I like base 2.
# ------------------------------------------------------------------------------


# All mean & CI results in a single list, using lapply -------------------------
thframe <- d[, c(3, 4, 10, 6)]  # Threshold data in order th1, th2, log2th3, th4
mout <- lapply(thframe, mean_table)
# ------------------------------------------------------------------------------


# Regression analysis with age -------------------------------------------------

screen_agereg <- function(th) {
  # Scatter plot and regression of threhold as a function of age. Shows linear  
  # regression line (OLS) and a LOWESS line, separate for blind and age-matched  
  # sighetd listeners.
  # Arguments: 
  #   th -- Numeric vector with threshold data (d$th.1, d$th.2, d$th.3 or d$th.4)
  # Returns: 
  #   mdiff -- Linear regression summaries, and a plot with lines for each group.
  
  plot(d$age[d$group != 'young'], th[d$group != 'young'], pch = '',
       xlab = 'Age', ylab = 'Threshold')  # empty plot
  
  # Add points and lines for sighted age-matched
  points(d$age[d$group == 'match'], th[d$group == 'match'], pch = 21, bg = 'blue')
  m1_match <- lm(th[d$group == 'match'] ~ d$age[d$group == 'match'])
  abline(m1_match, col = 'blue')  # Regression line
  smooth_line_m <- lowess(d$age[d$group == 'match'], th[d$group == 'match'], f = 1/2) 
  lines(smooth_line_m,lty = 2, col = 'blue')  # Add smoother line
  
  # Add points and lines for blind
  points(d$age[d$group == 'blind'], th[d$group == 'blind'], pch = 21, bg = 'green')
  m1_blind <- lm(th[d$group == 'blind'] ~ d$age[d$group == 'blind'])
  abline(m1_blind, col = 'darkgreen')
  smooth_line_b <- lowess(d$age[d$group == 'blind'], th[d$group == 'blind'], f = 1/2) 
  lines(smooth_line_b,lty = 2, col = 'darkgreen')
  
  # Regression coefficients added to upper left corner of the plot
  sstat <- round(c(summary(m1_blind)$coefficients[2, 1], 
                   summary(m1_match)$coefficients[2, 1]), 3)
  tt <- sprintf('Blind: b=%.3f, \nMatch: b=%.3f', sstat[1], sstat[2])
  ytext_location <- quantile(th[d$group != 'young'], probs = 0.90)
  text(30, ytext_location, tt, cex = 0.8)
}


# Pearson and Spearman correlations, to 3 decimals -----------------------------
pearson <- cor(d[, c(8, 3:5, 10, 6)], method = 'pearson')
spearman <- cor(d[, c(8, 3:5, 10, 6)], method = 'spearman')
round(pearson, 3)
round(spearman, 3)
# ------------------------------------------------------------------------------


# To be continued at REX105 ...

boot_mdndiff <- function(g1, g2, nb = 5e3) {
    # Bootstraps (percentile method) differences between group medians, returns
    # observed median-diff with 95% confidence intervals.
    # Arguments: 
    #   g1 -- Numeric vector with data for first group
    #   g2 -- Numeric vector with data for second group
    #   nb -- Numeric vector with number of bootstraps, default = 5000
    # Returns: 
    #   ciboot -- Numeric vector with median and 95% CI around the median
    
  boot_g1 <- replicate(nb, median(sample(g1, length(g1), replace = TRUE)))
  boot_g2 <- replicate(nb, median(sample(g2, length(g2), replace = TRUE)))
  mdn_diff <- boot_g1 - boot_g2
  ci95 <- quantile(mdn_diff, c(0.025, 0.975))
  out <- c(median(g1) - median(g2), ci95)
  names(out) <- c('median-diff', 'ci95lo', 'ci95hi')
  out
  }


mdn_table <- function(x) {
    # Conducts three bootsraps using boot_mdndiff() defined above. 
    # 
    # Arguments: 
    #   x -- Numeric vector with threshold data (d$th.1, d$th.2, d$th.3 or d$th.4)
    # Returns: 
    #   mdntable -- Numeric matrix with median statistics for three between-group  
    #             comparisons: blind-young, blind-agematched, and young-agematched
    
    blind <- x[d$group == 'blind']
    amatch <- x[d$group == 'match']  # "amatch" not to collide with R's match()
    young <- x[d$group == 'young']
    
    mdntable <- matrix(c(boot_mdndiff(blind, young),
                         boot_mdndiff(blind, amatch),
                         boot_mdndiff(young, amatch)),
                     nrow = 3, ncol = 3, byrow = TRUE)
    colnames(mdntable) <- c('mdndiff', 'ci95lo', 'ci95hi')
    rownames(mdntable) <- c('blind-young', 'blind-agematch', 'young-agematch')
    mdntable
  }
  
# Combine results
thframe <- d[, c(3, 4, 10, 6)]  # Threshold data in order th1, th2, log2th3, th4
mdnout <- lapply(thframe, mdn_table)
# ------------------------------------------------------------------------------


# To be continued at REX106 ...

# Fig. 1: Data for th.4 and mdn-diffs with 95 % CIs ---------------------------
 
# Left hand diagram: Boxplots
par(fig = c(0, 0.7, 0, 1), mar = c(4, 4, 2, 2))

# Empty plot
plot(1, axes = FALSE, ann = FALSE, xlim = c(0.5, 3.5), ylim = c(-10, 8), pch = '')

# Add boxplots
boxplot(d$th.4 ~ d$group, at = c(1, 2, 3), axes = FALSE, ann = FALSE,
        col = c('white', 'lightgrey', 'lightgreen'), medlwd = 1, 
        whisklty = 'solid', boxwex = 0.8, staplewex = 0.5, add = TRUE, pch = '')

# Add axis
axis(1, at = c(0, 1, 2, 3, 4), 
     labels = c('', 'Young (Y)', 'Age-matched (A)', 'Blind (B)', ''), 
     tck = .01, cex.axis = 0.6, pos = -10, mgp = c(3, 0, 0))
axis(2, las = 1, at = c(-10, -5, 0, 5, 8), labels = c('-10', '-5', '0', '5', ''),
     tck = .01, cex.axis = 0.6, mgp = c(3, 0.5, 0))

# Add axis labels
mtext(side = 1, text = 'Listener group', line = 1.2, cex = 0.8)
mtext(side = 2, text = 'Threshold [dB re standard]', line = 2, cex = 0.8)

# Right hand diagram: CIs about medians 
par(fig = c(0.7, 1, 0, 1), 
    mar = c(4, 2, 2, 1), new = TRUE)

# Empty plot
plot(1, xlim = c(0, 4), ylim = c(-4, 1), axes = FALSE, ann = FALSE, pch = '')

# Add x-axis
axis(1, at = c(0, 1, 2, 3, 4), labels = c('', 'Y-A', 'B-Y', 'B-A', ''),
     tck = .01, cex.axis = 0.6, pos = -4, mgp = c(3, 0, 0))
axis(2, at = seq(-4, 1), labels = seq(-4, 1), las = 1, 
     tck = .01, cex.axis = 0.6, pos = 0, mgp = c(3, 0.5, 0))

# Add dotted line (lty = 2) at no effect
lines(c(0, 4), c(0,0), lty = 2)

# Add points and CIs for the listener groups
cis <- mdnout$th.4  # The data to be plotted (stored in mdnout)

arrows(1, cis[3, 2], 1, cis[3, 3], angle = 90, code = 3, length = 0.1)
points(1, cis[3, 1], pch = 21, bg = "grey")

arrows(2, cis[1, 2], 2, cis[1, 3], angle = 90, code = 3, length = 0.1)
points(2, cis[1, 1], pch = 21, bg = "grey")

arrows(3, cis[2, 2], 3, cis[2, 3], angle = 90, code = 3, length = 0.1)
points(3, cis[2, 1], pch = 21, bg = "grey")

# Add axis labels
mtext(side = 1, text = 'Group contrast', line = 1.2, cex = 0.8)
mtext(side = 2, text = 'Diffeence between group medians [dB]',
      line = 2, cex = 0.8)

# Add text on error bras
text(2.5, 0.95, labels = 'Error bar: 95 % CI', cex = 0.6, font = 3, pos = 4)
# End Fig 1. -------------------------------------------------------------------

# Show results for differences between group medianas, with 95 % CIs
mdnout  
