######################################
##### Kihyo's MA thesis analysis #####
######################################

## REMOVE ALL ##
rm(list=ls())

setwd("C:/R/comparatives/clause")
options(scipen=999)

library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lme4)
library(lmerTest)
library(arm)

#####################################
##### First-pass reading times ######
#####################################

df.first = read.csv("first.time.csv", header=T)
str(df.first)

#############
#### EDA ####
#############

# change Participants and Stimulus as integer
df.first$item <- as.integer(df.first$items)
df.first$subject <- as.integer(df.first$subjects)
str(df.first)

# Summaries of the data
first.mean.SD <- ddply(df.first,c("condition"),
                       summarise,mean = mean(RT),
                       sd = sd(RT),
                       n = length(RT),
                       se = sd/sqrt(n))

first.mean.SD
# condition     mean       sd   n       se
# 1     ma-ma 1151.269 625.4215 420 30.51744
# 2    ma-nma 1092.383 589.3918 441 28.06628
# 3    nma-ma 1194.834 654.3568 440 31.19524
# 4   nma-nma 1026.607 568.4562 433 27.31826

# Check the normality of the DVs
ggplot(df.first, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.first$RT, ylab="First Pass Reading times");qqline(df.first$RT, col=2)

############################
#### Model Construction ####
############################

# Rescale IVs
df.first <- transform(df.first, parallel.std=arm::rescale(parallel))
str(df.first)

df.first <- transform(df.first, semantic.std=arm::rescale(semantic))
str(df.first)

df.first.max <- lmer(RT ~ parallel.std*semantic.std + 
                       (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.first)
summary(df.first.max)

df.first.1 <- lmer(RT ~ parallel.std*semantic.std + 
                       (1+(parallel.std*semantic.std)|item)+(1+(semantic.std)|subject), data = df.first)
summary(df.first.1)

df.first.2 <- lmer(RT ~ parallel.std*semantic.std + 
                     (1+(parallel.std*semantic.std)|item)+(1|subject), data = df.first)
summary(df.first.2)

df.first.3 <- lmer(RT ~ parallel.std*semantic.std + 
                     (1+(semantic.std)|item)+(1|subject), data = df.first)
summary(df.first.3)

df.first.4 <- lmer(RT ~ parallel.std*semantic.std + 
                     (1|item)+(1|subject), data = df.first)
summary(df.first.4)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | item) + (1 | subject)
#    Data: df.first
# 
# REML criterion at convergence: 26786.9
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.8091 -0.5825 -0.0911  0.5486  4.1858 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  14914   122.1   
#  subject  (Intercept)  81478   285.4   
#  Residual             281811   530.9   
# Number of obs: 1734, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error      df t value            Pr(>|t|)    
# (Intercept)                1115.11      51.73   44.41  21.555 <0.0000000000000002 ***
# parallel.std                -56.74      43.55   43.91  -1.303              0.1995    
# semantic.std               -112.47      43.55   43.90  -2.582              0.0132 *  
# parallel.std:semantic.std   -13.03      87.11   43.91  -0.150              0.8818    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std -0.004              
# semantc.std  0.002 -0.004       
# prlll.std:. -0.002  0.003 -0.009