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
##### Total reading times ###########
#####################################

# read .csv files
df.total = read.csv("total.time.csv", header=T)
str(df.total)

#############
#### EDA ####
#############

# change Participants and Stimulus as integer
df.total$item <- as.integer(df.total$items)
df.total$subject <- as.integer(df.total$subjects)
str(df.total)

# Summaries of the data
total.mean.SD <- ddply(df.total,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))

total.mean.SD
# condition     mean       sd   n       se
# 1     ma-ma 1550.985 712.4706 362 37.44662
# 2    ma-nma 1280.588 663.3623 412 32.68151
# 3    nma-ma 1527.999 679.8476 384 34.69333
# 4   nma-nma 1350.991 678.9794 403 33.82237

# Check the normality of the DVs
ggplot(df.total, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.total$RT, ylab="Total Reading times");qqline(df.total$RT, col=2)

############################
#### Model Construction ####
############################

# Rescale IVs
df.total <- transform(df.total, parallel.std=arm::rescale(parallel))
str(df.total)

df.total <- transform(df.total, semantic.std=arm::rescale(semantic))
str(df.total)

df.total.max <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.total)
summary(df.total.max) # Failed to converge

df.total.1 <- lmer(RT ~ parallel.std*semantic.std + 
                       (1+(parallel.std*semantic.std)|item)+(1+(parallel.std)+(semantic.std)|subject), data = df.total)
summary(df.total.1.)

df.total.2 <- lmer(RT ~ parallel.std*semantic.std + 
                       (1+(parallel.std*semantic.std)|item)+(1+(semantic.std)|subject), data = df.total)
summary(df.total.2)

df.total.3 <- lmer(RT ~ parallel.std*semantic.std + 
                     (1+(parallel.std*semantic.std)|item)+(1|subject), data = df.total)
summary(df.total.3)

df.total.4 <- lmer(RT ~ parallel.std*semantic.std + 
                     (1+(semantic.std)|item)+(1|subject), data = df.total)
summary(df.total.4)

df.total.5 <- lmer(RT ~ parallel.std*semantic.std + 
                     (1|item)+(1|subject), data = df.total)
summary(df.total.5)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | item) + (1 | subject)
#    Data: df.total
# 
# REML criterion at convergence: 24175.9
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.9283 -0.6592 -0.1196  0.5368  3.4464 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  22936   151.4   
#  subject  (Intercept) 177175   420.9   
#  Residual             285437   534.3   
# Number of obs: 1561, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error      df t value             Pr(>|t|)    
# (Intercept)                1446.96      73.87   42.35  19.588 < 0.0000000000000002 ***
# parallel.std                 45.13      51.58   42.31   0.875                0.387    
# semantic.std               -230.19      51.57   42.41  -4.463            0.0000586 ***
# parallel.std:semantic.std    26.26     103.11   42.39   0.255                0.800    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std -0.004              
# semantc.std  0.010 -0.005       
# prlll.std:. -0.002  0.029 -0.013
