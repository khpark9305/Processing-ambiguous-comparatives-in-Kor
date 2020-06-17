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
##### Second-pass reading times #####
#####################################

df.second = read.csv("second.time.csv", header=T)
str(df.second)

#############
#### EDA ####
#############

# change Participants and Stimulus as integer
df.second$item <- as.integer(df.second$items)
df.second$subject <- as.integer(df.second$subjects)
str(df.second)

# Summaries of the data
second.mean.SD <- ddply(df.second,c("condition"),
                       summarise,mean = mean(RT),
                       sd = sd(RT),
                       n = length(RT),
                       se = sd/sqrt(n))

second.mean.SD
# condition     mean       sd    n       se
# 1     ma-ma 312.1328 486.3358 4427 7.309396
# 2    ma-nma 236.9221 427.8269 4429 6.428583
# 3    nma-ma 247.9217 409.1647 4436 6.143309
# 4   nma-nma 218.7850 381.7339 4435 5.732103

# Check the normality of the DVs
ggplot(df.second, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.second$RT, ylab="Second Pass Reading times");qqline(df.second$RT, col=2)

############################
#### Model Construction ####
############################

# Rescale IVs
df.second <- transform(df.second, parallel.std=arm::rescale(parallel))
str(df.second)

df.second <- transform(df.second, semantic.std=arm::rescale(semantic))
str(df.second)

df.second.max <- lmer(RT ~ parallel.std*semantic.std + 
                       (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.second)
summary(df.second.max) # Failed to converge

df.second.1 <- lmer(RT ~ parallel.std*semantic.std + 
                        (1+(parallel.std*semantic.std)|item)+(1+(semantic.std)|subject), data = df.second)
summary(df.second.1)

df.second.2 <- lmer(RT ~ parallel.std*semantic.std + 
                      (1+(semantic.std)|item)+(1+(semantic.std)|subject), data = df.second)
summary(df.second.2)

df.second.3 <- lmer(RT ~ parallel.std*semantic.std + 
                      (1+(semantic.std)|item)+(1|subject), data = df.second)
summary(df.second.3)

df.second.4 <- lmer(RT ~ parallel.std*semantic.std + 
                      (1|item)+(1|subject), data = df.second)
summary(df.second.4)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | item) + (1 | subject)
#    Data: df.second
# 
# REML criterion at convergence: 262055.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.4137 -0.5229 -0.2124  0.2326  7.9099 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   3630    60.25  
#  subject  (Intercept)  28890   169.97  
#  Residual             151986   389.85  
# Number of obs: 17727, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error     df t value        Pr(>|t|)    
# (Intercept)                 254.64      29.41  42.82   8.658 0.0000000000587 ***
# parallel.std                 23.12      18.35  44.00   1.260          0.2144    
# semantic.std                -51.93      18.35  44.00  -2.830          0.0070 ** 
# parallel.std:semantic.std   -83.58      36.70  44.00  -2.277          0.0277 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std 0.000               
# semantc.std 0.000  0.000        
# prlll.std:. 0.000  0.000  0.000 