######################################
##### Kihyo's MA thesis analysis #####
######################################

## REMOVE ALL ##
rm(list=ls())

setwd("C:/R/comparatives")
options(scipen=999)

library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lme4)
library(lmerTest)
library(arm)

####################################
##### First-pass reading times #####
####################################

# read .csv files
df.first.R1 = read.csv("first.time.R1.csv", header=T)
df.first.R2 = read.csv("first.time.R2.csv", header=T)
df.first.R3 = read.csv("first.time.R3.csv", header=T)
df.first.R4 = read.csv("first.time.R4.csv", header=T)
df.first.R5 = read.csv("first.time.R5.csv", header=T)
df.first.R6 = read.csv("first.time.R6.csv", header=T)
df.first.R7 = read.csv("first.time.R7.csv", header=T)
df.first.R8 = read.csv("first.time.R8.csv", header=T)

#############
#### EDA ####
#############

# change Participants and Stimulus as integer
df.first.R1$item <- as.integer(df.first.R1$Stimulus)
df.first.R1$subject <- as.integer(df.first.R1$Participant)
str(df.first.R1)

df.first.R2$item <- as.integer(df.first.R2$Stimulus)
df.first.R2$subject <- as.integer(df.first.R2$Participant)
str(df.first.R2)

df.first.R3$item <- as.integer(df.first.R3$Stimulus)
df.first.R3$subject <- as.integer(df.first.R3$Participant)
str(df.first.R3)

df.first.R4$item <- as.integer(df.first.R4$Stimulus)
df.first.R4$subject <- as.integer(df.first.R4$Participant)
str(df.first.R4)

df.first.R5$item <- as.integer(df.first.R5$Stimulus)
df.first.R5$subject <- as.integer(df.first.R5$Participant)
str(df.first.R5)

df.first.R6$item <- as.integer(df.first.R6$Stimulus)
df.first.R6$subject <- as.integer(df.first.R6$Participant)
str(df.first.R6)

df.first.R7$item <- as.integer(df.first.R7$Stimulus)
df.first.R7$subject <- as.integer(df.first.R7$Participant)
str(df.first.R7)

df.first.R8$item <- as.integer(df.first.R8$Stimulus)
df.first.R8$subject <- as.integer(df.first.R8$Participant)
str(df.first.R8)

# Summaries of the data
mean.SD.R1 <- ddply(df.first.R1,c("condition"),
                    summarise,mean = mean(RT),
                                sd = sd(RT),
                                 n = length(RT),
                                se = sd/sqrt(n))
mean.SD.R1
#   condition      mean       sd   n       se
# 1     ma-ma  210.4032 125.2016 444 5.941804
# 2    ma-nma  220.1306 130.1463 444 6.176472
# 3    nma-ma  211.7427 131.2071 443 6.233839
# 4   nma-nma  212.1441 141.2331 444 6.702626

mean.SD.R1.semantic <- ddply(df.first.R1,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R1.semantic
#   semantic     mean       sd   n       se
# 1        1 215.2669 127.7186 888 4.285957
# 2        2 211.9436 136.2412 887 4.574531

mean.SD.R2 <- ddply(df.first.R2,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R2
#   condition     mean       sd   n       se
# 1     ma-ma  273.6753 174.8612 425   8.482013
# 2    ma-nma  255.6290 157.8421 442   7.507785
# 3    nma-ma  260.1869 171.0118 444   8.115863
# 4   nma-nma  251.9252 155.0294 441   7.382350

mean.SD.R2.semantic <- ddply(df.first.R2,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R2.semantic
#   semantic     mean       sd   n       se
# 1        1 264.4752 166.5508 867 5.656360
# 2        2 256.0701 163.2035 885 5.486024

mean.SD.R3 <- ddply(df.first.R3,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R3
#   condition     mean       sd   n       se
# 1     ma-ma  232.2280 135.60753 443  6.442908
# 2    ma-nma  265.3657 164.18275 443  7.800558
# 3    nma-ma  197.1599 127.29301 444  6.041059
# 4   nma-nma  197.5000 134.75644 444  6.395258

mean.SD.R3.semantic <- ddply(df.first.R3,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R3.semantic
#   semantic     mean       sd   n       se
# 1        1 248.7968 151.3998 886 5.086377
# 2        2 197.3300 131.0041 888 4.396208

mean.SD.R4 <- ddply(df.first.R4,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R4
#   condition     mean       sd   n       se
# 1     ma-ma 152.4910 136.6165 444 6.483534
# 2    ma-nma 136.2410 115.9768 444 5.504014
# 3    nma-ma 168.1081 140.6637 444 6.675605
# 4   nma-nma 166.6171 132.8857 444 6.306477

mean.SD.R4.semantic <- ddply(df.first.R4,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R4.semantic
#   semantic     mean       sd   n       se
# 1        1 144.3660 126.9068 888 4.258713
# 2        2 167.3626 136.7549 888 4.589193

mean.SD.R5 <- ddply(df.first.R5,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R5
#   condition     mean       sd   n       se
# 1     ma-ma  277.4070 188.7872 457   8.831091
# 2    ma-nma  279.8258 176.7548 442   8.407370
# 3    nma-ma  261.3469 160.8028 441   7.657276
# 4   nma-nma  250.5711 163.5325 443   7.769664

mean.SD.R5.semantic <- ddply(df.first.R5,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R5.semantic
# semantic     mean       sd   n       se
# 1        1 269.5200 175.6888 898 5.862813
# 2        2 265.1819 170.7965 885 5.741261

mean.SD.R6 <- ddply(df.first.R6,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R6
#   condition     mean       sd   n       se
# 1     ma-ma  286.5405 177.1345 444   8.406430
# 2    ma-nma  263.2957 152.1432 443   7.228539
# 3    nma-ma  287.1697 173.8787 442   8.270565
# 4   nma-nma  277.4027 174.9642 442   8.322197

mean.SD.R6.semantic <- ddply(df.first.R6,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R6.semantic
#   semantic     mean       sd   n       se
# 1        1 286.8544 175.4189 886 5.893313
# 2        2 270.3412 163.9975 885 5.512715

mean.SD.R7 <- ddply(df.first.R7,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R7
#   condition     mean       sd   n       se
# 1     ma-ma  238.9257 155.5814 444 7.383568
# 2    ma-nma  219.1580 160.1228 443 7.607664
# 3    nma-ma  238.2523 151.6972 444 7.199232
# 4   nma-nma  207.7095 152.3064 444 7.228143

mean.SD.R7.semantic <- ddply(df.first.R7,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R7.semantic
#   semantic     mean       sd   n       se
# 1        1 238.5890 153.5653 888 5.153315
# 2        2 213.4273 156.2759 887 5.247230

mean.SD.R8 <- ddply(df.first.R8,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R8
#   condition     mean       sd   n       se
# 1     ma-ma 26.09009 75.34490 444 3.575711
# 2    ma-nma 19.25901 65.26562 444 3.097369
# 3    nma-ma 21.95721 74.89930 444 3.554564
# 4   nma-nma 24.01802 69.96135 444 3.320219

mean.SD.R8.semantic <- ddply(df.first.R8,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R8.semantic
#   semantic     mean       sd   n       se
# 1        1 24.02365 75.10854 888 2.520478
# 2        2 21.63851 67.65799 888 2.270454


# Check the normality of the DVs
ggplot(df.first.R1, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.first.R1$RT, ylab="first Reading times");qqline(df.first.R1$RT, col=2)

ggplot(df.first.R2, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.first.R2$RT, ylab="first Reading times");qqline(df.first.R2$RT, col=2)

ggplot(df.first.R3, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.first.R3$RT, ylab="first Reading times");qqline(df.first.R3$RT, col=2)

ggplot(df.first.R4, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.first.R4$RT, ylab="first Reading times");qqline(df.first.R4$RT, col=2)

ggplot(df.first.R5, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.first.R5$RT, ylab="first Reading times");qqline(df.first.R5$RT, col=2)

ggplot(df.first.R6, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.first.R6$RT, ylab="first Reading times");qqline(df.first.R6$RT, col=2)

ggplot(df.first.R7, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.first.R7$RT, ylab="first Reading times");qqline(df.first.R7$RT, col=2)

ggplot(df.first.R8, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.first.R8$RT, ylab="first Reading times");qqline(df.first.R8$RT, col=2)
# All of the regions have positively skewed distribution, therby needing to transform DV into logarithmic scale.

############################
#### Model Construction ####
############################

# Tranform DV into logarithmic scale
# df.first.R1$logRT <- log(df.first.R1$RT)
# df.first.R2$logRT <- log(df.first.R2$RT)
# df.first.R3$logRT <- log(df.first.R3$RT)
# df.first.R4$logRT <- log(df.first.R4$RT)
# df.first.R5$logRT <- log(df.first.R5$RT)
# df.first.R6$logRT <- log(df.first.R6$RT)
# df.first.R7$logRT <- log(df.first.R7$RT)
# df.first.R8$logRT <- log(df.first.R8$RT)
# You can't build models with these since there are N/A values.

# Rescale IVs
df.first.R1 <- transform(df.first.R1, semantic.std=arm::rescale(semantic))
df.first.R2 <- transform(df.first.R2, semantic.std=arm::rescale(semantic))
df.first.R3 <- transform(df.first.R3, semantic.std=arm::rescale(semantic))
df.first.R4 <- transform(df.first.R4, semantic.std=arm::rescale(semantic))

df.first.R5 <- transform(df.first.R5, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
df.first.R6 <- transform(df.first.R6, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
df.first.R7 <- transform(df.first.R7, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
df.first.R8 <- transform(df.first.R8, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))

################
### Region 1 ###
################

# Maximum model
first.R1.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.first.R1)
summary(first.R1.mod.max) # Failed to converge

first.R1.mod.1 <- lmer(RT ~ semantic.std +
                           (1|item) + (1|subject), data = df.first.R1)

summary(first.R1.mod.1)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.first.R1
# 
# REML criterion at convergence: 21926.3
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.7202 -0.5422 -0.0692  0.3803  7.2206 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)    59.54   7.716 
#  subject  (Intercept)  4690.75  68.489 
#  Residual             12807.69 113.171 
# Number of obs: 1775, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error      df t value            Pr(>|t|)    
# (Intercept)   213.615     11.629  36.556  18.369 <0.0000000000000002 ***
# semantic.std   -3.306      5.816  46.044  -0.568               0.573    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.000 

################
### Region 2 ###
################

# Maximum model
first.R2.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.first.R2)
summary(first.R2.mod.max) # Failed to converge

first.R2.mod.1 <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1|subject), data = df.first.R2)
summary(first.R2.mod.1) # Failed to converge

first.R2.mod.2 <- lmer(RT ~ semantic.std +
                         (1|item) + (1|subject), data = df.first.R2)

summary(first.R2.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.first.R2
# 
# REML criterion at convergence: 22647.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.3140 -0.6179 -0.1841  0.3813  5.4915 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   634.4   25.19  
#  subject  (Intercept)  3966.9   62.98  
#  Residual             22727.4  150.76  
# Number of obs: 1752, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error      df t value            Pr(>|t|)    
# (Intercept)   260.781     11.552  42.644  22.575 <0.0000000000000002 ***
# semantic.std   -9.088     10.244  42.814  -0.887                0.38    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.002 

################
### Region 3 ###
################

# Maximum model
first.R3.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.first.R3)
summary(first.R3.mod.max) # Failed to converge

first.R3.mod.1 <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1|subject), data = df.first.R3)
summary(first.R3.mod.1) # Faild to converge

first.R3.mod.2 <- lmer(RT ~ semantic.std +
                         (1|item) + (1|subject), data = df.first.R3)

summary(first.R3.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.first.R3
# 
# REML criterion at convergence: 22370.8
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.6515 -0.5788 -0.1325  0.4352  5.4797 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   615.3   24.81  
#  subject  (Intercept)  3048.3   55.21  
#  Residual             16482.2  128.38  
# Number of obs: 1774, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error      df t value             Pr(>|t|)    
# (Intercept)   223.119     10.222  44.717  21.826 < 0.0000000000000002 ***
# semantic.std  -51.636      9.404  46.008  -5.491           0.00000167 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.000 

################
### Region 4 ###
################

#Maximum model
first.R4.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.first.R4)
summary(first.R4.mod.max) # Failed to converge

first.R4.mod.1 <- lmer(RT ~ semantic.std +
                           (1|item) + (1+(semantic.std)|subject), data = df.first.R4)
summary(first.R4.mod.1) # Failed to converge

first.R4.mod.2 <- lmer(RT ~ semantic.std +
                         (1|item) + (1|subject), data = df.first.R4)

summary(first.R4.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.first.R4
# 
# REML criterion at convergence: 22209
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.7649 -0.6895 -0.1106  0.4475  6.5820 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   145.8   12.08  
#  subject  (Intercept)  2204.7   46.95  
#  Residual             15116.2  122.95  
# Number of obs: 1776, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error      df t value             Pr(>|t|)    
# (Intercept)   155.864      8.434  38.406  18.480 < 0.0000000000000002 ***
# semantic.std   22.997      6.797  46.000   3.383              0.00147 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.000 

################
### Region 5 ###
################

# Maximum Model
first.R5.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.first.R5)

summary(first.R5.mod.max)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 + (parallel.std * semantic.std) |  
#     item) + (1 + (parallel.std * semantic.std) | subject)
#    Data: df.first.R5
# 
# REML criterion at convergence: 23970.2
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.6154 -0.5354 -0.1157  0.3342 15.6069 
# 
# Random effects:
#  Groups   Name                      Variance Std.Dev. Corr             
#  item     (Intercept)                 151.77  12.319                   
#           parallel.std               2198.78  46.891   0.96            
#           semantic.std               1855.94  43.081  -0.96 -1.00      
#           parallel.std:semantic.std  3053.80  55.261  -0.99 -0.95  0.95
#  subject  (Intercept)                6655.32  81.580                   
#           parallel.std                 95.54   9.774  -1.00            
#           semantic.std                 83.21   9.122   1.00 -1.00      
#           parallel.std:semantic.std   135.02  11.620   1.00 -1.00  1.00
#  Residual                           35569.37 188.598                   
# Number of obs: 1794, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error      df t value            Pr(>|t|)    
# (Intercept)                273.707     15.116  43.515  18.108 <0.0000000000000002 ***
# parallel.std                -7.647     13.969  27.087  -0.547              0.5886    
# semantic.std               -10.329     13.957  27.071  -0.740              0.4656    
# parallel.std:semantic.std  -47.929     27.686  27.207  -1.731              0.0948 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std  0.122              
# semantc.std -0.130 -0.585       
# prlll.std:. -0.206 -0.487  0.485
# convergence code: 0
# boundary (singular) fit: see ?isSingular

################
### Region 6 ###
################

first.R6.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.first.R6)

summary(first.R6.mod.max)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 + (parallel.std * semantic.std) |  
#     item) + (1 + (parallel.std * semantic.std) | subject)
#    Data: df.first.R6
# 
# REML criterion at convergence: 23448.5
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.8489 -0.6071 -0.2182  0.3796  9.6942 
# 
# Random effects:
#  Groups   Name                      Variance Std.Dev. Corr             
#  item     (Intercept)                 233.91  15.294                   
#           parallel.std                 59.72   7.728  0.80             
#           semantic.std                923.16  30.384  1.00  0.80       
#           parallel.std:semantic.std   248.13  15.752  0.81  1.00  0.81 
#  subject  (Intercept)                3157.07  56.188                   
#           parallel.std                 39.60   6.293   0.70            
#           semantic.std                271.15  16.466  -0.55  0.21      
#           parallel.std:semantic.std   546.71  23.382  -0.34 -0.91 -0.60
#  Residual                           30727.43 175.292                   
# Number of obs: 1776, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error      df t value            Pr(>|t|)    
# (Intercept)                282.489     10.627  41.199  26.581 <0.0000000000000002 ***
# parallel.std                 3.626     10.561  43.652   0.343               0.733    
# semantic.std               -15.917     10.853  38.791  -1.467               0.151    
# parallel.std:semantic.std   22.878     21.369  40.068   1.071               0.291    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std 0.130               
# semantc.std 0.059  0.144        
# prlll.std:. 0.017  0.350  0.110 
# convergence code: 0
# boundary (singular) fit: see ?isSingular

##################################################
###### Reanalysis w.o the factor 'semantic' ######
##################################################

first.R6.mod.max <- lmer(RT ~ parallel.std + 
                           (1+(parallel.std)|item)+(1+(parallel.std)|subject), data = df.first.R6)

summary(first.R6.mod.max) # Failed to converge

first.R6.mod.1 <- lmer(RT ~ parallel.std + 
                           (1+(parallel.std)|item)+(1|subject), data = df.first.R6)

summary(first.R6.mod.1) # Failed to converge

first.R6.mod.2 <- lmer(RT ~ parallel.std + 
                         (1|item)+(1|subject), data = df.first.R6)

summary(first.R6.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std + (1 | item) + (1 | subject)
#    Data: df.first.R6
# 
# REML criterion at convergence: 23071.2
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.0680 -0.6387 -0.2220  0.4252  4.9267 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   423.6   20.58  
#  subject  (Intercept)  3097.9   55.66  
#  Residual             25460.3  159.56  
# Number of obs: 1771, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error      df t value            Pr(>|t|)    
# (Intercept)   278.618     10.341  40.991  26.944 <0.0000000000000002 ***
# parallel.std    6.705      9.634  45.913   0.696                0.49    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# paralll.std 0.000 


################
### Region 7 ###
################

first.R7.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.first.R7)
summary(first.R7.mod.max) # Failed to converge

# Keep build models that will be converged,
first.R7.mod.1 <- lmer(RT ~ parallel.std*semantic.std + (1+(parallel.std*semantic.std)|item)+(1+(semantic.std)|subject), data = df.first.R7)
summary(first.R7.mod.1) # Failed to converge

first.R7.mod.2 <- lmer(RT ~ parallel.std*semantic.std + (1+(parallel.std)|item)+(1+(semantic.std)|subject), data = df.first.R7)
summary(first.R7.mod.2) # Failed to converge

first.R7.mod.3 <- lmer(RT ~ parallel.std*semantic.std + (1+(parallel.std)|item)+(1|subject), data = df.first.R7)
summary(first.R7.mod.3) # Failed to converge

first.R7.mod.4 <- lmer(RT ~ parallel.std*semantic.std + (1|item)+(1|subject), data = df.first.R7)

summary(first.R7.mod.4)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | item) + (1 | subject)
#    Data: df.first.R7
# 
# REML criterion at convergence: 22717.8
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.5622 -0.5800 -0.0992  0.4469  9.6686 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   569.1   23.86  
#  subject  (Intercept)  5008.6   70.77  
#  Residual             19902.9  141.08  
# Number of obs: 1776, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error      df t value            Pr(>|t|)    
# (Intercept)                226.870     12.587  41.220   18.02 <0.0000000000000002 ***
# parallel.std                -7.106      9.605  44.000   -0.74              0.4633    
# semantic.std               -23.437      9.605  44.000   -2.44              0.0188 *  
# parallel.std:semantic.std  -15.559     19.210  44.000   -0.81              0.4223    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std 0.000               
# semantc.std 0.000  0.000        
# prlll.std:. 0.000  0.000  0.000 

##################################################
###### Reanalysis w.o the factor 'semantic' ######
##################################################

first.R7.mod.max <- lmer(RT ~ parallel.std + 
                           (1+(parallel.std)|item)+(1+(parallel.std)|subject), data = df.first.R7)
summary(first.R7.mod.max) # Failed to converge
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std + (1 + (parallel.std) | item) + (1 + (parallel.std) |      subject)
#    Data: df.first.R7
# 
# REML criterion at convergence: 22626.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.6026 -0.5867 -0.0860  0.4436  5.8298 
# 
# Random effects:
#  Groups   Name         Variance Std.Dev. Corr 
#  item     (Intercept)     72.45   8.512       
#           parallel.std  2845.04  53.339  0.22 
#  subject  (Intercept)   4761.05  69.000       
#           parallel.std    26.79   5.176  -1.00
#  Residual              18761.72 136.973       
# Number of obs: 1775, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error      df t value            Pr(>|t|)    
# (Intercept)   226.078     12.473  43.645  18.125 <0.0000000000000002 ***
# parallel.std   -5.524     10.407  45.978  -0.531               0.598    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# paralll.std -0.043
# convergence code: 0
# boundary (singular) fit: see ?isSingular


################
### Region 8 ###
################

first.R8.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.first.R8)
summary(first.R8.mod.max) # Failed to converge

# Keep build models that will be converged,
first.R8.mod.1 <- lmer(RT ~ parallel.std*semantic.std + (1+(parallel.std*semantic.std)|item)+(1+(parallel.std)|subject), data = df.first.R8)
summary(first.R8.mod.1) # Failed to converge 

first.R8.mod.2 <- lmer(RT ~ parallel.std*semantic.std + (1+(parallel.std*semantic.std)|item)+(1|subject), data = df.first.R8)
summary(first.R8.mod.2) # Failed to converge

first.R8.mod.3 <- lmer(RT ~ parallel.std*semantic.std + (1+(semantic.std)|item)+(1|subject), data = df.first.R8)
summary(first.R8.mod.3) # Failed to converge

first.R8.mod.4 <- lmer(RT ~ parallel.std*semantic.std + (1|item)+(1|subject), data = df.first.R8)
summary(first.R8.mod.4) # Failed to  converge

first.R8.mod.5 <- lmer(RT ~ parallel.std*semantic.std + (1|subject), data = df.first.R8)

summary(first.R8.mod.5)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | subject)
#    Data: df.first.R8
# 
# REML criterion at convergence: 20035.9
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.5301 -0.3118 -0.1447 -0.0430  9.7751 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  subject  (Intercept)  614.6   24.79   
#  Residual             4510.9   67.16   
# Number of obs: 1776, groups:  subject, 37
# 
# Fixed effects:
#                            Estimate Std. Error        df t value   Pr(>|t|)    
# (Intercept)                 22.8311     4.3761   35.9996   5.217 0.00000773 ***
# parallel.std                 4.4459     3.1874 1736.0002   1.395      0.163    
# semantic.std                -2.3851     3.1874 1736.0002  -0.748      0.454    
# parallel.std:semantic.std    0.6261     6.3749 1736.0002   0.098      0.922    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std 0.000               
# semantc.std 0.000  0.000        
# prlll.std:. 0.000  0.000  0.000 

##################################################
###### Reanalysis w.o the factor 'semantic' ######
##################################################

first.R8.mod.max <- lmer(RT ~ parallel.std + 
                           (1+(parallel.std)|item)+(1+(parallel.std)|subject), data = df.first.R8)
summary(first.R8.mod.max) #  Failed to converge


first.R8.mod.1 <- lmer(RT ~ parallel.std + 
                           (1+(parallel.std)|item)+(1|subject), data = df.first.R8)
summary(first.R8.mod.1) #  Failed to converge

first.R8.mod.2 <- lmer(RT ~ parallel.std + 
                         (1|item)+(1|subject), data = df.first.R8)
summary(first.R8.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std + (1 | item) + (1 | subject)
#    Data: df.first.R8
# 
# REML criterion at convergence: 20044.5
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.5474 -0.3329 -0.1499 -0.0374  9.7460 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   35.9    5.992  
#  subject  (Intercept)  615.4   24.807  
#  Residual             4472.0   66.873  
# Number of obs: 1776, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value   Pr(>|t|)    
# (Intercept)    22.831      4.461 37.988   5.118 0.00000918 ***
# parallel.std    4.446      3.614 46.000   1.230      0.225    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# paralll.std 0.000 