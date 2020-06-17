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

#####################################
##### Second-pass reading times #####
####################################

# read .csv files
df.second.R1 = read.csv("second.time.R1.csv", header=T)
df.second.R2 = read.csv("second.time.R2.csv", header=T)
df.second.R3 = read.csv("second.time.R3.csv", header=T)
df.second.R4 = read.csv("second.time.R4.csv", header=T)
df.second.R5 = read.csv("second.time.R5.csv", header=T)
df.second.R6 = read.csv("second.time.R6.csv", header=T)
df.second.R7 = read.csv("second.time.R7.csv", header=T)
df.second.R8 = read.csv("second.time.R8.csv", header=T)

#############
#### EDA ####
#############

# change Participants and Stimulus as integer
df.second.R1$item <- as.integer(df.second.R1$Stimulus)
df.second.R1$subject <- as.integer(df.second.R1$Participant)
str(df.second.R1)

df.second.R2$item <- as.integer(df.second.R2$Stimulus)
df.second.R2$subject <- as.integer(df.second.R2$Participant)
str(df.second.R2)

df.second.R3$item <- as.integer(df.second.R3$Stimulus)
df.second.R3$subject <- as.integer(df.second.R3$Participant)
str(df.second.R3)

df.second.R4$item <- as.integer(df.second.R4$Stimulus)
df.second.R4$subject <- as.integer(df.second.R4$Participant)
str(df.second.R4)

df.second.R5$item <- as.integer(df.second.R5$Stimulus)
df.second.R5$subject <- as.integer(df.second.R5$Participant)
str(df.second.R5)

df.second.R6$item <- as.integer(df.second.R6$Stimulus)
df.second.R6$subject <- as.integer(df.second.R6$Participant)
str(df.second.R6)

df.second.R7$item <- as.integer(df.second.R7$Stimulus)
df.second.R7$subject <- as.integer(df.second.R7$Participant)
str(df.second.R7)

df.second.R8$item <- as.integer(df.second.R8$Stimulus)
df.second.R8$subject <- as.integer(df.second.R8$Participant)
str(df.second.R8)

# Summaries of the data
mean.SD.R1 <- ddply(df.second.R1,c("condition"),
                    summarise,mean = mean(RT),
                                sd = sd(RT),
                                 n = length(RT),
                                se = sd/sqrt(n))
mean.SD.R1
#   condition     mean       sd   n       se
# 1     ma-ma  194.5829 256.3559 434  12.30548
# 2    ma-nma  207.2471 275.0201 433  13.21662
# 3    nma-ma  157.4240 223.0681 441  10.62229
# 4   nma-nma  153.2374 220.8258 438  10.55147


mean.SD.R1.semantic <- ddply(df.second.R1,c("semantic"),
                             summarise,mean = mean(RT),
                                         sd = sd(RT),
                                          n = length(RT),
                                        se = sd/sqrt(n))

mean.SD.R1.semnatic
#   semantic     mean       sd   n       se
# 1        1 200.9077 265.7631 867 9.025787
# 2        2 155.3379 221.8371 879 7.482380


mean.SD.R2 <- ddply(df.second.R2,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R2
#   condition     mean       sd   n       se
# 1     ma-ma  346.4256 326.2198 390 16.51878
# 2    ma-nma  261.8305 293.4993 413 14.44216
# 3    nma-ma  263.4329 300.3910 432 14.45257
# 4   nma-nma  252.8677 287.4535 431 13.84615

mean.SD.R2.semantic <- ddply(df.second.R2,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R2.semantic
#   semantic     mean       sd   n       se
# 1        1 302.9166 312.5054 803 11.02808
# 2        2 258.1564 293.8779 863 10.00372

mean.SD.R3 <- ddply(df.second.R3,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R3
#   condition     mean       sd   n       se
# 1     ma-ma  284.7730 302.6984 423 14.71770
# 2    ma-nma  234.2640 266.2400 428 12.86920
# 3    nma-ma  169.6728 226.8285 437 10.85068
# 4   nma-nma  143.6273 218.5443 440 10.41869

mean.SD.R3.semantic <- ddply(df.second.R3,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R3.semantic
#   semantic     mean       sd   n       se
# 1        1 259.3702 285.8968 851 9.800415
# 2        2 156.6055 222.9647 877 7.528982


mean.SD.R4 <- ddply(df.second.R4,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R4
#   condition     mean       sd   n        se
# 1     ma-ma   61.65991 135.1327 444 6.413114
# 2    ma-nma   42.29955 126.8557 444 6.020304
# 3    nma-ma   60.60722 137.9320 443 6.553349
# 4   nma-nma   64.20946 142.1125 444 6.744362

mean.SD.R4.semantic <- ddply(df.second.R4,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R4.semantic
#   semantic     mean       sd   n       se
# 1        1 51.97973 131.3433 888 4.407591
# 2        2 62.41037 139.9728 887 4.699827

mean.SD.R5 <- ddply(df.second.R5,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R5
#   condition     mean       sd   n       se
# 1     ma-ma  276.4664 292.5179 446 13.85112
# 2    ma-nma  223.4700 259.1411 434 12.43917
# 3    nma-ma  277.6812 282.6607 436 13.53699
# 4   nma-nma  238.9558 261.0634 430 12.58960

mean.SD.R5.semantic <- ddply(df.second.R5,c("semantic"),
                             summarise,mean = mean(RT),
                                         sd = sd(RT),
                                          n = length(RT),
                                         se = sd/sqrt(n))

mean.SD.R5.semantic
#   semantic     mean       sd   n       se
# 1        1 269.5200 175.6888 898 5.862813
# 2        2 265.1819 170.7965 885 5.741261

mean.SD.R6 <- ddply(df.second.R6,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R6
#   condition     mean       sd   n       se
# 1     ma-ma  300.3641 292.7193 423 14.23250
# 2    ma-nma  191.6273 236.4608 432 11.37673
# 3    nma-ma  310.3271 293.1858 428 14.17167
# 4   nma-nma  253.8415 292.3809 429 14.11628

mean.SD.R6.semantic <- ddply(df.second.R6,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R6.semantic
#   semantic     mean       sd   n       se
# 1        1 305.3749 292.8241 851 10.03788
# 2        2 222.6260 267.4605 861  9.11503

mean.SD.R7 <- ddply(df.second.R7,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R7
#   condition     mean       sd   n       se
# 1     ma-ma  150.67273 231.3825 440  11.030729
# 2    ma-nma   96.31293 196.6211 441   9.362909
# 3    nma-ma  169.75621 234.4358 443  11.138381
# 4   nma-nma   93.29478 167.3105 441   7.967165

mean.SD.R7.semantic <- ddply(df.second.R7,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R7.semantic
#   semantic      mean       sd   n       se
# 1        1 160.24689 232.9830 883 7.840502
# 2        2  94.80385 182.4576 882 6.143667


mean.SD.R8 <- ddply(df.second.R8,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R8
#   condition     mean       sd  n       se
# 1     ma-ma 1.461712 21.62038 444 1.026058
# 2    ma-nma 3.788288 27.48339 444 1.304304
# 3    nma-ma 2.463964 41.24048 444 1.957187
# 4   nma-nma 3.700450 33.37465 444 1.583891

mean.SD.R8.semantic <- ddply(df.second.R8,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R8.semantic
#   semantic     mean       sd   n       se
# 1        1 1.962838 32.91107 888 1.104423
# 2        2 3.744369 30.55405 888 1.025327


# Check the normality of the DVs
ggplot(df.second.R1, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.second.R1$RT, ylab="second Reading times");qqline(df.second.R1$RT, col=2)

ggplot(df.second.R2, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.second.R2$RT, ylab="second Reading times");qqline(df.second.R2$RT, col=2)

ggplot(df.second.R3, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.second.R3$RT, ylab="second Reading times");qqline(df.second.R3$RT, col=2)

ggplot(df.second.R4, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.second.R4$RT, ylab="second Reading times");qqline(df.second.R4$RT, col=2)

ggplot(df.second.R5, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.second.R5$RT, ylab="second Reading times");qqline(df.second.R5$RT, col=2)

ggplot(df.second.R6, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.second.R6$RT, ylab="second Reading times");qqline(df.second.R6$RT, col=2)

ggplot(df.second.R7, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.second.R7$RT, ylab="second Reading times");qqline(df.second.R7$RT, col=2)

ggplot(df.second.R8, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.second.R8$RT, ylab="second Reading times");qqline(df.second.R8$RT, col=2)
# All of the regions have positively skewed distribution, therby needing to transform DV into logarithmic scale.

############################
#### Model Construction ####
############################

# Rescale IVs
df.second.R1 <- transform(df.second.R1, semantic.std=arm::rescale(semantic))
df.second.R2 <- transform(df.second.R2, semantic.std=arm::rescale(semantic))
df.second.R3 <- transform(df.second.R3, semantic.std=arm::rescale(semantic))
df.second.R4 <- transform(df.second.R4, semantic.std=arm::rescale(semantic))

df.second.R5 <- transform(df.second.R5, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
df.second.R6 <- transform(df.second.R6, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
df.second.R7 <- transform(df.second.R7, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
df.second.R8 <- transform(df.second.R8, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))

################
### Region 1 ###
################

# Maximum model
second.R1.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.second.R1)
summary(second.R1.mod.max) # Failed to converge

second.R1.mod.1 <- lmer(RT ~ semantic.std +
                            (1+(semantic.std)|item) + (1|subject), data = df.second.R1)
summary(second.R1.mod.1) # Failed to converge

second.R1.mod.2 <- lmer(RT ~ semantic.std +
                          (1|item) + (1|subject), data = df.second.R1)

summary(second.R1.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.second.R1
# 
# REML criterion at convergence: 23656.9
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.2925 -0.5646 -0.1919  0.3632  4.9203 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  2046     45.24  
#  subject  (Intercept) 17426    132.01  
#  Residual             41324    203.28  
# Number of obs: 1746, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value      Pr(>|t|)    
# (Intercept)    181.19      23.18  41.77   7.817 0.00000000105 ***
# semantic.std   -47.64      16.29  45.83  -2.925       0.00534 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.001 

################
### Region 2 ###
################

# Maximum model
second.R2.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.second.R2)
summary(second.R2.mod.max) # Failed to converge

second.R2.mod.1 <- lmer(RT ~ semantic.std +
                            (1|item) + (1|subject), data = df.second.R2)

summary(second.R2.mod.1)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.second.R2
# 
# REML criterion at convergence: 23350.3
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.1562 -0.7027 -0.1833  0.5138  3.8416 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  1676     40.94  
#  subject  (Intercept) 26591    163.07  
#  Residual             66557    257.99  
# Number of obs: 1666, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value         Pr(>|t|)    
# (Intercept)    291.77      28.18  38.58   10.35 0.00000000000108 ***
# semantic.std   -55.15      17.34  46.25   -3.18          0.00263 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.004 


################
### Region 3 ###
################

# Maximum model
second.R3.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.second.R3)
summary(second.R3.mod.max) # Failed to converge

second.R3.mod.1 <- lmer(RT ~ semantic.std +
                            (1|item) + (1|subject), data = df.second.R3)

summary(second.R3.mod.1)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.second.R3
# 
# REML criterion at convergence: 23632.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.5075 -0.6392 -0.2018  0.4314  4.1933 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  2050     45.27  
#  subject  (Intercept) 17856    133.63  
#  Residual             47098    217.02  
# Number of obs: 1728, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error      df t value        Pr(>|t|)    
# (Intercept)    212.11      23.51   41.33   9.023 0.0000000000255 ***
# semantic.std  -109.11      16.74   45.96  -6.520 0.0000000481553 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.003 

################
### Region 4 ###
################

#Maximum model
second.R4.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.second.R4)
summary(second.R4.mod.max) # Failed to converge

second.R4.mod.1 <- lmer(RT ~ semantic.std +
                            (1|item) + (1|subject), data = df.second.R4)

summary(second.R4.mod.1)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.second.R4
# 
# REML criterion at convergence: 22299.6
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.6918 -0.4227 -0.2268 -0.0356  7.3503 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   226.6   15.05  
#  subject  (Intercept)  2286.1   47.81  
#  Residual             15978.3  126.41  
# Number of obs: 1775, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value    Pr(>|t|)    
# (Intercept)    57.230      8.690 39.762   6.586 0.000000073 ***
# semantic.std   10.507      7.409 45.901   1.418       0.163    
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
second.R5.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.second.R5)
summary(second.R5.mod.max) # Failed to converge

# Keep build models that will be converged,
second.R5.mod.1 <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std*semantic.std)|item)+(1+(semantic.std)|subject), data = df.second.R5)
summary(second.R5.mod.1) # Failed to converge

second.R5.mod.2 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1+(parallel.std*semantic.std)|item)+(1|subject), data = df.second.R5)
summary(second.R5.mod.2) # Failed to converge

second.R5.mod.3 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1+(parallel.std)|item)+(1|subject), data = df.second.R5)
summary(second.R5.mod.3) # Failed to converge

second.R5.mod.4 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1|item)+(1|subject), data = df.second.R5)

summary(second.R5.mod.4)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | item) + (1 | subject)
#    Data: df.second.R5
# 
# REML criterion at convergence: 25649.6
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.3245 -0.5662 -0.1597  0.3940  9.7982 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  2308     48.04  
#  subject  (Intercept) 34275    185.14  
#  Residual             89451    299.08  
# Number of obs: 1794, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error      df t value        Pr(>|t|)    
# (Intercept)                290.412     32.006  39.312   9.074 0.0000000000346 ***
# parallel.std                27.507     19.805  43.536   1.389          0.1719    
# semantic.std               -38.813     19.805  43.536  -1.960          0.0564 .  
# parallel.std:semantic.std    3.567     39.609  43.544   0.090          0.9287    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std  0.002              
# semantc.std -0.002  0.004       
# prlll.std:.  0.001 -0.006  0.006

################
### Region 6 ###
################

# Maximum Model
second.R6.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.second.R6)
summary(second.R6.mod.max) # Failed to converge

# Keep build models that will be converged,
second.R6.mod.1 <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std*semantic.std)|item)+(1+(semantic.std)|subject), data = df.second.R6)
summary(second.R6.mod.1) # Failed to converge

second.R6.mod.2 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1+(parallel.std*semantic.std)|item)+(1|subject), data = df.second.R6)
summary(second.R6.mod.2) # Failed to converge

second.R6.mod.3 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1+(semantic.std)|item)+(1|subject), data = df.second.R6)
summary(second.R6.mod.3) # Failed to converge

second.R6.mod.4 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1|item)+(1|subject), data = df.second.R6)

summary(second.R6.mod.4)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | item) + (1 | subject)
#    Data: df.second.R6
# 
# REML criterion at convergence: 25672.8
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.6804 -0.5921 -0.1914  0.3800  5.6983 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   6113    78.19  
#  subject  (Intercept)  30841   175.62  
#  Residual             104023   322.53  
# Number of obs: 1776, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error     df t value         Pr(>|t|)    
# (Intercept)                 311.59      31.93  45.39   9.759 0.00000000000101 ***
# parallel.std                 34.63      27.27  44.00   1.270          0.21084    
# semantic.std                -94.77      27.27  44.00  -3.475          0.00116 ** 
# parallel.std:semantic.std    58.86      54.54  44.00   1.079          0.28636    
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

second.R6.mod.max <- lmer(RT ~ parallel.std + 
                            (1+(parallel.std)|item)+(1+(parallel.std)|subject), data = df.second.R6)
summary(second.R6.mod.max)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std + (1 + (parallel.std) | item) + (1 + (parallel.std) |      subject)
#    Data: df.second.R6
# 
# REML criterion at convergence: 23819.5
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.7007 -0.6755 -0.2051  0.5649  3.8496 
# 
# Random effects:
#  Groups   Name         Variance Std.Dev. Corr 
#  item     (Intercept)   1429.7   37.81        
#           parallel.std 16902.3  130.01   -0.73
#  subject  (Intercept)  16809.2  129.65        
#           parallel.std   515.7   22.71   1.00 
#  Residual              59386.9  243.69        
# Number of obs: 1712, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value            Pr(>|t|)    
# (Intercept)    269.94      24.64  50.11  10.954 0.00000000000000666 ***
# parallel.std    26.89      24.96  38.21   1.077               0.288    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# paralll.std -0.114
# convergence code: 0
# boundary (singular) fit: see ?isSingular


################
### Region 7 ###
################

# Maximum Model
second.R7.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.second.R7)
summary(second.R7.mod.max) # Failed to converge

# Keep build models that will be converged,
second.R7.mod.1 <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std*semantic.std)|item)+(1+(semantic.std)|subject), data = df.second.R7)
summary(second.R7.mod.1) # Failed to converge

second.R7.mod.2 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1+(parallel.std*semantic.std)|item)+(1|subject), data = df.second.R7)
summary(second.R7.mod.2) # Failed to converge

second.R7.mod.3 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1+(semantic.std)|item)+(1|subject), data = df.second.R7)
summary(second.R7.mod.3) # Failed to converge

second.R7.mod.4 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1|item)+(1|subject), data = df.second.R7)

summary(second.R7.mod.4)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | item) + (1 | subject)
#    Data: df.second.R7
# 
# REML criterion at convergence: 24146.3
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.7992 -0.5144 -0.2024  0.2300 10.5467 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  2235     47.28  
#  subject  (Intercept) 17729    133.15  
#  Residual             43797    209.28  
# Number of obs: 1776, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error      df t value    Pr(>|t|)    
# (Intercept)                138.043     23.460  42.268   5.884 0.000000571 ***
# parallel.std                -7.116     16.879  44.001  -0.422    0.675376    
# semantic.std               -62.224     16.879  44.001  -3.687    0.000621 ***
# parallel.std:semantic.std    9.191     33.758  44.001   0.272    0.786683    
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

second.R7.mod.max <- lmer(RT ~ parallel.std + 
                            (1+(parallel.std)|item)+(1+(parallel.std)|subject), data = df.second.R7)
summary(second.R7.mod.max) # Failed to converge

second.R7.mod.1 <- lmer(RT ~ parallel.std + 
                            (1+(parallel.std)|item)+(1|subject), data = df.second.R7)
summary(second.R7.mod.1) # Failed to converge

second.R7.mod.2 <- lmer(RT ~ parallel.std + 
                          (1|subject), data = df.second.R7)
summary(second.R7.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std + (1 | subject)
#    Data: df.second.R7
# 
# REML criterion at convergence: 23607.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.6738 -0.5478 -0.3065  0.3196  5.8984 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  subject  (Intercept)  9802     99.01  
#  Residual             35986    189.70  
# Number of obs: 1765, groups:  subject, 37
# 
# Fixed effects:
#              Estimate Std. Error       df t value      Pr(>|t|)    
# (Intercept)   129.516     16.892   35.514   7.667 0.00000000488 ***
# parallel.std   -9.991      9.031 1726.552  -1.106         0.269    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# paralll.std 0.000 


################
### Region 8 ###
################

# Maximum Model
second.R8.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.second.R8)

summary(second.R8.mod.max)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 + (parallel.std * semantic.std) |  
#     item) + (1 + (parallel.std * semantic.std) | subject)
#    Data: df.second.R8
# 
# REML criterion at convergence: 17294
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.1104 -0.0720 -0.0661 -0.0320 25.8918 
# 
# Random effects:
#  Groups   Name                      Variance Std.Dev. Corr             
#  item     (Intercept)                 1.064   1.031                    
#           parallel.std                4.359   2.088   -1.00            
#           semantic.std                4.313   2.077   -1.00  1.00      
#           parallel.std:semantic.std  16.912   4.112    1.00 -1.00 -1.00
#  subject  (Intercept)                16.527   4.065                    
#           parallel.std               27.640   5.257   -0.50            
#           semantic.std               30.497   5.522    0.09  0.81      
#           parallel.std:semantic.std  18.305   4.278    0.94 -0.77 -0.25
#  Residual                           973.989  31.209                    
# Number of obs: 1776, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error      df t value Pr(>|t|)   
# (Intercept)                 2.8536     1.0414 38.2010    2.74  0.00928 **
# parallel.std               -0.5450     1.8161 46.0725   -0.30  0.76543   
# semantic.std                1.7815     1.8372 42.3228    0.97  0.33771   
# parallel.std:semantic.std   0.9144     3.2709 73.8522    0.28  0.78060   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std -0.249              
# semantc.std -0.065  0.299       
# prlll.std:.  0.235 -0.199 -0.146
# convergence code: 0
# boundary (singular) fit: see ?isSingular

##################################################
###### Reanalysis w.o the factor 'semantic' ######
##################################################

second.R8.mod.max <- lmer(RT ~ parallel.std + 
                            (1+(parallel.std)|item)+(1+(parallel.std)|subject), data = df.second.R8)
summary(second.R8.mod.max) # Failed to converge

second.R8.mod.1 <- lmer(RT ~ parallel.std + 
                            (1|item)+(1+(parallel.std)|subject), data = df.second.R8)
summary(second.R8.mod.1) # Failed to converge

second.R8.mod.2 <- lmer(RT ~ parallel.std + 
                          (1|item)+(1|subject), data = df.second.R8)
summary(second.R8.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std + (1 | item) + (1 | subject)
#    Data: df.second.R8
# 
# REML criterion at convergence: 17312.2
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -0.3635 -0.0614 -0.0614 -0.0441 26.5886 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   0.00    0.000  
#  subject  (Intercept)  14.84    3.853  
#  Residual             994.61   31.537  
# Number of obs: 1776, groups:  item, 48; subject, 37
# 
# Fixed effects:
#               Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)     2.8536     0.9804   36.0000   2.911  0.00615 **
# parallel.std   -0.5450     1.4967 1738.0000  -0.364  0.71578   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# paralll.std 0.000 
# convergence code: 0
# boundary (singular) fit: see ?isSingular