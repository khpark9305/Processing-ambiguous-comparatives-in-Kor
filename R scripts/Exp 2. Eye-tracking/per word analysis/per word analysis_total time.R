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

###############################
##### Total reading times #####
###############################

# read .csv files
df.total.R1 = read.csv("total.time.R1.csv", header=T)
df.total.R2 = read.csv("total.time.R2.csv", header=T)
df.total.R3 = read.csv("total.time.R3.csv", header=T)
df.total.R4 = read.csv("total.time.R4.csv", header=T)
df.total.R5 = read.csv("total.time.R5.csv", header=T)
df.total.R6 = read.csv("total.time.R6.csv", header=T)
df.total.R7 = read.csv("total.time.R7.csv", header=T)
df.total.R8 = read.csv("total.time.R8.csv", header=T)

#############
#### EDA ####
#############

# change Participants and Stimulus as integer
df.total.R1$item <- as.integer(df.total.R1$Stimulus)
df.total.R1$subject <- as.integer(df.total.R1$Participant)
str(df.total.R1)

df.total.R2$item <- as.integer(df.total.R2$Stimulus)
df.total.R2$subject <- as.integer(df.total.R2$Participant)
str(df.total.R2)

df.total.R3$item <- as.integer(df.total.R3$Stimulus)
df.total.R3$subject <- as.integer(df.total.R3$Participant)
str(df.total.R3)

df.total.R4$item <- as.integer(df.total.R4$Stimulus)
df.total.R4$subject <- as.integer(df.total.R4$Participant)
str(df.total.R4)

df.total.R5$item <- as.integer(df.total.R5$Stimulus)
df.total.R5$subject <- as.integer(df.total.R5$Participant)
str(df.total.R5)

df.total.R6$item <- as.integer(df.total.R6$Stimulus)
df.total.R6$subject <- as.integer(df.total.R6$Participant)
str(df.total.R6)

df.total.R7$item <- as.integer(df.total.R7$Stimulus)
df.total.R7$subject <- as.integer(df.total.R7$Participant)
str(df.total.R7)

df.total.R8$item <- as.integer(df.total.R8$Stimulus)
df.total.R8$subject <- as.integer(df.total.R8$Participant)
str(df.total.R8)

# Summaries of the data
mean.SD.R1 <- ddply(df.total.R1,c("condition"),
                    summarise,mean = mean(RT),
                                sd = sd(RT),
                                 n = length(RT),
                                se = sd/sqrt(n))
mean.SD.R1
# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 377.6831 261.4268 421 12.74116
# 2    ma-nma 402.8005 273.3712 421 13.32330
# 3    nma-ma 354.4253 242.2464 434 11.62820
# 4   nma-nma 348.4365 250.7957 430 12.09444

mean.SD.R1.semantic <- ddply(df.total.R1,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R1.semantic
#   semantic     mean       sd   n       se
# 1        1 390.2418 267.6018 842 9.222167
# 2        2 351.4448 246.4136 864 8.383162

mean.SD.R2 <- ddply(df.total.R2,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R2

# Fixation time less than 1200
# condition     mean       sd   n       se
# 1     ma-ma 556.9639 318.1096 360 16.76585
# 2    ma-nma 476.0844 287.7262 391 14.55094
# 3    nma-ma 473.3409 289.3288 408 14.32391
# 4   nma-nma 464.7483 278.3226 408 13.77902

mean.SD.R2.semantic <- ddply(df.total.R2,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R2.semantic
#   semantic     mean       sd   n        se
# 1        1 514.8549 305.1585 751 11.135393
# 2        2 469.0446 283.7374 816  9.932796


mean.SD.R3 <- ddply(df.total.R3,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R3

# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 474.4383 286.3227 402 14.28048
# 2    ma-nma 469.1855 278.7213 414 13.69841
# 3    nma-ma 354.8615 256.7131 431 12.36543
# 4   nma-nma 327.2968 240.5432 434 11.54644

mean.SD.R3.semantic <- ddply(df.total.R3,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R3.semantic
#   semantic     mean       sd   n       se
# 1        1 471.7733 282.3304 816 9.883542
# 2        2 341.0313 248.9697 865 8.465224

mean.SD.R4 <- ddply(df.total.R4,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R4

# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 209.3812 206.6952 442 9.831486
# 2    ma-nma 174.2109 173.7953 442 8.266599
# 3    nma-ma 222.1559 194.3986 440 9.267589
# 4   nma-nma 228.8835 209.5671 443 9.956832

mean.SD.R4.semantic <- ddply(df.total.R4,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R4.semantic
#   semantic     mean       sd   n       se
# 1        1 191.7960 191.6563 884 6.446099
# 2        2 225.5311 202.0644 883 6.800008


mean.SD.R5 <- ddply(df.total.R5,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R5

# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 510.6572 304.5569 421 14.84320
# 2    ma-nma 478.4924 289.8802 421 14.12790
# 3    nma-ma 508.0688 303.2777 417 14.85157
# 4   nma-nma 458.6240 283.9995 416 13.92422

mean.SD.R5.semantic <- ddply(df.total.R5,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R5.semantic
#   semantic     mean       sd   n        se
# 1        1 509.3692 303.7422 838 10.492601
# 2        2 468.6176 286.9730 837  9.919238

mean.SD.R6 <- ddply(df.total.R6,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R6

# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 533.1347 286.9464 395 14.43784
# 2    ma-nma 438.6291 244.7626 422 11.91485
# 3    nma-ma 550.1511 283.5022 401 14.15742
# 4   nma-nma 490.3818 273.7679 406 13.58687

mean.SD.R6.semantic <- ddply(df.total.R6,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R6.semantic
#   semantic     mean       sd   n        se
# 1        1 541.7070 285.1642 796 10.107376
# 2        2 464.0054 260.5224 828  9.053778

mean.SD.R7 <- ddply(df.total.R7,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R7

# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 371.1296 256.6183 432 12.34655
# 2    ma-nma 292.2858 228.6823 431 11.01524
# 3    nma-ma 396.7171 263.2722 438 12.57963
# 4   nma-nma 299.5450 226.0948 440 10.77865

mean.SD.R7.semantic <- ddply(df.total.R7,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R7.semantic
#   semantic     mean       sd   n       se
# 1        1 384.0116 260.1550 870 8.820081
# 2        2 295.9529 227.2771 871 7.700991

mean.SD.R8 <- ddply(df.total.R8,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R8

# fixation time less than 1200
#  condition     mean       sd   n       se
# 1     ma-ma 27.62635 79.97567 444 3.795478
# 2    ma-nma 23.10495 78.82028 444 3.740645
# 3    nma-ma 24.48694 92.80414 444 4.404290
# 4   nma-nma 27.78581 83.97447 444 3.985252

mean.SD.R8.semantic <- ddply(df.total.R8,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R8.semantic
#   semantic     mean       sd   n       se
# 1        1 26.05664 86.59310 888 2.905874
# 2        2 25.44538 81.42592 888 2.732475

# Check the normality of the DVs
ggplot(df.total.R1, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.total.R1$RT, ylab="Total Reading times");qqline(df.total.R1$RT, col=2)

ggplot(df.total.R2, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.total.R2$RT, ylab="Total Reading times");qqline(df.total.R2$RT, col=2)

ggplot(df.total.R3, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.total.R3$RT, ylab="Total Reading times");qqline(df.total.R3$RT, col=2)

ggplot(df.total.R4, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.total.R4$RT, ylab="Total Reading times");qqline(df.total.R4$RT, col=2)

ggplot(df.total.R5, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.total.R5$RT, ylab="Total Reading times");qqline(df.total.R5$RT, col=2)

ggplot(df.total.R6, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.total.R6$RT, ylab="Total Reading times");qqline(df.total.R6$RT, col=2)

ggplot(df.total.R7, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.total.R7$RT, ylab="Total Reading times");qqline(df.total.R7$RT, col=2)

ggplot(df.total.R8, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.total.R8$RT, ylab="Total Reading times");qqline(df.total.R8$RT, col=2)
# All of the regions have positively skewed distribution, therby needing to transform DV into logarithmic scale.

############################
#### Model Construction ####
############################

# Tranform DV into logarithmic scale
# df.total.R1$logRT <- log(df.total.R1$RT)
# df.total.R2$logRT <- log(df.total.R2$RT)
# df.total.R3$logRT <- log(df.total.R3$RT)
# df.total.R4$logRT <- log(df.total.R4$RT)
# df.total.R5$logRT <- log(df.total.R5$RT)
# df.total.R6$logRT <- log(df.total.R6$RT)
# df.total.R7$logRT <- log(df.total.R7$RT)
# df.total.R8$logRT <- log(df.total.R8$RT)
# Since first and total pass time have 0 values (which means there values with N/A with logarithmic scale), you can't use these.

# Rescale IVs
df.total.R1 <- transform(df.total.R1, semantic.std=arm::rescale(semantic))
df.total.R2 <- transform(df.total.R2, semantic.std=arm::rescale(semantic))
df.total.R3 <- transform(df.total.R3, semantic.std=arm::rescale(semantic))
df.total.R4 <- transform(df.total.R4, semantic.std=arm::rescale(semantic))

df.total.R5 <- transform(df.total.R5, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
df.total.R6 <- transform(df.total.R6, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
df.total.R7 <- transform(df.total.R7, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
df.total.R8 <- transform(df.total.R8, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))

################
### Region 1 ###
################

# Maximum model
total.R1.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item)+(1+(semantic.std)|subject), data = df.total.R1)
summary(total.R1.mod.max) # Failed to converge

# Keep build models that will be converged,
# with dropping the random effect explaining the least variance
# with dropping corrleation pratmeters that had values -1 or +1 (cf. Metzner et al. 2015)

total.R1.mod.1 <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item)+(1|subject), data = df.total.R1)
summary(total.R1.mod.1) # Failed to converge

total.R1.mod.2 <- lmer(RT ~ semantic.std +
                         (1|item)+(1|subject), data = df.total.R1)

summary(total.R1.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.total.R1
# 
# REML criterion at convergence: 23159.7
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.0018 -0.6037 -0.1414  0.5357  4.6219 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  2260     47.54  
#  subject  (Intercept) 23120    152.05  
#  Residual             42097    205.18  
# Number of obs: 1706, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value            Pr(>|t|)    
# (Intercept)    377.42      26.40  40.90  14.298 <0.0000000000000002 ***
# semantic.std   -42.64      16.95  45.87  -2.515              0.0155 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.002 

################
### Region 2 ###
################

# Maximum model
total.R2.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.total.R2)
summary(total.R2.mod.max) # Failed to converge

total.R2.mod.1 <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1|subject), data = df.total.R2)
summary(total.R2.mod.1) # Failed to converge

total.R2.mod.2 <- lmer(RT ~ semantic.std +
                         (1|item) + (1|subject), data = df.total.R2)

summary(total.R2.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.total.R2
# 
# REML criterion at convergence: 21787.3
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.6447 -0.6984 -0.1322  0.6384  3.5060 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  2643     51.41  
#  subject  (Intercept) 27967    167.23  
#  Residual             58617    242.11  
# Number of obs: 1567, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value             Pr(>|t|)    
# (Intercept)    509.71      29.16  40.34  17.482 < 0.0000000000000002 ***
# semantic.std   -55.18      19.29  45.95  -2.861              0.00633 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.006 


################
### Region 3 ###
################

# Maximum model
total.R3.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.total.R3)
summary(total.R3.mod.max) # Failed to converge

total.R3.mod.1 <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1|subject), data = df.total.R3)
summary(total.R3.mod.1) # Failed to converge

total.R3.mod.2 <- lmer(RT ~ semantic.std +
                         (1|item) + (1|subject), data = df.total.R3)

summary(total.R3.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.total.R3
# 
# REML criterion at convergence: 23065.6
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.4981 -0.6794 -0.1389  0.5583  3.5083 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  2350     48.48  
#  subject  (Intercept) 20618    143.59  
#  Residual             49052    221.48  
# Number of obs: 1681, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error      df t value             Pr(>|t|)    
# (Intercept)    412.69      25.21   41.42  16.368 < 0.0000000000000002 ***
# semantic.std  -139.60      17.70   46.14  -7.887       0.000000000426 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.006 

################
### Region 4 ###
################

#Maximum model
total.R4.mod.max <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.total.R4)
summary(total.R4.mod.max) # Failed to converge

total.R4.mod.1 <- lmer(RT ~ semantic.std +
                           (1+(semantic.std)|item) + (1|subject), data = df.total.R4)
summary(total.R4.mod.1) # Failed to converge

total.R4.mod.2 <- lmer(RT ~ semantic.std +
                         (1|item) + (1|subject), data = df.total.R4)
summary(total.R4.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 | subject)
#    Data: df.total.R4
# 
# REML criterion at convergence: 23432.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.1007 -0.6408 -0.1734  0.4437  4.9324 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   782.9   27.98  
#  subject  (Intercept)  6517.0   80.73  
#  Residual             31735.6  178.14  
# Number of obs: 1767, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value            Pr(>|t|)    
# (Intercept)    209.26      14.51  41.21  14.426 <0.0000000000000002 ***
# semantic.std    33.74      11.71  45.97   2.881               0.006 ** 
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
total.R5.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.total.R5)

summary(total.R5.mod.max)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 + (parallel.std * semantic.std) |  
#     item) + (1 + (parallel.std * semantic.std) | subject)
#    Data: df.total.R5
# 
# REML criterion at convergence: 23280.8
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.9213 -0.6796 -0.1046  0.6308  3.0189 
# 
# Random effects:
#  Groups   Name                      Variance Std.Dev. Corr             
#  item     (Intercept)                  41.15   6.415                   
#           parallel.std               1421.55  37.703  -0.84            
#           semantic.std               3080.81  55.505   0.81 -0.77      
#           parallel.std:semantic.std  6838.63  82.696  -0.63  0.38 -0.87
#  subject  (Intercept)               27248.65 165.072                   
#           parallel.std                 15.57   3.946  -1.00            
#           semantic.std                354.96  18.840  -1.00  1.00      
#           parallel.std:semantic.std   977.10  31.259   1.00 -1.00 -1.00
#  Residual                           59833.49 244.609                   
# Number of obs: 1675, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error     df t value            Pr(>|t|)    
# (Intercept)                 499.91      28.39  38.77  17.611 <0.0000000000000002 ***
# parallel.std                 -7.66      16.63  37.11  -0.461              0.6477    
# semantic.std                -41.44      16.91  38.27  -2.450              0.0189 *  
# parallel.std:semantic.std   -23.42      33.63  37.88  -0.696              0.4904    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std -0.143              
# semantc.std -0.125 -0.280       
# prlll.std:.  0.061  0.168 -0.380
# convergence code: 0
# boundary (singular) fit: see ?isSingular

################
### Region 6 ###
################

# Maximum model
total.R6.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.total.R6)
summary(total.R6.mod.max) # Failed to converge

# Keep build models that will be converged in the same way as above.
total.R6.mod.1 <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(semantic.std)|subject), data = df.total.R6)
summary(total.R6.mod.1) # Failed to converge

total.R6.mod.2 <- lmer(RT ~ parallel.std*semantic.std + 
                         (1+(parallel.std*semantic.std)|item)+(1|subject), data = df.total.R6)
summary(total.R6.mod.2) # Failed to converge

total.R6.mod.3 <- lmer(RT ~ parallel.std*semantic.std + 
                         (1+(parallel.std)|item)+(1|subject), data = df.total.R6)
summary(total.R6.mod.3) # Failed to converge

total.R6.mod.4 <- lmer(RT ~ parallel.std*semantic.std + 
                         (1|item)+(1|subject), data = df.total.R6)

summary(total.R6.mod.4)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | item) + (1 | subject)
#    Data: df.total.R6
# 
# REML criterion at convergence: 22562.8
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.3234 -0.7095 -0.1159  0.6260  2.8239 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  2513     50.13  
#  subject  (Intercept) 14125    118.85  
#  Residual             59697    244.33  
# Number of obs: 1624, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error     df t value             Pr(>|t|)    
# (Intercept)                 513.56      21.72  42.58  23.649 < 0.0000000000000002 ***
# parallel.std                 20.29      18.90  43.52   1.073               0.2890    
# semantic.std                -82.91      18.90  43.55  -4.386             0.000072 ***
# parallel.std:semantic.std    75.12      37.79  43.53   1.988               0.0532 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std -0.003              
# semantc.std  0.004  0.002       
# prlll.std:.  0.001  0.011 -0.008

##################################################
###### Reanalysis w.o the factor 'semantic' ######
##################################################

total.R6.mod.max <- lmer(RT ~ parallel.std + 
                           (1+(parallel.std)|item)+(1+(parallel.std)|subject), data = df.total.R6)
summary(total.R6.mod.max) # Failed to converge

total.R6.mod.1 <- lmer(RT ~ parallel.std + 
                           (1+(parallel.std)|item)+(1|subject), data = df.total.R6) 
summary(total.R6.mod.1) # Failed to converge


total.R6.mod.2 <- lmer(RT ~ parallel.std + 
                         (1|item)+(1|subject), data = df.total.R6) 
summary(total.R6.mod.2) # Failed to converge

total.R6.mod.3 <- lmer(RT ~ parallel.std + 
                         (1|subject), data = df.total.R6) 
summary(total.R6.mod.3)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std + (1 | subject)
#    Data: df.total.R6
# 
# REML criterion at convergence: 22651.5
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.4343 -0.7449 -0.1399  0.6185  2.8500 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  subject  (Intercept) 13473    116.1   
#  Residual             64061    253.1   
# Number of obs: 1624, groups:  subject, 37
# 
# Fixed effects:
#              Estimate Std. Error      df t value            Pr(>|t|)    
# (Intercept)    512.04      20.10   34.96  25.469 <0.0000000000000002 ***
# parallel.std    21.83      12.57 1585.72   1.736              0.0828 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# paralll.std 0.001 


################
### Region 7 ###
################

# Maximum model
total.R7.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.total.R7)
summary(total.R7.mod.max) # Failed to converge

# Keep build models that will be converged in the same way as above.
total.R7.mod.1 <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(semantic.std)|subject), data = df.total.R7)
summary(total.R7.mod.1) # Faild to converge

total.R7.mod.2 <- lmer(RT ~ parallel.std*semantic.std + 
                         (1+(parallel.std*semantic.std)|item)+(1|subject), data = df.total.R7)
summary(total.R7.mod.2) # Failed to converge

total.R7.mod.3 <- lmer(RT ~ parallel.std*semantic.std + 
                         (1+(semantic.std)|item)+(1|subject), data = df.total.R7)
summary(total.R7.mod.3) # Failed to converge

total.R7.mod.4 <- lmer(RT ~ parallel.std*semantic.std + 
                         (1|item)+(1|subject), data = df.total.R7)

summary(total.R7.mod.4)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | item) + (1 | subject)
#    Data: df.total.R7
# 
# REML criterion at convergence: 23600.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.5206 -0.6631 -0.1153  0.5140  4.2965 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)  3298     57.43  
#  subject  (Intercept) 17200    131.15  
#  Residual             41695    204.19  
# Number of obs: 1741, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error     df t value             Pr(>|t|)    
# (Intercept)                 345.68      23.62  44.72  14.638 < 0.0000000000000002 ***
# parallel.std                -10.29      19.25  43.66  -0.534                0.596    
# semantic.std                -86.48      19.25  43.66  -4.491            0.0000513 ***
# parallel.std:semantic.std    24.53      38.51  43.67   0.637                0.527    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std  0.000              
# semantc.std  0.000 -0.002       
# prlll.std:. -0.001  0.000  0.001

##################################################
###### Reanalysis w.o the factor 'semantic' ######
##################################################

total.R7.mod.max <- lmer(RT ~ parallel.std + 
                           (1+(parallel.std)|item)+(1+(parallel.std)|subject), data = df.total.R7)
summary(total.R7.mod.max) # Failed to converge

total.R7.mod.1 <- lmer(RT ~ parallel.std + 
                           (1+(parallel.std)|item)+(1|subject), data = df.total.R7)
summary(total.R7.mod.1) # Failed to converge

total.R7.mod.2 <- lmer(RT ~ parallel.std + 
                         (1|subject), data = df.total.R7)
summary(total.R7.mod.2) 

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std + (1 | subject)
#    Data: df.total.R7
# 
# REML criterion at convergence: 23747.8
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.2249 -0.6958 -0.1623  0.5210  4.2889 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  subject  (Intercept) 17201    131.2   
#  Residual             46637    216.0   
# Number of obs: 1741, groups:  subject, 37
# 
# Fixed effects:
#              Estimate Std. Error      df t value            Pr(>|t|)    
# (Intercept)    345.40      22.18   35.32  15.574 <0.0000000000000002 ***
# parallel.std   -11.38      10.35 1702.36  -1.099               0.272    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# paralll.std 0.000 

################
### Region 8 ###
################

# Maximum model
total.R8.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                           (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.total.R8)

summary(total.R8.mod.max)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 + (parallel.std * semantic.std) |  
#     item) + (1 + (parallel.std * semantic.std) | subject)
#    Data: df.total.R8
# 
# REML criterion at convergence: 20605.6
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.5225 -0.3011 -0.1348 -0.0232 13.0487 
# 
# Random effects:
#  Groups   Name                      Variance Std.Dev. Corr             
#  item     (Intercept)                  0.00   0.000                    
#           parallel.std                49.13   7.009    NaN             
#           semantic.std               113.54  10.656    NaN  1.00       
#           parallel.std:semantic.std  172.46  13.133    NaN  1.00  1.00 
#  subject  (Intercept)                843.30  29.040                    
#           parallel.std                50.35   7.096   -0.15            
#           semantic.std               122.58  11.072    0.21  0.93      
#           parallel.std:semantic.std  391.37  19.783    0.17 -1.00 -0.93
#  Residual                           6131.45  78.304                    
# Number of obs: 1776, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error      df t value  Pr(>|t|)    
# (Intercept)                25.7510     5.2265 38.2305   4.927 0.0000165 ***
# parallel.std                3.9101     4.4111 47.5561   0.886     0.380    
# semantic.std               -0.6113     4.6271 39.4825  -0.132     0.896    
# parallel.std:semantic.std   1.5414     9.1085 39.1910   0.169     0.866    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std 0.026               
# semantc.std 0.116  0.250        
# prlll.std:. 0.120  0.001  0.008 
# convergence code: 0
# boundary (singular) fit: see ?isSingular

##################################################
###### Reanalysis w.o the factor 'semantic' ######
##################################################

total.R8.mod.max <- lmer(RT ~ parallel.std + 
                           (1+(parallel.std)|item)+(1+(parallel.std)|subject), data = df.total.R8)
summary(total.R8.mod.max)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std + (1 + (parallel.std) | item) + (1 + (parallel.std) |      subject)
#    Data: df.total.R8
# 
# REML criterion at convergence: 20621.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.4611 -0.2924 -0.1237 -0.0305 13.3418 
# 
# Random effects:
#  Groups   Name         Variance Std.Dev. Corr 
#  item     (Intercept)    18.772  4.333        
#           parallel.std   75.369  8.682   1.00 
#  subject  (Intercept)   841.674 29.012        
#           parallel.std    1.265  1.125   -1.00
#  Residual              6204.270 78.767        
# Number of obs: 1776, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value  Pr(>|t|)    
# (Intercept)    25.751      5.199 37.633   4.953 0.0000157 ***
# parallel.std    3.910      4.140 65.128   0.944     0.348    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# paralll.std 0.032 
# convergence code: 0
# boundary (singular) fit: see ?isSingular