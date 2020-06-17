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
##### Go-past reading times ########
####################################

# read .csv files
df.gopast.R2 = read.csv("gopast.time.R2.csv", header=T)
df.gopast.R3 = read.csv("gopast.time.R3.csv", header=T)
df.gopast.R4 = read.csv("gopast.time.R4.csv", header=T)
df.gopast.R5 = read.csv("gopast.time.R5.csv", header=T)
df.gopast.R6 = read.csv("gopast.time.R6.csv", header=T)
df.gopast.R7 = read.csv("gopast.time.R7.csv", header=T)

#############
#### EDA ####
#############

# change subjects and item as integer
# df.gopast.R2$item <- as.integer(df.gopast.R2$item)
# df.gopast.R2$subject <- as.integer(df.gopast.R2$subject)
str(df.gopast.R2)
# 
# df.gopast.R3$item <- as.integer(df.gopast.R3$item)
# df.gopast.R3$subject <- as.integer(df.gopast.R3$subject)
str(df.gopast.R3)
# 
# df.gopast.R4$item <- as.integer(df.gopast.R4$item)
# df.gopast.R4$subject <- as.integer(df.gopast.R4$subject)
str(df.gopast.R4)
# 
# df.gopast.R5$item <- as.integer(df.gopast.R5$item)
# df.gopast.R5$subject <- as.integer(df.gopast.R5$subject)
str(df.gopast.R5)
# 
# df.gopast.R6$item <- as.integer(df.gopast.R6$item)
# df.gopast.R6$subject <- as.integer(df.gopast.R6$subject)
str(df.gopast.R6)
# 
# df.gopast.R7$item <- as.integer(df.gopast.R7$item)
# df.gopast.R7$subject <- as.integer(df.gopast.R7$subject)
str(df.gopast.R7)
# They are already intergers, so you don't have to change their type.

# Summaries of the data
mean.SD.R2 <- ddply(df.gopast.R2,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R2
#   condition     mean       sd   n        se
# 1     ma-ma 332.6828 237.7418 419 11.614449
# 2    ma-nma 309.9532 218.3506 438 10.433197
# 3    nma-ma 299.2876 210.4746 442 10.011254
# 4   nma-nma 288.0277 193.8613 437  9.273643

mean.SD.R2.semantic <- ddply(df.gopast.R2,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R2.semantic
#   semantic     mean      sd   n       se
# 1        1 321.0660 228.187 857 7.794721
# 2        2 293.6896 202.349 879 6.825061

mean.SD.R3 <- ddply(df.gopast.R3,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R3
#   condition     mean       sd   n       se
# 1     ma-ma 335.7870 260.9950 415 12.81174
# 2    ma-nma 375.4074 276.6028 420 13.49683
# 3    nma-ma 253.7825 212.2729 435 10.17770
# 4   nma-nma 251.5101 212.8210 437 10.18061

mean.SD.R3.semantic <- ddply(df.gopast.R3,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R3.semantic
#   semantic     mean       sd   n       se
# 1        1 355.7158 269.5275 835 9.327385
# 2        2 252.6437 212.4287 872 7.193744

mean.SD.R4 <- ddply(df.gopast.R4,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R4
#   condition     mean       sd   n       se
# 1     ma-ma 211.3706 237.0647 436 11.35334
# 2    ma-nma 189.0011 215.6391 435 10.33911
# 3    nma-ma 230.5430 241.1173 437 11.53420
# 4   nma-nma 220.8500 218.4222 434 10.48460

mean.SD.R4.semantic <- ddply(df.gopast.R4,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R4.semantic
#   semantic     mean       sd   n       se
# 1        1 200.1987 226.7637 871 7.683594
# 2        2 225.7132 230.0077 871 7.793515

mean.SD.R5 <- ddply(df.gopast.R5,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R5
#   condition     mean       sd   n       se
# 1     ma-ma 278.4694 192.1029 432 9.242556
# 2    ma-nma 293.3556 194.8064 441 9.276495
# 3    nma-ma 276.3970 182.9935 436 8.763798
# 4   nma-nma 269.1136 190.4992 435 9.133736

mean.SD.R5.semantic <- ddply(df.gopast.R5,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R5.semantic
#   semantic     mean       sd   n       se
# 1        1 277.4285 187.4771 868 6.363388
# 2        2 281.3176 192.9510 876 6.519210

mean.SD.R6 <- ddply(df.gopast.R6,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R6
#   condition     mean       sd   n       se
# 1     ma-ma 359.1341 250.1445 417 12.24963
# 2    ma-nma 314.4203 209.2905 428 10.11644
# 3    nma-ma 369.5960 236.7010 429 11.42803
# 4   nma-nma 343.4960 230.1769 428 11.12602

mean.SD.R6.semantic <- ddply(df.gopast.R6,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R6.semantic
#   semantic     mean       sd   n       se
# 1        1 364.4392 243.3322 846 8.365933
# 2        2 328.9582 220.3337 856 7.530852

mean.SD.R7 <- ddply(df.gopast.R7,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
mean.SD.R7
#   condition     mean       sd   n       se
# 1     ma-ma 252.5108 176.9205 437 8.463253
# 2    ma-nma 222.4320 167.5746 438 8.007024
# 3    nma-ma 249.3514 178.8662 436 8.566136
# 4   nma-nma 220.8320 173.0663 438 8.269426

mean.SD.R7.semantic <- ddply(df.gopast.R7,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

mean.SD.R7.semantic
#   semantic     mean       sd   n       se
# 1        1 250.9329 177.7998 873 6.017613
# 2        2 221.6320 170.2471 876 5.752116

# Check the normality of the DVs
ggplot(df.gopast.R2, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.gopast.R2$RT, ylab="gopast Reading times");qqline(df.gopast.R2$RT, col=2)

ggplot(df.gopast.R3, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.gopast.R3$RT, ylab="gopast Reading times");qqline(df.gopast.R3$RT, col=2)

ggplot(df.gopast.R4, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.gopast.R4$RT, ylab="gopast Reading times");qqline(df.gopast.R4$RT, col=2)

ggplot(df.gopast.R5, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.gopast.R5$RT, ylab="gopast Reading times");qqline(df.gopast.R5$RT, col=2)

ggplot(df.gopast.R6, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.gopast.R6$RT, ylab="gopast Reading times");qqline(df.gopast.R6$RT, col=2)

ggplot(df.gopast.R7, aes(x=RT)) + geom_histogram(col="pink",labels=TRUE)
qqnorm(df.gopast.R7$RT, ylab="gopast Reading times");qqline(df.gopast.R7$RT, col=2)
# All of the regions have positively skewed distribution, therby needing to transform DV into logarithmic scale.

############################
#### Model Construction ####
############################

# Rescale IVs
df.gopast.R2 <- transform(df.gopast.R2, semantic.std=arm::rescale(semantic))
df.gopast.R3 <- transform(df.gopast.R3, semantic.std=arm::rescale(semantic))
df.gopast.R4 <- transform(df.gopast.R4, semantic.std=arm::rescale(semantic))
str(df.gopast.R2)
str(df.gopast.R3)
str(df.gopast.R4)

df.gopast.R5 <- transform(df.gopast.R5, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
df.gopast.R6 <- transform(df.gopast.R6, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
df.gopast.R7 <- transform(df.gopast.R7, parallel.std=arm::rescale(parallel), semantic.std=arm::rescale(semantic))
str(df.gopast.R5)
str(df.gopast.R6)
str(df.gopast.R7)

################
### Region 2 ###
################

# Maximum model
gopast.R2.mod.max <- lmer(RT ~ semantic.std +
                            (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.gopast.R2)
summary(gopast.R2.mod.max) # Failed to converge

gopast.R2.mod.1 <- lmer(RT ~ semantic.std +
                            (1+(semantic.std)|item) + (1|subject), data = df.gopast.R2) # Failed to converge
summary(gopast.R2.mod.1)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 + (semantic.std) | item) + (1 | subject)
#    Data: df.gopast.R2
# 
# REML criterion at convergence: 23249.4
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.3572 -0.6115 -0.1925  0.3437  4.2890 
# 
# Random effects:
#  Groups   Name         Variance Std.Dev. Corr
#  item     (Intercept)     16.7    4.087      
#           semantic.std  1914.2   43.751  0.48
#  subject  (Intercept)  10212.3  101.056      
#  Residual              36189.1  190.234      
# Number of obs: 1736, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value            Pr(>|t|)    
# (Intercept)    308.81      17.53  38.09  17.618 <0.0000000000000002 ***
# semantic.std   -28.83      11.17  44.37  -2.581              0.0132 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.017 

################
### Region 3 ###
################

# Maximum model
gopast.R3.mod.max <- lmer(RT ~ semantic.std +
                            (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.gopast.R3)
summary(gopast.R3.mod.max) # Failed to converge

gopast.R3.mod.1 <- lmer(RT ~ semantic.std +
                            (1+(semantic.std)|item) + (1|subject), data = df.gopast.R3)
summary(gopast.R3.mod.max)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 + (semantic.std) | item) + (1 + (semantic.std) |      subject)
#    Data: df.gopast.R3
# 
# REML criterion at convergence: 23318.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.5474 -0.5942 -0.2187  0.3344  4.1125 
# 
# Random effects:
#  Groups   Name         Variance Std.Dev. Corr 
#  item     (Intercept)      0      0.00        
#           semantic.std  5152     71.78    NaN 
#  subject  (Intercept)  10088    100.44        
#           semantic.std  4884     69.89   -0.92
#  Residual              46948    216.68        
# Number of obs: 1707, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error      df t value             Pr(>|t|)    
# (Intercept)    307.58      18.09   41.09  17.004 < 0.0000000000000002 ***
# semantic.std  -109.81      18.71   44.87  -5.868          0.000000495 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std -0.522
# convergence code: 0
# boundary (singular) fit: see ?isSingular


################
### Region 4 ###
################

# Maximum model
gopast.R4.mod.max <- lmer(RT ~ semantic.std +
                            (1+(semantic.std)|item) + (1+(semantic.std)|subject), data = df.gopast.R4)
summary(gopast.R4.mod.max) # Failed to converge

gopast.R4.mod.1 <- lmer(RT ~ semantic.std +
                            (1|item) + (1+(semantic.std)|subject), data = df.gopast.R4)
summary(gopast.R4.mod.1)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ semantic.std + (1 | item) + (1 + (semantic.std) | subject)
#    Data: df.gopast.R4
# 
# REML criterion at convergence: 23719.8
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.6894 -0.5914 -0.2179  0.2307  4.4555 
# 
# Random effects:
#  Groups   Name         Variance Std.Dev. Corr
#  item     (Intercept)    608.7   24.67       
#  subject  (Intercept)   5787.7   76.08       
#           semantic.std   545.4   23.35   0.32
#  Residual              45865.8  214.16       
# Number of obs: 1742, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value            Pr(>|t|)    
# (Intercept)    213.92      13.98  39.57  15.301 <0.0000000000000002 ***
# semantic.std    25.54      13.07  30.86   1.954              0.0598 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# semantc.std 0.084 

################
### Region 5 ###
################

# Maximum model
gopast.R5.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.gopast.R5)
summary(gopast.R5.mod.max) # Failed to converge

gopast.R5.mod.1 <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std*semantic.std)|item)+(1+(parallel.std)+(semantic.std)|subject), data = df.gopast.R5)
summary(gopast.R5.mod.1)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 + (parallel.std * semantic.std) |      item) + (1 + (parallel.std) + (semantic.std) | subject)
#    Data: df.gopast.R5
# 
# REML criterion at convergence: 23023.9
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.3369 -0.5797 -0.1512  0.3355  5.2719 
# 
# Random effects:
#  Groups   Name                      Variance Std.Dev. Corr             
#  item     (Intercept)                  22.52   4.745                   
#           parallel.std                465.47  21.575   0.48            
#           semantic.std                352.67  18.779  -0.64 -0.98      
#           parallel.std:semantic.std   344.59  18.563  -0.84 -0.87  0.95
#  subject  (Intercept)                5447.59  73.808                   
#           parallel.std                  9.03   3.005  -0.20            
#           semantic.std                175.74  13.257   1.00 -0.22      
#  Residual                           30578.55 174.867                   
# Number of obs: 1744, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error      df t value            Pr(>|t|)    
# (Intercept)                280.292     13.036  37.705  21.502 <0.0000000000000002 ***
# parallel.std               -10.306      9.546  32.874  -1.080               0.288    
# semantic.std                 3.617      9.779  46.216   0.370               0.713    
# parallel.std:semantic.std  -26.474     19.080  46.545  -1.388               0.172    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std  0.034              
# semantc.std  0.161 -0.213       
# prlll.std:. -0.079 -0.134  0.119
# convergence code: 0
# boundary (singular) fit: see ?isSingular


################
### Region 6 ###
################

# Maximum model
gopast.R6.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.gopast.R6)
summary(gopast.R6.mod.max) # Failed to converge

gopast.R6.mod.1 <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std)+(semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.gopast.R6)
summary(gopast.R6.mod.1) # Failed to converge

gopast.R6.mod.2 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1|item)+(1+(parallel.std*semantic.std)|subject), data = df.gopast.R6)
summary(gopast.R6.mod.2) # Failed to converge

gopast.R6.mod.3 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1|item)+(1|subject), data = df.gopast.R6)
summary(gopast.R6.mod.3)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | item) + (1 | subject)
#    Data: df.gopast.R6
# 
# REML criterion at convergence: 23219.9
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.8422 -0.6641 -0.2657  0.4207  3.6915 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   524.1   22.89  
#  subject  (Intercept)  5709.2   75.56  
#  Residual             47788.6  218.61  
# Number of obs: 1702, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error     df t value             Pr(>|t|)    
# (Intercept)                 347.78      13.91  38.82  25.010 < 0.0000000000000002 ***
# parallel.std                 10.33      12.49  43.93   0.827              0.41280    
# semantic.std                -36.70      12.50  43.96  -2.937              0.00526 ** 
# parallel.std:semantic.std    37.52      24.99  43.94   1.502              0.14034    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std -0.001              
# semantc.std  0.001 -0.005       
# prlll.std:. -0.002  0.001 -0.002

##################################################
###### Reanalysis w.o the factor 'semantic' ######
##################################################

gopast.R6.mod.max <- lmer(RT ~ parallel.std + 
                            (1+(parallel.std)|item)+(1+(parallel.std)|subject), data = df.gopast.R6)
summary(gopast.R6.mod.max) # Failed to converge

gopast.R6.mod.1 <- lmer(RT ~ parallel.std + 
                            (1+(parallel.std)|item)+(1|subject), data = df.gopast.R6)
summary(gopast.R6.mod.1) # Failed to converge

gopast.R6.mod.2 <- lmer(RT ~ parallel.std + 
                          (1|item)+(1|subject), data = df.gopast.R6)
summary(gopast.R6.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std + (1 | item) + (1 | subject)
#    Data: df.gopast.R6
# 
# REML criterion at convergence: 23245.1
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.8695 -0.6521 -0.2610  0.4260  3.6448 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   892.7   29.88  
#  subject  (Intercept)  5698.9   75.49  
#  Residual             47784.7  218.60  
# Number of obs: 1702, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error     df t value            Pr(>|t|)    
# (Intercept)    347.89      14.17  41.41  24.553 <0.0000000000000002 ***
# parallel.std    10.11      13.67  46.33   0.739               0.463    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# paralll.std -0.001

################
### Region 7 ###
################

# Maximum model
gopast.R7.mod.max <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std*semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.gopast.R7)
summary(gopast.R7.mod.max) # Failed to converge

gopast.R7.mod.1 <- lmer(RT ~ parallel.std*semantic.std + 
                            (1+(parallel.std)+(semantic.std)|item)+(1+(parallel.std*semantic.std)|subject), data = df.gopast.R7)
summary(gopast.R7.mod.1) # Failed to converge

gopast.R7.mod.2 <- lmer(RT ~ parallel.std*semantic.std + 
                          (1|item)+(1+(parallel.std*semantic.std)|subject), data = df.gopast.R7)
summary(gopast.R7.mod.2) # Failed to converge
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std * semantic.std + (1 | item) + (1 + (parallel.std *      semantic.std) | subject)
#    Data: df.gopast.R7
# 
# REML criterion at convergence: 22665.5
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.6398 -0.5691 -0.1147  0.4095  5.6183 
# 
# Random effects:
#  Groups   Name                      Variance Std.Dev. Corr             
#  item     (Intercept)                 622.8   24.96                    
#  subject  (Intercept)                6366.3   79.79                    
#           parallel.std                449.0   21.19   -0.33            
#           semantic.std               1185.3   34.43    0.35 -0.73      
#           parallel.std:semantic.std   361.1   19.00   -0.13 -0.49 -0.22
#  Residual                           23214.2  152.36                    
# Number of obs: 1749, groups:  item, 48; subject, 37
# 
# Fixed effects:
#                           Estimate Std. Error       df t value            Pr(>|t|)    
# (Intercept)               237.4981    14.0827  40.5184  16.865 <0.0000000000000002 ***
# parallel.std                0.4035    10.8239  40.4691   0.037              0.9704    
# semantic.std              -29.1934    11.7073  41.5098  -2.494              0.0167 *  
# parallel.std:semantic.std  -6.3849    20.7327  42.3190  -0.308              0.7596    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prlll. smntc.
# paralll.std -0.099              
# semantc.std  0.158 -0.114       
# prlll.std:. -0.018 -0.023 -0.016
# convergence code: 0
# boundary (singular) fit: see ?isSingular

##################################################
###### Reanalysis w.o the factor 'semantic' ######
##################################################

gopast.R7.mod.max <- lmer(RT ~ parallel.std + 
                            (1+(parallel.std)|item)+(1+(parallel.std)|subject), data = df.gopast.R7)
summary(gopast.R7.mod.max) # Failed to converge

gopast.R7.mod.1 <- lmer(RT ~ parallel.std + 
                            (1+(parallel.std)|item)+(1|subject), data = df.gopast.R7)
summary(gopast.R7.mod.1) # Failed to converge

gopast.R7.mod.2 <- lmer(RT ~ parallel.std + 
                          (1|item)+(1|subject), data = df.gopast.R7)
summary(gopast.R7.mod.2)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: RT ~ parallel.std + (1 | item) + (1 | subject)
#    Data: df.gopast.R7
# 
# REML criterion at convergence: 22696
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.6136 -0.5852 -0.1024  0.4009  5.6255 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  item     (Intercept)   786.1   28.04  
#  subject  (Intercept)  6341.0   79.63  
#  Residual             23647.3  153.78  
# Number of obs: 1749, groups:  item, 48; subject, 37
# 
# Fixed effects:
#              Estimate Std. Error       df t value            Pr(>|t|)    
# (Intercept)  237.4617    14.1876  41.7243  16.737 <0.0000000000000002 ***
# parallel.std   0.6693    10.9367  45.5079   0.061               0.951    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# paralll.std 0.000 