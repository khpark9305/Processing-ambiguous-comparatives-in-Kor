######################################
##### Kihyo's MA thesis analysis #####
######################################

## REMOVE ALL ##
rm(list=ls())

setwd("C:/R/comparatives")
options(scipen=999)

library(descriptr)
library(lme4)

df = read.csv("sentence.completion.csv", header=T)
str(df)

#############
#### EDA ####
#############
ds_summary_stats(df)
summary(df)

############################
#### Model Construction ####
############################

model <- glmer(ans ~ type + (1|subjects) + (1|items), data=df, family=binomial("logit"))

summary(model)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: ans ~ type + (1 | subjects) + (1 | items)
# Data: df.sencom
# 
# AIC      BIC   logLik deviance df.resid 
# 1370.3   1398.4   -680.2   1360.3     2035 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -9.0454 -0.2655  0.1214  0.2882  7.7130 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# items    (Intercept) 1.9754   1.4055  
# subjects (Intercept) 0.4548   0.6744  
# Number of obs: 2040, groups:  items, 60; subjects, 34
# 
# Fixed effects:
#              Estimate Std. Error z value             Pr(>|z|)    
#   (Intercept)  -3.1913     0.3899  -8.185 0.000000000000000272 ***
#   typestrong    6.1598     0.5401  11.405 < 0.0000000000000002 ***
#   typeweak      4.2876     0.5046   8.498 < 0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) typstr
# typestrong -0.682       
# typeweak   -0.714  0.541