######################################
##### Kihyo's MA thesis analysis #####
######################################

## REMOVE ALL ##

setwd("C:/R/comparatives")
options(scipen=999)

library(descriptr)
library(lme4)

df.compre = read.csv("sentence.comprehension.csv", header=T)
str(df.compre)

#############
#### EDA ####
#############
ds_summary_stats(df.compre)
summary(df.compre)

############################
#### Model Construction ####
############################

df.compre.trans<-transform(df.compre, type.std=arm::rescale(type))

str(df.compre.trans)

# Maximal model
model.max <- glmer(ans ~ type.std + (1+type.std|subjects) + (1+type.std|items), data=df.compre.trans, family=binomial)
summary(model.max) # Failed to converge

model1 <- glmer(ans ~ type.std + (1|subjects) + (1+type.std|items), data=df.compre.trans, family=binomial)
summary(model1) # Failed to converge

model2 <- glmer(ans ~ type.digit.std + (1+type.std|subjects) + (1|items), data=df.compre.trans, family=binomial)
summary(model2)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: ans ~ type.std + (1 + type.std | subjects) + (1 | items)
# Data: df.compre.trans
# 
# AIC      BIC   logLik deviance df.resid 
# 1194.4   1226.3   -591.2   1182.4     1494 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.6600 -0.3327 -0.1683  0.3142  4.1121 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev. Corr 
# subjects (Intercept) 2.2798   1.510         
# type.std    7.4912   2.737    -0.34
# items    (Intercept) 0.1089   0.330         
# Number of obs: 1500, groups:  subjects, 50; items, 30
# 
# Fixed effects:
#   Estimate Std. Error z value            Pr(>|z|)    
# (Intercept)  -0.4623     0.2569  -1.799               0.072 .  
# type.std      4.0301     0.4833   8.339 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# type.std -0.216