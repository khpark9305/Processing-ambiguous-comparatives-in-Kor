######################################################
##### Kihyo's MA thesis analysis - Visualization #####
######################################################

## REMOVE ALL ##
rm(list=ls())

setwd("C:/R/comparatives")
options(scipen=999)

library(plyr)
library(dplyr)
library(ggplot2)

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
first.mean.SD.R1 <- ddply(df.first.R1,c("condition"),
                          summarise,mean = mean(RT),
                          sd = sd(RT),
                          n = length(RT),
                          se = sd/sqrt(n))
first.mean.SD.R1
#   condition      mean       sd   n       se
# 1     ma-ma  210.4032 125.2016 444 5.941804
# 2    ma-nma  220.1306 130.1463 444 6.176472
# 3    nma-ma  211.7427 131.2071 443 6.233839
# 4   nma-nma  212.1441 141.2331 444 6.702626

Region <- c("R1", "R1", "R1", "R1") # add a value as region

x <- cbind(first.mean.SD.R1, Region) # add the region column to first.mean.SD.R1 (the rest process is the same as below)
x

first.mean.SD.R1.semantic <- ddply(df.first.R1,c("semantic"),
                                   summarise,mean = mean(RT),
                                   sd = sd(RT),
                                   n = length(RT),
                                   se = sd/sqrt(n))

first.mean.SD.R1.semantic
#   semantic     mean       sd   n       se
# 1        1 215.2669 127.7186 888 4.285957
# 2        2 211.9436 136.2412 887 4.574531

first.mean.SD.R2 <- ddply(df.first.R2,c("condition"),
                          summarise,mean = mean(RT),
                          sd = sd(RT),
                          n = length(RT),
                          se = sd/sqrt(n))
first.mean.SD.R2
#   condition     mean       sd   n       se
# 1     ma-ma  273.6753 174.8612 425   8.482013
# 2    ma-nma  255.6290 157.8421 442   7.507785
# 3    nma-ma  260.1869 171.0118 444   8.115863
# 4   nma-nma  251.9252 155.0294 441   7.382350

Region <- c("R2", "R2", "R2", "R2")

y <- cbind(first.mean.SD.R2, Region)
y

first.mean.SD.R2.semantic <- ddply(df.first.R2,c("semantic"),
                                   summarise,mean = mean(RT),
                                   sd = sd(RT),
                                   n = length(RT),
                                   se = sd/sqrt(n))

first.mean.SD.R2.semantic
#   semantic     mean       sd   n       se
# 1        1 264.4752 166.5508 867 5.656360
# 2        2 256.0701 163.2035 885 5.486024

first.mean.SD.R3 <- ddply(df.first.R3,c("condition"),
                          summarise,mean = mean(RT),
                          sd = sd(RT),
                          n = length(RT),
                          se = sd/sqrt(n))
first.mean.SD.R3
#   condition     mean       sd   n       se
# 1     ma-ma  232.2280 135.60753 443  6.442908
# 2    ma-nma  265.3657 164.18275 443  7.800558
# 3    nma-ma  197.1599 127.29301 444  6.041059
# 4   nma-nma  197.5000 134.75644 444  6.395258

Region <- c("R3", "R3", "R3", "R3")

z <- cbind(first.mean.SD.R3, Region)
z

first.mean.SD.R3.semantic <- ddply(df.first.R3,c("semantic"),
                                   summarise,mean = mean(RT),
                                   sd = sd(RT),
                                   n = length(RT),
                                   se = sd/sqrt(n))

first.mean.SD.R3.semantic
#   semantic     mean       sd   n       se
# 1        1 248.7968 151.3998 886 5.086377
# 2        2 197.3300 131.0041 888 4.396208

first.mean.SD.R4 <- ddply(df.first.R4,c("condition"),
                          summarise,mean = mean(RT),
                          sd = sd(RT),
                          n = length(RT),
                          se = sd/sqrt(n))
first.mean.SD.R4
#   condition     mean       sd   n       se
# 1     ma-ma 152.4910 136.6165 444 6.483534
# 2    ma-nma 136.2410 115.9768 444 5.504014
# 3    nma-ma 168.1081 140.6637 444 6.675605
# 4   nma-nma 166.6171 132.8857 444 6.306477

Region <- c("R4", "R4", "R4", "R4")

w <- cbind(first.mean.SD.R4, Region)
w

first.mean.SD.R4.semantic <- ddply(df.first.R4,c("semantic"),
                                   summarise,mean = mean(RT),
                                   sd = sd(RT),
                                   n = length(RT),
                                   se = sd/sqrt(n))

first.mean.SD.R4.semantic
#   semantic     mean       sd   n       se
# 1        1 144.3660 126.9068 888 4.258713
# 2        2 167.3626 136.7549 888 4.589193

first.mean.SD.R5 <- ddply(df.first.R5,c("condition"),
                          summarise,mean = mean(RT),
                          sd = sd(RT),
                          n = length(RT),
                          se = sd/sqrt(n))
first.mean.SD.R5
#   condition     mean       sd   n       se
# 1     ma-ma  277.4070 188.7872 457   8.831091
# 2    ma-nma  279.8258 176.7548 442   8.407370
# 3    nma-ma  261.3469 160.8028 441   7.657276
# 4   nma-nma  250.5711 163.5325 443   7.769664

Region <- c("R5", "R5", "R5", "R5")

p <- cbind(first.mean.SD.R5, Region)
p

first.mean.SD.R5.semantic <- ddply(df.first.R5,c("semantic"),
                                   summarise,mean = mean(RT),
                                   sd = sd(RT),
                                   n = length(RT),
                                   se = sd/sqrt(n))

first.mean.SD.R5.semantic
# semantic     mean       sd   n       se
# 1        1 269.5200 175.6888 898 5.862813
# 2        2 265.1819 170.7965 885 5.741261

first.mean.SD.R6 <- ddply(df.first.R6,c("condition"),
                          summarise,mean = mean(RT),
                          sd = sd(RT),
                          n = length(RT),
                          se = sd/sqrt(n))
first.mean.SD.R6
#   condition     mean       sd   n       se
# 1     ma-ma  286.5405 177.1345 444   8.406430
# 2    ma-nma  263.2957 152.1432 443   7.228539
# 3    nma-ma  287.1697 173.8787 442   8.270565
# 4   nma-nma  277.4027 174.9642 442   8.322197

Region <- c("R6", "R6", "R6", "R6")

q <- cbind(first.mean.SD.R6, Region)
q

first.mean.SD.R6.semantic <- ddply(df.first.R6,c("semantic"),
                                   summarise,mean = mean(RT),
                                   sd = sd(RT),
                                   n = length(RT),
                                   se = sd/sqrt(n))

first.mean.SD.R6.semantic
#   semantic     mean       sd   n       se
# 1        1 286.8544 175.4189 886 5.893313
# 2        2 270.3412 163.9975 885 5.512715

first.mean.SD.R7 <- ddply(df.first.R7,c("condition"),
                          summarise,mean = mean(RT),
                          sd = sd(RT),
                          n = length(RT),
                          se = sd/sqrt(n))
first.mean.SD.R7
#   condition     mean       sd   n       se
# 1     ma-ma  238.9257 155.5814 444 7.383568
# 2    ma-nma  219.1580 160.1228 443 7.607664
# 3    nma-ma  238.2523 151.6972 444 7.199232
# 4   nma-nma  207.7095 152.3064 444 7.228143

Region <- c("R7", "R7", "R7", "R7")

r <- cbind(first.mean.SD.R7, Region)
r

first.mean.SD.R7.semantic <- ddply(df.first.R7,c("semantic"),
                                   summarise,mean = mean(RT),
                                   sd = sd(RT),
                                   n = length(RT),
                                   se = sd/sqrt(n))

first.mean.SD.R7.semantic
#   semantic     mean       sd   n       se
# 1        1 238.5890 153.5653 888 5.153315
# 2        2 213.4273 156.2759 887 5.247230

first.mean.SD.R8 <- ddply(df.first.R8,c("condition"),
                          summarise,mean = mean(RT),
                          sd = sd(RT),
                          n = length(RT),
                          se = sd/sqrt(n))
first.mean.SD.R8
#   condition     mean       sd   n       se
# 1     ma-ma 26.09009 75.34490 444 3.575711
# 2    ma-nma 19.25901 65.26562 444 3.097369
# 3    nma-ma 21.95721 74.89930 444 3.554564
# 4   nma-nma 24.01802 69.96135 444 3.320219

Region <- c("R8", "R8", "R8", "R8")

s <- cbind(first.mean.SD.R8, Region)
s

first.mean.SD.R8.semantic <- ddply(df.first.R8,c("semantic"),
                                   summarise,mean = mean(RT),
                                   sd = sd(RT),
                                   n = length(RT),
                                   se = sd/sqrt(n))

first.mean.SD.R8.semantic
#   semantic     mean       sd   n       se
# 1        1 24.02365 75.10854 888 2.520478
# 2        2 21.63851 67.65799 888 2.270454

all.regions <- rbind(x,y,z,w,p,q,r,s) # Wrap it up
all.regions

# Since the legend is alphabetical order, we have to change it as written in the paper..

attributes(all.regions$condition) # Check the type of the variable 'condition'
# $levels
# [1] "ma-ma"   "ma-nma"  "nma-ma"  "nma-nma"
# 
# $class
# [1] "factor"

# Change the order of the legend
all.regions.1 <- transform(all.regions,
                           condition.ordered = factor(condition, levels = c("ma-ma", "nma-ma", "nma-nma", "ma-nma")))
str(all.regions.1)

first.regions.graph.color.1 <- ggplot(data=all.regions.1, aes(x=Region, y=mean, group=condition.ordered)) +
  geom_line(aes(color=condition.ordered), size=1.25) +
  geom_point(aes(color=condition.ordered), size=4) +
  theme_minimal() +
  ggtitle("일차 읽기 시간 (ms)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),    
        legend.text = element_text(size=15), 
        legend.key.size = unit(2, "cm"),
        axis.text.x = element_text(size=20, face='bold'),
        axis.text.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face='bold', hjust=0.5))

first.regions.graph.color.1    


first.regions.graph.color.1  + scale_color_discrete(name="Cond", 
                                                    labels = c("Cond1(無有)", "Cond2(無無)", "Cond3(有有)", "Cond4(有無)")) + 
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, by = 50))

# Export it with 1000 x 700 size