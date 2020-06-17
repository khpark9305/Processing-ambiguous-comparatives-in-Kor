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
second.mean.SD.R1 <- ddply(df.second.R1,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
second.mean.SD.R1
#   condition     mean       sd   n       se
# 1     ma-ma  194.5829 256.3559 434  12.30548
# 2    ma-nma  207.2471 275.0201 433  13.21662
# 3    nma-ma  157.4240 223.0681 441  10.62229
# 4   nma-nma  153.2374 220.8258 438  10.55147

Region <- c("R1", "R1", "R1", "R1") # add a value as region

x <- cbind(second.mean.SD.R1, Region) # add the region column to second.mean.SD.R1 (the rest process is the same as below)
x

second.mean.SD.R1.semantic <- ddply(df.second.R1,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

second.mean.SD.R1.semnatic
#   semantic     mean       sd   n       se
# 1        1 200.9077 265.7631 867 9.025787
# 2        2 155.3379 221.8371 879 7.482380


second.mean.SD.R2 <- ddply(df.second.R2,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
second.mean.SD.R2
#   condition     mean       sd   n       se
# 1     ma-ma  346.4256 326.2198 390 16.51878
# 2    ma-nma  261.8305 293.4993 413 14.44216
# 3    nma-ma  263.4329 300.3910 432 14.45257
# 4   nma-nma  252.8677 287.4535 431 13.84615

Region <- c("R2", "R2", "R2", "R2")

y <- cbind(second.mean.SD.R2, Region)
y

second.mean.SD.R2.semantic <- ddply(df.second.R2,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

second.mean.SD.R2.semantic
#   semantic     mean       sd   n       se
# 1        1 302.9166 312.5054 803 11.02808
# 2        2 258.1564 293.8779 863 10.00372

second.mean.SD.R3 <- ddply(df.second.R3,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
second.mean.SD.R3
#   condition     mean       sd   n       se
# 1     ma-ma  284.7730 302.6984 423 14.71770
# 2    ma-nma  234.2640 266.2400 428 12.86920
# 3    nma-ma  169.6728 226.8285 437 10.85068
# 4   nma-nma  143.6273 218.5443 440 10.41869

Region <- c("R3", "R3", "R3", "R3")

z <- cbind(second.mean.SD.R3, Region)
z

second.mean.SD.R3.semantic <- ddply(df.second.R3,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

second.mean.SD.R3.semantic
#   semantic     mean       sd   n       se
# 1        1 259.3702 285.8968 851 9.800415
# 2        2 156.6055 222.9647 877 7.528982

second.mean.SD.R4 <- ddply(df.second.R4,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
second.mean.SD.R4
#   condition     mean       sd   n        se
# 1     ma-ma   61.65991 135.1327 444 6.413114
# 2    ma-nma   42.29955 126.8557 444 6.020304
# 3    nma-ma   60.60722 137.9320 443 6.553349
# 4   nma-nma   64.20946 142.1125 444 6.744362

Region <- c("R4", "R4", "R4", "R4")

w <- cbind(second.mean.SD.R4, Region)
w

second.mean.SD.R4.semantic <- ddply(df.second.R4,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

second.mean.SD.R4.semantic
#   semantic     mean       sd   n       se
# 1        1 51.97973 131.3433 888 4.407591
# 2        2 62.41037 139.9728 887 4.699827

second.mean.SD.R5 <- ddply(df.second.R5,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
second.mean.SD.R5
#   condition     mean       sd   n       se
# 1     ma-ma  276.4664 292.5179 446 13.85112
# 2    ma-nma  223.4700 259.1411 434 12.43917
# 3    nma-ma  277.6812 282.6607 436 13.53699
# 4   nma-nma  238.9558 261.0634 430 12.58960

Region <- c("R5", "R5", "R5", "R5")

p <- cbind(second.mean.SD.R5, Region)
p

second.mean.SD.R5.semantic <- ddply(df.second.R5,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

second.mean.SD.R5.semantic
#   semantic     mean       sd   n       se
# 1        1 269.5200 175.6888 898 5.862813
# 2        2 265.1819 170.7965 885 5.741261

second.mean.SD.R6 <- ddply(df.second.R6,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
second.mean.SD.R6
#   condition     mean       sd   n       se
# 1     ma-ma  300.3641 292.7193 423 14.23250
# 2    ma-nma  191.6273 236.4608 432 11.37673
# 3    nma-ma  310.3271 293.1858 428 14.17167
# 4   nma-nma  253.8415 292.3809 429 14.11628

Region <- c("R6", "R6", "R6", "R6")

q <- cbind(second.mean.SD.R6, Region)
q

second.mean.SD.R6.semantic <- ddply(df.second.R6,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

second.mean.SD.R6.semantic
#   semantic     mean       sd   n       se
# 1        1 305.3749 292.8241 851 10.03788
# 2        2 222.6260 267.4605 861  9.11503

second.mean.SD.R7 <- ddply(df.second.R7,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
second.mean.SD.R7
#   condition     mean       sd   n       se
# 1     ma-ma  150.67273 231.3825 440  11.030729
# 2    ma-nma   96.31293 196.6211 441   9.362909
# 3    nma-ma  169.75621 234.4358 443  11.138381
# 4   nma-nma   93.29478 167.3105 441   7.967165

Region <- c("R7", "R7", "R7", "R7")

r <- cbind(second.mean.SD.R7, Region)
r

second.mean.SD.R7.semantic <- ddply(df.second.R7,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

second.mean.SD.R7.semantic
#   semantic      mean       sd   n       se
# 1        1 160.24689 232.9830 883 7.840502
# 2        2  94.80385 182.4576 882 6.143667


second.mean.SD.R8 <- ddply(df.second.R8,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
second.mean.SD.R8
#   condition     mean       sd  n       se
# 1     ma-ma 1.461712 21.62038 444 1.026058
# 2    ma-nma 3.788288 27.48339 444 1.304304
# 3    nma-ma 2.463964 41.24048 444 1.957187
# 4   nma-nma 3.700450 33.37465 444 1.583891

Region <- c("R8", "R8", "R8", "R8")

s <- cbind(second.mean.SD.R8, Region)
s

second.mean.SD.R8.semantic <- ddply(df.second.R8,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

second.mean.SD.R8.semantic
#   semantic     mean       sd   n       se
# 1        1 1.962838 32.91107 888 1.104423
# 2        2 3.744369 30.55405 888 1.025327

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

second.regions.graph.color.1 <- ggplot(data=all.regions.1, aes(x=Region, y=mean, group=condition.ordered)) +
  geom_line(aes(color=condition.ordered), size=1.25) +
  geom_point(aes(color=condition.ordered), size=4) +
  theme_minimal() +
  ggtitle("이차 읽기 시간 (ms)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),    
        legend.text = element_text(size=15), 
        legend.key.size = unit(2, "cm"),
        axis.text.x = element_text(size=20, face='bold'),
        axis.text.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face='bold', hjust=0.5))

second.regions.graph.color.1    


second.regions.graph.color.1  + scale_color_discrete(name="Cond", 
                                                    labels = c("Cond1(無有)", "Cond2(無無)", "Cond3(有有)", "Cond4(有無)")) + 
  scale_y_continuous(limits = c(0, 350), breaks = seq(0, 350, by = 50))

# Export it with 1000 x 700 size
