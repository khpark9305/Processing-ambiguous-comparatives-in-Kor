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
total.mean.SD.R1 <- ddply(df.total.R1,c("condition"),
                          summarise,mean = mean(RT),
                          sd = sd(RT),
                          n = length(RT),
                          se = sd/sqrt(n))
total.mean.SD.R1
# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 377.6831 261.4268 421 12.74116
# 2    ma-nma 402.8005 273.3712 421 13.32330
# 3    nma-ma 354.4253 242.2464 434 11.62820
# 4   nma-nma 348.4365 250.7957 430 12.09444

str(total.mean.SD.R1)

Region <- c("R1", "R1", "R1", "R1") # add a value as region

x <- cbind(total.mean.SD.R1, Region) # add the region column to total.mean.SD.R1 (the rest process is the same as below)
x

total.mean.SD.R1.semantic <- ddply(df.total.R1,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

total.mean.SD.R1.semantic
#   semantic     mean       sd   n       se
# 1        1 390.2418 267.6018 842 9.222167
# 2        2 351.4448 246.4136 864 8.383162

total.mean.SD.R2 <- ddply(df.total.R2,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
total.mean.SD.R2
# Fixation time less than 1200
# condition     mean       sd   n       se
# 1     ma-ma 556.9639 318.1096 360 16.76585
# 2    ma-nma 476.0844 287.7262 391 14.55094
# 3    nma-ma 473.3409 289.3288 408 14.32391
# 4   nma-nma 464.7483 278.3226 408 13.77902

Region <- c("R2", "R2", "R2", "R2")

y <- cbind(total.mean.SD.R2, Region)
y

total.mean.SD.R2.semantic <- ddply(df.total.R2,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

total.mean.SD.R2.semantic
#   semantic     mean       sd   n        se
# 1        1 514.8549 305.1585 751 11.135393
# 2        2 469.0446 283.7374 816  9.932796


total.mean.SD.R3 <- ddply(df.total.R3,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
total.mean.SD.R3
# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 474.4383 286.3227 402 14.28048
# 2    ma-nma 469.1855 278.7213 414 13.69841
# 3    nma-ma 354.8615 256.7131 431 12.36543
# 4   nma-nma 327.2968 240.5432 434 11.54644

Region <- c("R3", "R3", "R3", "R3")

z <- cbind(total.mean.SD.R3, Region)
z

total.mean.SD.R3.semantic <- ddply(df.total.R3,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

total.mean.SD.R3.semantic
#   semantic     mean       sd   n       se
# 1        1 471.7733 282.3304 816 9.883542
# 2        2 341.0313 248.9697 865 8.465224

total.mean.SD.R4 <- ddply(df.total.R4,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
total.mean.SD.R4
# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 209.3812 206.6952 442 9.831486
# 2    ma-nma 174.2109 173.7953 442 8.266599
# 3    nma-ma 222.1559 194.3986 440 9.267589
# 4   nma-nma 228.8835 209.5671 443 9.956832

Region <- c("R4", "R4", "R4", "R4")

w <- cbind(total.mean.SD.R4, Region)
w

total.mean.SD.R4.semantic <- ddply(df.total.R4,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

total.mean.SD.R4.semantic
#   semantic     mean       sd   n       se
# 1        1 191.7960 191.6563 884 6.446099
# 2        2 225.5311 202.0644 883 6.800008


total.mean.SD.R5 <- ddply(df.total.R5,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
total.mean.SD.R5
# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 510.6572 304.5569 421 14.84320
# 2    ma-nma 478.4924 289.8802 421 14.12790
# 3    nma-ma 508.0688 303.2777 417 14.85157
# 4   nma-nma 458.6240 283.9995 416 13.92422

Region <- c("R5", "R5", "R5", "R5")

p <- cbind(total.mean.SD.R5, Region)
p

total.mean.SD.R5.semantic <- ddply(df.total.R5,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

total.mean.SD.R5.semantic
#   semantic     mean       sd   n        se
# 1        1 509.3692 303.7422 838 10.492601
# 2        2 468.6176 286.9730 837  9.919238

total.mean.SD.R6 <- ddply(df.total.R6,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
total.mean.SD.R6
# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 533.1347 286.9464 395 14.43784
# 2    ma-nma 438.6291 244.7626 422 11.91485
# 3    nma-ma 550.1511 283.5022 401 14.15742
# 4   nma-nma 490.3818 273.7679 406 13.58687

Region <- c("R6", "R6", "R6", "R6")

q <- cbind(total.mean.SD.R6, Region)
q

total.mean.SD.R6.semantic <- ddply(df.total.R6,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

total.mean.SD.R6.semantic
#   semantic     mean       sd   n        se
# 1        1 541.7070 285.1642 796 10.107376
# 2        2 464.0054 260.5224 828  9.053778

total.mean.SD.R7 <- ddply(df.total.R7,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
total.mean.SD.R7
# Fixation time less than 1200
#   condition     mean       sd   n       se
# 1     ma-ma 371.1296 256.6183 432 12.34655
# 2    ma-nma 292.2858 228.6823 431 11.01524
# 3    nma-ma 396.7171 263.2722 438 12.57963
# 4   nma-nma 299.5450 226.0948 440 10.77865

Region <- c("R7", "R7", "R7", "R7")

r <- cbind(total.mean.SD.R7, Region)
r

total.mean.SD.R7.semantic <- ddply(df.total.R7,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

total.mean.SD.R7.semantic
#   semantic     mean       sd   n       se
# 1        1 384.0116 260.1550 870 8.820081
# 2        2 295.9529 227.2771 871 7.700991

total.mean.SD.R8 <- ddply(df.total.R8,c("condition"),
                    summarise,mean = mean(RT),
                    sd = sd(RT),
                    n = length(RT),
                    se = sd/sqrt(n))
total.mean.SD.R8
# fixation time less than 1200
#  condition     mean       sd   n       se
# 1     ma-ma 27.62635 79.97567 444 3.795478
# 2    ma-nma 23.10495 78.82028 444 3.740645
# 3    nma-ma 24.48694 92.80414 444 4.404290
# 4   nma-nma 27.78581 83.97447 444 3.985252

Region <- c("R8", "R8", "R8", "R8")

s <- cbind(total.mean.SD.R8, Region)
s

total.mean.SD.R8.semantic <- ddply(df.total.R8,c("semantic"),
                             summarise,mean = mean(RT),
                             sd = sd(RT),
                             n = length(RT),
                             se = sd/sqrt(n))

total.mean.SD.R8.semantic
#   semantic     mean       sd   n       se
# 1        1 26.05664 86.59310 888 2.905874
# 2        2 25.44538 81.42592 888 2.732475  

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
                               ggtitle("총 읽기 시간 (ms)") +
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
                               scale_y_continuous(breaks = seq(0, 600, by = 50))

# Export it with 1000 x 700 size

### Dummies...

# first.regions.graph.shape <- ggplot(data=all.regions, aes(x=Region, y=mean, group=condition)) +
#                              geom_line(aes(linetype=condition), size=1) +
#                              geom_point(aes(shape=condition), size=3) +
#                              theme_minimal() +
#                              theme(legend.position = "bottom",
#                              legend.title = element_blank(),    
#                              legend.text = element_text(size=15), 
#                              legend.key.size = unit(2, "cm"),
#                              axis.text.x = element_text(size=15),
#                              axis.text.y = element_text(size=15),
#                              axis.title.x = element_blank(),
#                              axis.title.y = element_blank())
# first.regions.graph.shape