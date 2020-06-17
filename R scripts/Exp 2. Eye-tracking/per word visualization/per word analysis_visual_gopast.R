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
gopast.mean.SD.R2 <- ddply(df.gopast.R2,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
gopast.mean.SD.R2
#   condition     mean       sd   n        se
# 1     ma-ma 332.6828 237.7418 419 11.614449
# 2    ma-nma 309.9532 218.3506 438 10.433197
# 3    nma-ma 299.2876 210.4746 442 10.011254
# 4   nma-nma 288.0277 193.8613 437  9.273643

Region <- c("R2", "R2", "R2", "R2")

y <- cbind(gopast.mean.SD.R2, Region)
y

gopast.mean.SD.R2.semantic <- ddply(df.gopast.R2,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

gopast.mean.SD.R2.semantic
#   semantic     mean      sd   n       se
# 1        1 321.0660 228.187 857 7.794721
# 2        2 293.6896 202.349 879 6.825061

gopast.mean.SD.R3 <- ddply(df.gopast.R3,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
gopast.mean.SD.R3
#   condition     mean       sd   n       se
# 1     ma-ma 335.7870 260.9950 415 12.81174
# 2    ma-nma 375.4074 276.6028 420 13.49683
# 3    nma-ma 253.7825 212.2729 435 10.17770
# 4   nma-nma 251.5101 212.8210 437 10.18061

Region <- c("R3", "R3", "R3", "R3")

z <- cbind(gopast.mean.SD.R3, Region)
z

gopast.mean.SD.R3.semantic <- ddply(df.gopast.R3,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

gopast.mean.SD.R3.semantic
#   semantic     mean       sd   n       se
# 1        1 355.7158 269.5275 835 9.327385
# 2        2 252.6437 212.4287 872 7.193744

gopast.mean.SD.R4 <- ddply(df.gopast.R4,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
gopast.mean.SD.R4
#   condition     mean       sd   n       se
# 1     ma-ma 211.3706 237.0647 436 11.35334
# 2    ma-nma 189.0011 215.6391 435 10.33911
# 3    nma-ma 230.5430 241.1173 437 11.53420
# 4   nma-nma 220.8500 218.4222 434 10.48460

Region <- c("R4", "R4", "R4", "R4")

w <- cbind(gopast.mean.SD.R4, Region)
w

gopast.mean.SD.R4.semantic <- ddply(df.gopast.R4,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

gopast.mean.SD.R4.semantic
#   semantic     mean       sd   n       se
# 1        1 200.1987 226.7637 871 7.683594
# 2        2 225.7132 230.0077 871 7.793515

gopast.mean.SD.R5 <- ddply(df.gopast.R5,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
gopast.mean.SD.R5
#   condition     mean       sd   n       se
# 1     ma-ma 278.4694 192.1029 432 9.242556
# 2    ma-nma 293.3556 194.8064 441 9.276495
# 3    nma-ma 276.3970 182.9935 436 8.763798
# 4   nma-nma 269.1136 190.4992 435 9.133736

Region <- c("R5", "R5", "R5", "R5")

p <- cbind(gopast.mean.SD.R5, Region)
p


gopast.mean.SD.R5.semantic <- ddply(df.gopast.R5,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

gopast.mean.SD.R5.semantic
#   semantic     mean       sd   n       se
# 1        1 277.4285 187.4771 868 6.363388
# 2        2 281.3176 192.9510 876 6.519210

gopast.mean.SD.R6 <- ddply(df.gopast.R6,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
gopast.mean.SD.R6
#   condition     mean       sd   n       se
# 1     ma-ma 359.1341 250.1445 417 12.24963
# 2    ma-nma 314.4203 209.2905 428 10.11644
# 3    nma-ma 369.5960 236.7010 429 11.42803
# 4   nma-nma 343.4960 230.1769 428 11.12602

Region <- c("R6", "R6", "R6", "R6")

q <- cbind(gopast.mean.SD.R6, Region)
q

gopast.mean.SD.R6.semantic <- ddply(df.gopast.R6,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

gopast.mean.SD.R6.semantic
#   semantic     mean       sd   n       se
# 1        1 364.4392 243.3322 846 8.365933
# 2        2 328.9582 220.3337 856 7.530852

gopast.mean.SD.R7 <- ddply(df.gopast.R7,c("condition"),
                           summarise,mean = mean(RT),
                           sd = sd(RT),
                           n = length(RT),
                           se = sd/sqrt(n))
gopast.mean.SD.R7
#   condition     mean       sd   n       se
# 1     ma-ma 252.5108 176.9205 437 8.463253
# 2    ma-nma 222.4320 167.5746 438 8.007024
# 3    nma-ma 249.3514 178.8662 436 8.566136
# 4   nma-nma 220.8320 173.0663 438 8.269426

Region <- c("R7", "R7", "R7", "R7")

r <- cbind(gopast.mean.SD.R7, Region)
r

gopast.mean.SD.R7.semantic <- ddply(df.gopast.R7,c("semantic"),
                                    summarise,mean = mean(RT),
                                    sd = sd(RT),
                                    n = length(RT),
                                    se = sd/sqrt(n))

gopast.mean.SD.R7.semantic
#   semantic     mean       sd   n       se
# 1        1 250.9329 177.7998 873 6.017613
# 2        2 221.6320 170.2471 876 5.752116

all.regions <- rbind(y,z,w,p,q,r) # Wrap it up
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

gopast.regions.graph.color.1 <- ggplot(data=all.regions.1, aes(x=Region, y=mean, group=condition.ordered)) +
  geom_line(aes(color=condition.ordered), size=1.25) +
  geom_point(aes(color=condition.ordered), size=4) +
  theme_minimal() +
  ggtitle("역방향 누적 읽기 시간 (ms)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),    
        legend.text = element_text(size=15), 
        legend.key.size = unit(2, "cm"),
        axis.text.x = element_text(size=20, face='bold'),
        axis.text.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face='bold', hjust=0.5))

gopast.regions.graph.color.1    


gopast.regions.graph.color.1  + scale_color_discrete(name="Cond", 
                                                     labels = c("Cond1(無有)", "Cond2(無無)", "Cond3(有有)", "Cond4(有無)")) + 
  scale_y_continuous(limits = c(150, 400), breaks = seq(150, 400, by = 50))

# Export it with 1000 x 700 size
