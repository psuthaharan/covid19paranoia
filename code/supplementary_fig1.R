rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/revisions/data")

# load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(devtools)
#library(easyGgplot2)
library(openintro)
library(plotrix)
library(gridExtra)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(Rmisc)

library(cowplot)
#library(dplyr)
library(readr)



source("C:/Pandemic_2020/revisions/code/theme_publication.R") # source: https://rpubs.com/Koundy/71792


# read data
pandemic <- read.csv('pandemicPRL.csv')

# subset for pandemic data
pandemic_dat <- pandemic[which(pandemic$dataset == "pandemic"),] 

## calculate anxiety scores (normalized)
# extract relevant columns to calculate anxiety score
pandemic_dat$anxiety_score <- rowMeans(pandemic_dat[, grepl("bai_", names(pandemic_dat))], na.rm = TRUE)
#pandemic_dat$anxiety_normalized <- (pandemic_dat$anxiety_score-min(pandemic_dat$anxiety_score, na.rm = TRUE))/(max(pandemic_dat$anxiety_score, na.rm = TRUE)-min(pandemic_dat$anxiety_score, na.rm = TRUE))

# remove row (i.e., subject) that contains NaN (i.e., skipped/declined questions)
pandemic_dat <- pandemic_dat[-c(which(is.na(pandemic_dat$anxiety_score))),] # only 1 subject removed

## calculate depression scores (normalized)
# extract relevant columns to calculate depression score
pandemic_dat$depression_score <- rowMeans(pandemic_dat[, grepl("bdi_", names(pandemic_dat))], na.rm = TRUE)
#pandemic_dat$depression_normalized <- (pandemic_dat$depression_score-min(pandemic_dat$depression_score, na.rm = TRUE))/(max(pandemic_dat$depression_score, na.rm = TRUE)-min(pandemic_dat$depression_score, na.rm = TRUE))


## calculate paranoia scores (normalized)
# extract relevant columns to calculate paranoia score
pandemic_dat$paranoia_score <- rowMeans(pandemic_dat[, grepl("rgpts_", names(pandemic_dat))], na.rm = TRUE)
#pandemic_dat$paranoia_normalized <- (pandemic_dat$paranoia_score-min(pandemic_dat$paranoia_score, na.rm = TRUE))/(max(pandemic_dat$paranoia_score, na.rm = TRUE)-min(pandemic_dat$paranoia_score, na.rm = TRUE))


# calculate paranoia grouping
pandemic_dat$paranoia_group <- ifelse(rowSums(pandemic_dat[, grepl("rgpts_per", names(pandemic_dat))], na.rm = TRUE) > 10, "high","low")



# order pandemic period variable
pandemic_dat$period <- factor(pandemic_dat$period, levels = c("prelockdown",
                                                              "lockdown",
                                                              "postlockdown"))


# Conduct ANOVA tests for each group
# group 1: anxiety
#boxplot(pandemic_df$anxiety ~ pandemic_df$period)
ANOVA1 <- aov(pandemic_dat$anxiety_score ~ pandemic_dat$period)
summary(ANOVA1) 
Anova(ANOVA1, type = "II") # significant; p = 0.01145 *

etaSquared(ANOVA1, type = 2, anova = TRUE)

# group 2: depression
#boxplot(pandemic_df$depression ~ pandemic_df$period)
ANOVA2 <- aov(pandemic_dat$depression_score ~ pandemic_dat$period)
summary(ANOVA2) 
Anova(ANOVA2, type = "II") # insignificant; p = 0.19

# group 3: paranoia
#boxplot(pandemic_df$paranoia ~ pandemic_df$period)
ANOVA3 <- aov(pandemic_dat$paranoia_score ~ pandemic_dat$period)
summary(ANOVA3) 
Anova(ANOVA3, type = "II") # significant; p = 8.834e-07 ***


pandemic_dat_df <- data.frame(period = pandemic_dat$period,
                              pandemic_dat[,grepl("_score", names(pandemic_dat))]
)
colnames(pandemic_dat_df) <- c("period","anxiety","depression","paranoia")


pandemic_df <- pandemic_dat_df %>%
  gather(key = "group",
         value = "score", anxiety, depression, paranoia)



sumrepdat <- summarySE(pandemic_df, measurevar = "score", groupvars=c("group", "period"))



supplementary_fig1 <- ggplot(sumrepdat, aes(x = period, y= score, fill = as.factor(group))) +
  geom_bar(stat="identity", color = "black", lwd=1.2, alpha = c(0.6,0.8,1.0,
                                                                0.6,0.8,1.0,
                                                                0.6,0.8,1.0),
           position = position_dodge(0.9)) +
  geom_errorbar(data=sumrepdat, aes(x=period, ymin=score-se, ymax=score+se), width=0.4, colour="black", alpha=0.9,
                position = position_dodge(0.9),
                size=2) +
  scale_fill_manual("",values = c("#4A6D7A",
                                  "#0063A5",
                                  "#DA1F23"))

supplementary_fig1 + theme_Publication() + theme(axis.title.y = element_blank(),
                                                 axis.title.x = element_blank(),
                                                 axis.text = element_blank(), 
                                                 axis.line = element_line(colour="black", size = 1.5),
                                                 axis.ticks = element_line(colour="black", size = 1.5),
                                                 legend.text = element_blank(),
                                                 legend.position = "none")



