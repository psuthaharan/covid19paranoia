################################################################################################
################################################################################################
################################################################################################
rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/revisions/data")


# load libraries/other packages
library(dplyr)
library(tidyverse)
library(ggpubr)
source("C:/Pandemic_2020/revisions/code/theme_publication.R") # source: https://rpubs.com/Koundy/71792
library(gridExtra)
library(DT)

# read in data
dat <- read.csv("pandemicPRL.csv", 
                stringsAsFactors = FALSE)


# subset for replication data
dat_replication <- dat[which(dat$dataset == "replication"),]



# calculate paranoia scores
dat_replication$paranoia_score <- rowMeans(dat_replication[, grepl("rgpts_", names(dat_replication))], na.rm = TRUE)

# calculate paranoia grouping
dat_replication$paranoia_group <- ifelse(rowSums(dat_replication[, grepl("rgpts_per", names(dat_replication))], na.rm = TRUE) > 10, "high","low")



dat_replication_df <- data.frame(task = dat_replication$task_type,
                                 sabotage = dat_replication$sabotage_decks_avatars + 3,
                                 paranoia = dat_replication$paranoia_score)


# nonsocial: sabotage vs paranoia correlation
dat_replication_nonsocial_df <- dat_replication_df[which(dat_replication_df$task == "nonsocial"),]
p1 <- ggplot(dat_replication_nonsocial_df, aes(x=paranoia, y=sabotage)) +
  geom_point(shape=16,color="#595757",alpha=0.4) + #stat_cor(label.x=2.5, label.y = 0) + 
  geom_smooth(method="lm" , color="#659ec7", fill="#add8e6", size=1.5, alpha=0.4) + 
  labs(title ="",x="Paranoia",y="Sabotage")

p1 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.text = element_blank(), 
                                  axis.line = element_line(colour="black", size = 1.5),
                                  axis.ticks = element_line(colour="black", size = 1.5),
                                  legend.text = element_blank())



# social: sabotage vs paranoia correlation
dat_replication_social_df <- dat_replication_df[which(dat_replication_df$task == "social"),]
p2 <- ggplot(dat_replication_social_df, aes(x=paranoia, y=sabotage)) +
  geom_point(shape=16,color="#595757",alpha=0.4) + #stat_cor(label.x=2.5, label.y = 0) + 
  geom_smooth(method="lm" , color="#659ec7", fill="#add8e6", size=1.5, alpha=0.4) + 
  labs(title ="",x="Paranoia",y="Sabotage")

p2 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.text = element_blank(), 
                                  axis.line = element_line(colour="black", size = 1.5),
                                  axis.ticks = element_line(colour="black", size = 1.5),
                                  legend.text = element_blank())


