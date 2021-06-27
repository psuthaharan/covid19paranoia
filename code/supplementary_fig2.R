### Analysis: Investigate differences in behavior and performance between task versions - non-social and social
###
### Description: Perform three-way mixed ANOVA
###                 two between-subjects factor: group, task
###                 one within-subjects factor: block
### Figure: Supplementary 2
### Written by: Praveen Suthaharan

# clear environment
rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/revisions/data")

# load libraries
library(dplyr)
library(ggpubr)
source("C:/Pandemic_2020/revisions/code/theme_publication.R") # source: https://rpubs.com/Koundy/71792
library(rstatix)
library(BayesFactor) # install.packages("BayesFactor")

# read data
dat <- read.csv('pandemicPRL.csv')


# subset for pre-lockdown data
dat_prepandemic <- dat %>%
  dplyr::filter((dat$dataset == "elife2020" | dat$dataset == "pandemic") &
                  dat$period == "prelockdown")


# calculate paranoia group - nonsocial/social
# extract scid from prelockdown, nonsocial task
nonsocial_group <- as.data.frame(dat_prepandemic[which(dat_prepandemic$task_type == "nonsocial"),]$scid_group)
colnames(nonsocial_group) <- "group"

# calculate gpts from prelockdown, social task
social_dat <- dat_prepandemic[which(dat_prepandemic$task_type == "social"),]
social_dat$paranoia_group <- ifelse(rowSums(social_dat[, grepl("rgpts_per", names(social_dat))], na.rm = TRUE) > 10, "high","low")
social_group <- as.data.frame(social_dat$paranoia_group)
colnames(social_group) <- "group"

# combine paranoia grouping into single column
dat_prepandemic$paranoia_group <- rbind(nonsocial_group, social_group)

# extract relevant features
dat_prepandemic_df <- data.frame(task = dat_prepandemic$task_type,
                                 group = dat_prepandemic$paranoia_group,
                                 points_earned = dat_prepandemic$points_earned,
                                 dat_prepandemic[, grepl("reversals_", names(dat_prepandemic))],
                                 dat_prepandemic[, grepl("wsr_", names(dat_prepandemic))],
                                 dat_prepandemic[, grepl("lsr_", names(dat_prepandemic))])



####################################################################################

# two-way ANOVA: points earned
dat_prepandemic_points_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                        task = dat_prepandemic_df$task,
                                        group = dat_prepandemic_df$group,
                                        points = dat_prepandemic_df$points_earned
)


# Supplementary Figure 1B_points_nonsocial
dat_prepandemic_points_df$group <- factor(dat_prepandemic_points_df$group, levels = c("low","high"))

fig1b_points <- ggplot(dat_prepandemic_points_df, aes(x = group,
                                                      y= points,
                                                      fill = as.factor(task))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("nonsocial" = "#506870", "social" = "#506870"))


fig1b_points + theme_Publication() + theme(axis.title.y = element_blank(),
                                           axis.title.x = element_blank(),
                                           axis.text = element_blank(), 
                                           axis.line = element_line(colour="black", size = 1.5),
                                           axis.ticks = element_line(colour="black", size = 1.5),
                                           legend.text = element_blank(),
                                           legend.position = "none")


# two-way ANOVA: points earned
aov2_points_earned <- aov(points ~ group * task, data = dat_prepandemic_points_df)
summary(aov2_points_earned)
Anova(aov2_points_earned, type="III")


# compute Bayes Factor: reversals achieved
ttestBF(dat_prepandemic_points_df[which(dat_prepandemic_points_df$task == "nonsocial"),]$points,
        dat_prepandemic_points_df[which(dat_prepandemic_points_df$task == "social"),]$points)



####################################################################################

# three-way mixed ANOVA: reversals achieved
dat_prepandemic_reversals_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                           task = dat_prepandemic_df$task,
                                           group = dat_prepandemic_df$group,
                                           dat_prepandemic_df[, grepl("reversals_", names(dat_prepandemic_df))][1:2]
)

# gather block scores into long format
dat_prepandemic_reversals_df <- dat_prepandemic_reversals_df %>%
  gather(key = "block", value = "reversals", reversals_block1, reversals_block2) %>%
  convert_as_factor(id, block)


# Supplementary Figure 1B_reversals_nonsocial
dat_prepandemic_reversals_nonsocial_df <- dat_prepandemic_reversals_df[which(dat_prepandemic_reversals_df$task == "nonsocial"),]
dat_prepandemic_reversals_nonsocial_df$group <- factor(dat_prepandemic_reversals_nonsocial_df$group, levels = c("low","high"))

fig1b_nonsocial_reversals <- ggplot(dat_prepandemic_reversals_nonsocial_df, aes(x = group,
                                                                                y= reversals,
                                                                                fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("reversals_block1" = "#708090", "reversals_block2" = "#2F4F4F"))


fig1b_nonsocial_reversals + theme_Publication() + theme(axis.title.y = element_blank(),
                                                        axis.title.x = element_blank(),
                                                        axis.text = element_blank(), 
                                                        axis.line = element_line(colour="black", size = 1.5),
                                                        axis.ticks = element_line(colour="black", size = 1.5),
                                                        legend.text = element_blank(),
                                                        legend.position = "none")


# Supplementary Figure 1B_reversals_social
dat_prepandemic_reversals_social_df <- dat_prepandemic_reversals_df[which(dat_prepandemic_reversals_df$task == "social"),]
dat_prepandemic_reversals_social_df$group <- factor(dat_prepandemic_reversals_social_df$group, levels = c("low","high"))

fig1b_social_reversals <- ggplot(dat_prepandemic_reversals_social_df, aes(x = group,
                                                                          y= reversals,
                                                                          fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("reversals_block1" = "#708090", "reversals_block2" = "#2F4F4F"))


fig1b_social_reversals + theme_Publication() + theme(axis.title.y = element_blank(),
                                                     axis.title.x = element_blank(),
                                                     axis.text = element_blank(), 
                                                     axis.line = element_line(colour="black", size = 1.5),
                                                     axis.ticks = element_line(colour="black", size = 1.5),
                                                     legend.text = element_blank(),
                                                     legend.position = "none")




# ANOVA: reversals achieved
prepandemic_reversals_aov <- anova_test(
  data = dat_prepandemic_reversals_df, dv = reversals, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_reversals_aov)


# compute Bayes Factor: reversals achieved
ttestBF(dat_prepandemic_reversals_df[which(dat_prepandemic_reversals_df$task == "nonsocial"),]$reversals,
        dat_prepandemic_reversals_df[which(dat_prepandemic_reversals_df$task == "social"),]$reversals)



####################################################################################


# three-way mixed ANOVA: WSR
dat_prepandemic_wsr_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                     task = dat_prepandemic_df$task,
                                     group = dat_prepandemic_df$group,
                                     dat_prepandemic_df[, grepl("wsr_", names(dat_prepandemic_df))]
)

# gather block scores into long format
dat_prepandemic_wsr_df <- dat_prepandemic_wsr_df %>%
  gather(key = "block", value = "wsr", wsr_block1, wsr_block2) %>%
  convert_as_factor(id, block)


# Figure 1C_wsr_nonsocial
dat_prepandemic_wsr_nonsocial_df <- dat_prepandemic_wsr_df[which(dat_prepandemic_wsr_df$task == "nonsocial"),]
dat_prepandemic_wsr_nonsocial_df$group <- factor(dat_prepandemic_wsr_nonsocial_df$group, levels = c("low","high"))

fig1c_nonsocial_wsr <- ggplot(dat_prepandemic_wsr_nonsocial_df, aes(x = group,
                                                                    y= wsr,
                                                                    fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("wsr_block1" = "#708090", "wsr_block2" = "#2F4F4F"))


fig1c_nonsocial_wsr + theme_Publication() + theme(axis.title.y = element_blank(),
                                                  axis.title.x = element_blank(),
                                                  axis.text = element_blank(), 
                                                  axis.line = element_line(colour="black", size = 1.5),
                                                  axis.ticks = element_line(colour="black", size = 1.5),
                                                  legend.text = element_blank(),
                                                  legend.position = "none")


# Figure 1C_wsr_social
dat_prepandemic_wsr_social_df <- dat_prepandemic_wsr_df[which(dat_prepandemic_wsr_df$task == "social"),]
dat_prepandemic_wsr_social_df$group <- factor(dat_prepandemic_wsr_social_df$group, levels = c("low","high"))

fig1c_social_wsr <- ggplot(dat_prepandemic_wsr_social_df, aes(x = group,
                                                              y= wsr,
                                                              fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("wsr_block1" = "#708090", "wsr_block2" = "#2F4F4F"))


fig1c_social_wsr + theme_Publication() + theme(axis.title.y = element_blank(),
                                               axis.title.x = element_blank(),
                                               axis.text = element_blank(), 
                                               axis.line = element_line(colour="black", size = 1.5),
                                               axis.ticks = element_line(colour="black", size = 1.5),
                                               legend.text = element_blank(),
                                               legend.position = "none")




# ANOVA: WSR
prepandemic_wsr_aov <- anova_test(
  data = dat_prepandemic_wsr_df, dv = wsr, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_wsr_aov)


# compute Bayes Factor: WSR
ttestBF(dat_prepandemic_wsr_df[which(dat_prepandemic_wsr_df$task == "nonsocial"),]$wsr,
        dat_prepandemic_wsr_df[which(dat_prepandemic_wsr_df$task == "social"),]$wsr)


####################################################################################

# three-way mixed ANOVA: LSR
dat_prepandemic_lsr_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                     task = dat_prepandemic_df$task,
                                     group = dat_prepandemic_df$group,
                                     dat_prepandemic_df[, grepl("lsr_", names(dat_prepandemic_df))]
)

# gather block scores into long format
dat_prepandemic_lsr_df <- dat_prepandemic_lsr_df %>%
  gather(key = "block", value = "lsr", lsr_block1, lsr_block2) %>%
  convert_as_factor(id, block)

# Figure 1C_lsr_nonsocial
dat_prepandemic_lsr_nonsocial_df <- dat_prepandemic_lsr_df[which(dat_prepandemic_lsr_df$task == "nonsocial"),]
dat_prepandemic_lsr_nonsocial_df$group <- factor(dat_prepandemic_lsr_nonsocial_df$group, levels = c("low","high"))

fig1c_nonsocial_lsr <- ggplot(dat_prepandemic_lsr_nonsocial_df, aes(x = group,
                                                                    y= lsr,
                                                                    fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("lsr_block1" = "#708090", "lsr_block2" = "#2F4F4F"))


fig1c_nonsocial_lsr + theme_Publication() + theme(axis.title.y = element_blank(),
                                                  axis.title.x = element_blank(),
                                                  axis.text = element_blank(), 
                                                  axis.line = element_line(colour="black", size = 1.5),
                                                  axis.ticks = element_line(colour="black", size = 1.5),
                                                  legend.text = element_blank(),
                                                  legend.position = "none")



# Figure 1C_lsr_social
dat_prepandemic_lsr_social_df <- dat_prepandemic_lsr_df[which(dat_prepandemic_lsr_df$task == "social"),]
dat_prepandemic_lsr_social_df$group <- factor(dat_prepandemic_lsr_social_df$group, levels = c("low","high"))

fig1c_social_lsr <- ggplot(dat_prepandemic_lsr_social_df, aes(x = group,
                                                              y= lsr,
                                                              fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("lsr_block1" = "#708090", "lsr_block2" = "#2F4F4F"))


fig1c_social_lsr + theme_Publication() + theme(axis.title.y = element_blank(),
                                               axis.title.x = element_blank(),
                                               axis.text = element_blank(), 
                                               axis.line = element_line(colour="black", size = 1.5),
                                               axis.ticks = element_line(colour="black", size = 1.5),
                                               legend.text = element_blank(),
                                               legend.position = "none")



# ANOVA: LSR
prepandemic_lsr_aov <- anova_test(
  data = dat_prepandemic_lsr_df, dv = lsr, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_lsr_aov)

# compute Bayes Factor: LSR
ttestBF(dat_prepandemic_lsr_df[which(dat_prepandemic_lsr_df$task == "nonsocial"),]$lsr,
        dat_prepandemic_lsr_df[which(dat_prepandemic_lsr_df$task == "social"),]$lsr)


