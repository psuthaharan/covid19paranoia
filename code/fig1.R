### Analysis: Investigate differences in belief updating between task versions - non-social and social
###
### Description: Perform three-way mixed ANOVA
###                 two between-subjects factor: group, task
###                 one within-subjects factor: block
### Figure: 1B
### Written by: Praveen Suthaharan

# clear environment
rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/revisions/data")

# load libraries
library(dplyr)
library(ggpubr) # to use ggboxplot()
source("C:/Pandemic_2020/revisions/code/theme_publication.R") # source: https://rpubs.com/Koundy/71792
library(rstatix) # to use anova_test() and get_anova_table()

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

# calculate rgpts from prelockdown, social task
social_dat <- dat_prepandemic[which(dat_prepandemic$task_type == "social"),]
social_dat$paranoia_group <- ifelse(rowSums(social_dat[, grepl("rgpts_per", names(social_dat))], na.rm = TRUE) > 10, "high","low")
social_group <- as.data.frame(social_dat$paranoia_group)
colnames(social_group) <- "group"

# combine paranoia grouping into single column
dat_prepandemic$paranoia_group <- rbind(nonsocial_group, social_group)

# extract belief-updating features
dat_prepandemic_df <- data.frame(task = dat_prepandemic$task_type,
                                 group = dat_prepandemic$paranoia_group,
                                 dat_prepandemic[, grepl("mu03_", names(dat_prepandemic))],
                                 dat_prepandemic[, grepl("omega3_", names(dat_prepandemic))],
                                 dat_prepandemic[, grepl("mu02_", names(dat_prepandemic))],
                                 dat_prepandemic[, grepl("omega2_", names(dat_prepandemic))],
                                 dat_prepandemic[, grepl("kappa2_", names(dat_prepandemic))])



###################################################################################

# three-way mixed ANOVA: mu03
dat_prepandemic_mu03_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                     task = dat_prepandemic_df$task,
                                     group = dat_prepandemic_df$group,
                                     dat_prepandemic_df[, grepl("mu03_", names(dat_prepandemic_df))][1:2]
)

# gather block scores into long format
dat_prepandemic_mu03_df <- dat_prepandemic_mu03_df %>%
  gather(key = "block", value = "mu03", mu03_1, mu03_2) %>%
  convert_as_factor(id, block)

dat_prepandemic_mu03_df$group <- factor(dat_prepandemic_mu03_df$group, levels = c("low","high"))

# Figure 1B_mu03_nonsocial
dat_prepandemic_mu03_nonsocial_df <- dat_prepandemic_mu03_df[which(dat_prepandemic_mu03_df$task == "nonsocial"),]
dat_prepandemic_mu03_nonsocial_df$group <- factor(dat_prepandemic_mu03_nonsocial_df$group, levels = c("low","high"))

fig1d_nonsocial_mu03 <- ggplot(dat_prepandemic_mu03_nonsocial_df, aes(x = group,
                                                                      y= mu03,
                                                                      fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("mu03_1" = "#708090", "mu03_2" = "#2F4F4F"))


fig1d_nonsocial_mu03 + theme_Publication() + theme(axis.title.y = element_blank(),
                                                   axis.title.x = element_blank(),
                                                   axis.text = element_blank(), 
                                                   axis.line = element_line(colour="black", size = 1.5),
                                                   axis.ticks = element_line(colour="black", size = 1.5),
                                                   legend.text = element_blank(),
                                                   legend.position = "none")



# Figure 1D_mu03_social
dat_prepandemic_mu03_social_df <- dat_prepandemic_mu03_df[which(dat_prepandemic_mu03_df$task == "social"),]
dat_prepandemic_mu03_social_df$group <- factor(dat_prepandemic_mu03_social_df$group, levels = c("low","high"))

fig1d_social_mu03 <- ggplot(dat_prepandemic_mu03_social_df, aes(x = group,
                                                               y= mu03,
                                                               fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("mu03_1" = "#708090", "mu03_2" = "#2F4F4F"))


fig1d_social_mu03 + theme_Publication() + theme(axis.title.y = element_blank(),
                                                axis.title.x = element_blank(),
                                                axis.text = element_blank(), 
                                                axis.line = element_line(colour="black", size = 1.5),
                                                axis.ticks = element_line(colour="black", size = 1.5),
                                                legend.text = element_blank(),
                                                legend.position = "none")


# mixed anova
prepandemic_mu03_aov <- anova_test(
  data = dat_prepandemic_mu03_df, dv = mu03, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_mu03_aov)

# We observe neither a significant three-way interaction nor two-way interaction;
# however, we see a group main effect and block main effect. Therefore, we proceed
# to conduct simple simple main effects

# simple simple main effect of group
group_effect <- dat_prepandemic_mu03_df %>%
  group_by(task,block) %>%
  anova_test(dv = mu03, wid=id, between = group)
# results suggest significant group effect in the nonsocial task

# simple simple main effect of block
block_effect <- dat_prepandemic_mu03_df %>%
  group_by(task,group) %>%
  anova_test(dv = mu03, wid=id, within = block)
# results suggest significant block effect

# Significant simple simple main effects are followed by simple simple comparisons
# Fit pairwise comparisons: group
pwc_group <- dat_prepandemic_mu03_df %>%
  group_by(task, block) %>%
  pairwise_t_test(mu03 ~ group, p.adjust.method = "BH") %>%
  select(-p, -p.signif)
pwc_group %>% filter(task == "nonsocial") # focus on the results of nonsocial


pwc_block <- dat_prepandemic_mu03_df %>%
  group_by(task, group) %>%
  pairwise_t_test(mu03 ~ block, p.adjust.method = "BH") %>%
  select(-p, -p.signif)
pwc_block

####################################################################################

# three-way mixed ANOVA: omega3
dat_prepandemic_omega3_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                      task = dat_prepandemic_df$task,
                                      group = dat_prepandemic_df$group,
                                      dat_prepandemic_df[, grepl("omega3_", names(dat_prepandemic_df))][1:2]
)

# gather block scores into long format
dat_prepandemic_omega3_df <- dat_prepandemic_omega3_df %>%
  gather(key = "block", value = "omega3", omega3_1, omega3_2) %>%
  convert_as_factor(id, block)

# Figure 1D_omega3_nonsocial
dat_prepandemic_omega3_nonsocial_df <- dat_prepandemic_omega3_df[which(dat_prepandemic_omega3_df$task == "nonsocial"),]
dat_prepandemic_omega3_nonsocial_df$group <- factor(dat_prepandemic_omega3_nonsocial_df$group, levels = c("low","high"))

fig1d_nonsocial_omega3 <- ggplot(dat_prepandemic_omega3_nonsocial_df, aes(x = group,
                                                                          y= omega3,
                                                                          fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("omega3_1" = "#708090", "omega3_2" = "#2F4F4F"))


fig1d_nonsocial_omega3 + theme_Publication() + theme(axis.title.y = element_blank(),
                                                     axis.title.x = element_blank(),
                                                     axis.text = element_blank(), 
                                                     axis.line = element_line(colour="black", size = 1.5),
                                                     axis.ticks = element_line(colour="black", size = 1.5),
                                                     legend.text = element_blank(),
                                                     legend.position = "none")



# Figure 1D_omega3_social
dat_prepandemic_omega3_social_df <- dat_prepandemic_omega3_df[which(dat_prepandemic_omega3_df$task == "social"),]
dat_prepandemic_omega3_social_df$group <- factor(dat_prepandemic_omega3_social_df$group, levels = c("low","high"))

fig1d_social_omega3 <- ggplot(dat_prepandemic_omega3_social_df, aes(x = group,
                                                                    y= omega3,
                                                                    fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("omega3_1" = "#708090", "omega3_2" = "#2F4F4F"))


fig1d_social_omega3 + theme_Publication() + theme(axis.title.y = element_blank(),
                                                  axis.title.x = element_blank(),
                                                  axis.text = element_blank(), 
                                                  axis.line = element_line(colour="black", size = 1.5),
                                                  axis.ticks = element_line(colour="black", size = 1.5),
                                                  legend.text = element_blank(),
                                                  legend.position = "none")


# anova
prepandemic_omega3_aov <- anova_test(
  data = dat_prepandemic_omega3_df, dv = omega3, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_omega3_aov)


####################################################################################

# three-way mixed ANOVA: mu02
dat_prepandemic_mu02_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                      task = dat_prepandemic_df$task,
                                      group = dat_prepandemic_df$group,
                                      dat_prepandemic_df[, grepl("mu02_", names(dat_prepandemic_df))][1:2]
)

# gather block scores into long format
dat_prepandemic_mu02_df <- dat_prepandemic_mu02_df %>%
  gather(key = "block", value = "mu02", mu02_1, mu02_2) %>%
  convert_as_factor(id, block)

# Figure 1D_mu02_nonsocial
dat_prepandemic_mu02_nonsocial_df <- dat_prepandemic_mu02_df[which(dat_prepandemic_mu02_df$task == "nonsocial"),]
dat_prepandemic_mu02_nonsocial_df$group <- factor(dat_prepandemic_mu02_nonsocial_df$group, levels = c("low","high"))

fig1d_nonsocial_mu02 <- ggplot(dat_prepandemic_mu02_nonsocial_df, aes(x = group,
                                                                      y= mu02,
                                                                      fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("mu02_1" = "#708090", "mu02_2" = "#2F4F4F"))


fig1d_nonsocial_mu02 + theme_Publication() + theme(axis.title.y = element_blank(),
                                                   axis.title.x = element_blank(),
                                                   axis.text = element_blank(), 
                                                   axis.line = element_line(colour="black", size = 1.5),
                                                   axis.ticks = element_line(colour="black", size = 1.5),
                                                   legend.text = element_blank(),
                                                   legend.position = "none")



# Figure 1D_mu02_social
dat_prepandemic_mu02_social_df <- dat_prepandemic_mu02_df[which(dat_prepandemic_mu02_df$task == "social"),]
dat_prepandemic_mu02_social_df$group <- factor(dat_prepandemic_mu02_social_df$group, levels = c("low","high"))

fig1d_social_mu02 <- ggplot(dat_prepandemic_mu02_social_df, aes(x = group,
                                                                y= mu02,
                                                                fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("mu02_1" = "#708090", "mu02_2" = "#2F4F4F"))


fig1d_social_mu02 + theme_Publication() + theme(axis.title.y = element_blank(),
                                                axis.title.x = element_blank(),
                                                axis.text = element_blank(), 
                                                axis.line = element_line(colour="black", size = 1.5),
                                                axis.ticks = element_line(colour="black", size = 1.5),
                                                legend.text = element_blank(),
                                                legend.position = "none")


# anova
prepandemic_mu02_aov <- anova_test(
  data = dat_prepandemic_mu02_df, dv = mu02, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_mu02_aov)



####################################################################################

# three-way mixed ANOVA: omega2
dat_prepandemic_omega2_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                        task = dat_prepandemic_df$task,
                                        group = dat_prepandemic_df$group,
                                        dat_prepandemic_df[, grepl("omega2_", names(dat_prepandemic_df))][1:2]
)

# gather block scores into long format
dat_prepandemic_omega2_df <- dat_prepandemic_omega2_df %>%
  gather(key = "block", value = "omega2", omega2_1, omega2_2) %>%
  convert_as_factor(id, block)

# Figure 1D_omega2_nonsocial
dat_prepandemic_omega2_nonsocial_df <- dat_prepandemic_omega2_df[which(dat_prepandemic_omega2_df$task == "nonsocial"),]
dat_prepandemic_omega2_nonsocial_df$group <- factor(dat_prepandemic_omega2_nonsocial_df$group, levels = c("low","high"))

fig1d_nonsocial_omega2 <- ggplot(dat_prepandemic_omega2_nonsocial_df, aes(x = group,
                                                                          y= omega2,
                                                                          fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("omega2_1" = "#708090", "omega2_2" = "#2F4F4F"))


fig1d_nonsocial_omega2 + theme_Publication() + theme(axis.title.y = element_blank(),
                                                     axis.title.x = element_blank(),
                                                     axis.text = element_blank(), 
                                                     axis.line = element_line(colour="black", size = 1.5),
                                                     axis.ticks = element_line(colour="black", size = 1.5),
                                                     legend.text = element_blank(),
                                                     legend.position = "none")



# Figure 1D_omega2_social
dat_prepandemic_omega2_social_df <- dat_prepandemic_omega2_df[which(dat_prepandemic_omega2_df$task == "social"),]
dat_prepandemic_omega2_social_df$group <- factor(dat_prepandemic_omega2_social_df$group, levels = c("low","high"))

fig1d_social_omega2 <- ggplot(dat_prepandemic_omega2_social_df, aes(x = group,
                                                                    y= omega2,
                                                                    fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("omega2_1" = "#708090", "omega2_2" = "#2F4F4F"))


fig1d_social_omega2 + theme_Publication() + theme(axis.title.y = element_blank(),
                                                  axis.title.x = element_blank(),
                                                  axis.text = element_blank(), 
                                                  axis.line = element_line(colour="black", size = 1.5),
                                                  axis.ticks = element_line(colour="black", size = 1.5),
                                                  legend.text = element_blank(),
                                                  legend.position = "none")


# anova
prepandemic_omega2_aov <- anova_test(
  data = dat_prepandemic_omega2_df, dv = omega2, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_omega2_aov)



####################################################################################

# three-way mixed ANOVA: kappa
dat_prepandemic_kappa_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                        task = dat_prepandemic_df$task,
                                        group = dat_prepandemic_df$group,
                                        dat_prepandemic_df[, grepl("kappa2_", names(dat_prepandemic_df))][1:2]
)

# gather block scores into long format
dat_prepandemic_kappa_df <- dat_prepandemic_kappa_df %>%
  gather(key = "block", value = "kappa", kappa2_1, kappa2_2) %>%
  convert_as_factor(id, block)

# Figure 1D_kappa_nonsocial
dat_prepandemic_kappa_nonsocial_df <- dat_prepandemic_kappa_df[which(dat_prepandemic_kappa_df$task == "nonsocial"),]
dat_prepandemic_kappa_nonsocial_df$group <- factor(dat_prepandemic_kappa_nonsocial_df$group, levels = c("low","high"))

fig1d_nonsocial_kappa <- ggplot(dat_prepandemic_kappa_nonsocial_df, aes(x = group,
                                                                        y= kappa,
                                                                        fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("kappa2_1" = "#708090", "kappa2_2" = "#2F4F4F"))


fig1d_nonsocial_kappa + theme_Publication() + theme(axis.title.y = element_blank(),
                                                    axis.title.x = element_blank(),
                                                    axis.text = element_blank(), 
                                                    axis.line = element_line(colour="black", size = 1.5),
                                                    axis.ticks = element_line(colour="black", size = 1.5),
                                                    legend.text = element_blank(),
                                                    legend.position = "none")



# Figure 1D_kappa_social
dat_prepandemic_kappa_social_df <- dat_prepandemic_kappa_df[which(dat_prepandemic_kappa_df$task == "social"),]
dat_prepandemic_kappa_social_df$group <- factor(dat_prepandemic_kappa_social_df$group, levels = c("low","high"))

fig1d_social_kappa <- ggplot(dat_prepandemic_kappa_social_df, aes(x = group,
                                                                  y= kappa,
                                                                  fill = as.factor(block))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("kappa2_1" = "#708090", "kappa2_2" = "#2F4F4F"))


fig1d_social_kappa + theme_Publication() + theme(axis.title.y = element_blank(),
                                                  axis.title.x = element_blank(),
                                                  axis.text = element_blank(), 
                                                  axis.line = element_line(colour="black", size = 1.5),
                                                  axis.ticks = element_line(colour="black", size = 1.5),
                                                  legend.text = element_blank(),
                                                  legend.position = "none")


# anova
prepandemic_kappa_aov <- anova_test(
  data = dat_prepandemic_kappa_df, dv = kappa, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_kappa_aov)



########################################################################
########################################################################
########################################################################
########################################################################


# subset for replication data
dat_replication <- dat %>%
  dplyr::filter(dat$dataset == "replication")

# calculate paranoia score
dat_replication$paranoia_score <- rowMeans(dat_replication[, grepl("rgpts_", names(dat_replication))], na.rm = TRUE)
# calculate paranoia group
dat_replication$paranoia_group <- ifelse(rowSums(dat_replication[, grepl("rgpts_per", names(dat_replication))], na.rm = TRUE) > 10, "high","low")


# extract relevant features
dat_replication_df <- data.frame(task = dat_replication$task_type,
                                 group = dat_replication$paranoia_group,
                                 dat_replication[, grepl("wsr_", names(dat_replication))],
                                 dat_replication[, grepl("lsr_", names(dat_replication))],
                                 dat_replication[, grepl("mu03_", names(dat_replication))],
                                 dat_replication[, grepl("omega3_", names(dat_replication))],
                                 dat_replication[, grepl("mu02_", names(dat_replication))],
                                 dat_replication[, grepl("omega2_", names(dat_replication))],
                                 dat_replication[, grepl("kappa2_", names(dat_replication))])


####################################################################################

# three-way mixed ANOVA: WSR
dat_replication_wsr_df <- data.frame(id = 1:nrow(dat_replication_df),
                                     task = dat_replication_df$task,
                                     group = dat_replication_df$group,
                                     dat_replication_df[, grepl("wsr_", names(dat_replication_df))]
)

# gather block scores into long format
dat_replication_wsr_df <- dat_replication_wsr_df %>%
  gather(key = "block", value = "wsr", wsr_block1, wsr_block2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_replication_wsr <- ggboxplot(
  dat_replication_wsr_df, x = "group", y = "wsr",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_replication_wsr


# anova
replication_wsr_aov <- anova_test(
  data = dat_replication_wsr_df, dv = wsr, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(replication_wsr_aov)



####################################################################################

# three-way mixed ANOVA: LSR
dat_replication_lsr_df <- data.frame(id = 1:nrow(dat_replication_df),
                                     task = dat_replication_df$task,
                                     group = dat_replication_df$group,
                                     dat_replication_df[, grepl("lsr_", names(dat_replication_df))]
)

# gather block scores into long format
dat_replication_lsr_df <- dat_replication_lsr_df %>%
  gather(key = "block", value = "lsr", lsr_block1, lsr_block2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_replication_lsr <- ggboxplot(
  dat_replication_lsr_df, x = "group", y = "lsr",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_replication_lsr


# anova
replication_lsr_aov <- anova_test(
  data = dat_replication_lsr_df, dv = lsr, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(replication_lsr_aov)



####################################################################################

# three-way mixed ANOVA: mu03
dat_prepandemic_mu03_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                      task = dat_prepandemic_df$task,
                                      group = dat_prepandemic_df$group,
                                      dat_prepandemic_df[, grepl("mu03_", names(dat_prepandemic_df))][1:2]
)

# gather block scores into long format
dat_prepandemic_mu03_df <- dat_prepandemic_mu03_df %>%
  gather(key = "block", value = "mu03", mu03_1, mu03_2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_prepandemic_mu03 <- ggboxplot(
  dat_prepandemic_mu03_df, x = "group", y = "mu03",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_prepandemic_mu03


# anova
prepandemic_mu03_aov <- anova_test(
  data = dat_prepandemic_mu03_df, dv = mu03, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_mu03_aov)



####################################################################################

# three-way mixed ANOVA: omega3
dat_prepandemic_omega3_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                        task = dat_prepandemic_df$task,
                                        group = dat_prepandemic_df$group,
                                        dat_prepandemic_df[, grepl("omega3_", names(dat_prepandemic_df))][1:2]
)

# gather block scores into long format
dat_prepandemic_omega3_df <- dat_prepandemic_omega3_df %>%
  gather(key = "block", value = "omega3", omega3_1, omega3_2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_prepandemic_omega3 <- ggboxplot(
  dat_prepandemic_omega3_df, x = "group", y = "omega3",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_prepandemic_omega3


# anova
prepandemic_omega3_aov <- anova_test(
  data = dat_prepandemic_omega3_df, dv = omega3, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_omega3_aov)



####################################################################################

# three-way mixed ANOVA: mu02
dat_prepandemic_mu02_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                      task = dat_prepandemic_df$task,
                                      group = dat_prepandemic_df$group,
                                      dat_prepandemic_df[, grepl("mu02_", names(dat_prepandemic_df))][1:2]
)

# gather block scores into long format
dat_prepandemic_mu02_df <- dat_prepandemic_mu02_df %>%
  gather(key = "block", value = "mu02", mu02_1, mu02_2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_prepandemic_mu02 <- ggboxplot(
  dat_prepandemic_mu02_df, x = "group", y = "mu02",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_prepandemic_mu02


# anova
prepandemic_mu02_aov <- anova_test(
  data = dat_prepandemic_mu02_df, dv = mu02, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_mu02_aov)



####################################################################################

# three-way mixed ANOVA: omega2
dat_prepandemic_omega2_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                        task = dat_prepandemic_df$task,
                                        group = dat_prepandemic_df$group,
                                        dat_prepandemic_df[, grepl("omega2_", names(dat_prepandemic_df))][1:2]
)

# gather block scores into long format
dat_prepandemic_omega2_df <- dat_prepandemic_omega2_df %>%
  gather(key = "block", value = "omega2", omega2_1, omega2_2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_prepandemic_omega2 <- ggboxplot(
  dat_prepandemic_omega2_df, x = "group", y = "omega2",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_prepandemic_omega2


# anova
prepandemic_omega2_aov <- anova_test(
  data = dat_prepandemic_omega2_df, dv = omega2, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_omega2_aov)



####################################################################################

# three-way mixed ANOVA: kappa
dat_prepandemic_kappa_df <- data.frame(id = 1:nrow(dat_prepandemic_df),
                                       task = dat_prepandemic_df$task,
                                       group = dat_prepandemic_df$group,
                                       dat_prepandemic_df[, grepl("kappa2_", names(dat_prepandemic_df))][1:2]
)

# gather block scores into long format
dat_prepandemic_kappa_df <- dat_prepandemic_kappa_df %>%
  gather(key = "block", value = "kappa", kappa2_1, kappa2_2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_prepandemic_kappa <- ggboxplot(
  dat_prepandemic_kappa_df, x = "group", y = "kappa",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_prepandemic_kappa


# anova
prepandemic_kappa_aov <- anova_test(
  data = dat_prepandemic_kappa_df, dv = kappa, wid = id,
  within = block, between = c(task, group)
)
get_anova_table(prepandemic_kappa_aov)




########################################################################
########################################################################
########################################################################
########################################################################


# subset for pandemic data
dat_pandemic <- dat %>%
  dplyr::filter((dat$dataset == "elife2020" | dat$dataset == "pandemic") & (dat$period == "prelockdown" |
                                                                            dat$period == "lockdown" |
                                                                            dat$period == "postlockdown"))


# calculate paranoia group - nonsocial/social
# extract scid from prelockdown, nonsocial group
nonsocial_pre_group <- as.data.frame(dat_pandemic[which(dat_pandemic$dataset == "elife2020"),]$scid_group)
colnames(nonsocial_pre_group) <- "group"

pandemic_dat <- dat_pandemic[-which(dat_pandemic$dataset == "elife2020"),]
pandemic_dat$paranoia_group <- ifelse(rowSums(pandemic_dat[, grepl("rgpts_per", names(pandemic_dat))], na.rm = TRUE) > 10, "high","low")

pandemic_group <- as.data.frame(pandemic_dat$paranoia_group)
colnames(pandemic_group) <- "group"

dat_pandemic$paranoia_group <- rbind(nonsocial_pre_group, pandemic_group)



# extract relevant features
dat_pandemic_df <- data.frame(period = dat_pandemic$period,
                                 task = dat_pandemic$task_type,
                                 group = dat_pandemic$paranoia_group,
                                 dat_pandemic[, grepl("wsr_", names(dat_pandemic))],
                                 dat_pandemic[, grepl("lsr_", names(dat_pandemic))],
                                 dat_pandemic[, grepl("mu03_", names(dat_pandemic))],
                                 dat_pandemic[, grepl("omega3_", names(dat_pandemic))],
                                 dat_pandemic[, grepl("mu02_", names(dat_pandemic))],
                                 dat_pandemic[, grepl("omega2_", names(dat_pandemic))],
                                 dat_pandemic[, grepl("kappa2_", names(dat_pandemic))])


####################################################################################

# three-way mixed ANOVA: WSR
dat_pandemic_wsr_df <- data.frame(id = 1:nrow(dat_pandemic_df),
                                     period = dat_pandemic_df$period,
                                     task = dat_pandemic_df$task,
                                     group = dat_pandemic_df$group,
                                     dat_pandemic_df[, grepl("wsr_", names(dat_pandemic_df))]
)

# gather block scores into long format
dat_pandemic_wsr_df <- dat_pandemic_wsr_df %>%
  gather(key = "block", value = "wsr", wsr_block1, wsr_block2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_pandemic_wsr <- ggboxplot(
  dat_pandemic_wsr_df, x = "period", y = "wsr",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_pandemic_wsr


# anova
pandemic_wsr_aov <- anova_test(
  data = dat_pandemic_wsr_df, dv = wsr, wid = id,
  within = block, between = c(task, period)
)
get_anova_table(pandemic_wsr_aov)



####################################################################################

# three-way mixed ANOVA: LSR
dat_pandemic_lsr_df <- data.frame(id = 1:nrow(dat_pandemic_df),
                                  period = dat_pandemic_df$period,
                                  task = dat_pandemic_df$task,
                                  group = dat_pandemic_df$group,
                                  dat_pandemic_df[, grepl("lsr_", names(dat_pandemic_df))]
)

# gather block scores into long format
dat_pandemic_lsr_df <- dat_pandemic_lsr_df %>%
  gather(key = "block", value = "lsr", lsr_block1, lsr_block2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_pandemic_lsr <- ggboxplot(
  dat_pandemic_lsr_df, x = "period", y = "lsr",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_pandemic_lsr


# anova
pandemic_lsr_aov <- anova_test(
  data = dat_pandemic_lsr_df, dv = lsr, wid = id,
  within = block, between = c(task, period)
)
get_anova_table(pandemic_lsr_aov)



####################################################################################

# three-way mixed ANOVA: mu03
dat_pandemic_mu03_df <- data.frame(id = 1:nrow(dat_pandemic_df),
                                  period = dat_pandemic_df$period,
                                  task = dat_pandemic_df$task,
                                  group = dat_pandemic_df$group,
                                  dat_pandemic_df[, grepl("mu03_", names(dat_pandemic_df))][1:2]
)

# gather block scores into long format
dat_pandemic_mu03_df <- dat_pandemic_mu03_df %>%
  gather(key = "block", value = "mu03", mu03_1, mu03_2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_pandemic_mu03 <- ggboxplot(
  dat_pandemic_mu03_df, x = "period", y = "mu03",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_pandemic_mu03


# anova
pandemic_mu03_aov <- anova_test(
  data = dat_pandemic_mu03_df, dv = mu03, wid = id,
  within = block, between = c(task, period)
)
get_anova_table(pandemic_mu03_aov)



####################################################################################

# three-way mixed ANOVA: omega3
dat_pandemic_omega3_df <- data.frame(id = 1:nrow(dat_pandemic_df),
                                   period = dat_pandemic_df$period,
                                   task = dat_pandemic_df$task,
                                   group = dat_pandemic_df$group,
                                   dat_pandemic_df[, grepl("omega3_", names(dat_pandemic_df))][1:2]
)

# gather block scores into long format
dat_pandemic_omega3_df <- dat_pandemic_omega3_df %>%
  gather(key = "block", value = "omega3", omega3_1, omega3_2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_pandemic_omega3 <- ggboxplot(
  dat_pandemic_omega3_df, x = "period", y = "omega3",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_pandemic_omega3


# anova
pandemic_omega3_aov <- anova_test(
  data = dat_pandemic_omega3_df, dv = omega3, wid = id,
  within = block, between = c(task, period)
)
get_anova_table(pandemic_omega3_aov)



####################################################################################

# three-way mixed ANOVA: mu02
dat_pandemic_mu02_df <- data.frame(id = 1:nrow(dat_pandemic_df),
                                   period = dat_pandemic_df$period,
                                   task = dat_pandemic_df$task,
                                   group = dat_pandemic_df$group,
                                   dat_pandemic_df[, grepl("mu02_", names(dat_pandemic_df))][1:2]
)

# gather block scores into long format
dat_pandemic_mu02_df <- dat_pandemic_mu02_df %>%
  gather(key = "block", value = "mu02", mu02_1, mu02_2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_pandemic_mu02 <- ggboxplot(
  dat_pandemic_mu02_df, x = "period", y = "mu02",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_pandemic_mu02


# anova
pandemic_mu02_aov <- anova_test(
  data = dat_pandemic_mu02_df, dv = mu02, wid = id,
  within = block, between = c(task, period)
)
get_anova_table(pandemic_mu02_aov)



####################################################################################

# three-way mixed ANOVA: omega2
dat_pandemic_omega2_df <- data.frame(id = 1:nrow(dat_pandemic_df),
                                     period = dat_pandemic_df$period,
                                     task = dat_pandemic_df$task,
                                     group = dat_pandemic_df$group,
                                     dat_pandemic_df[, grepl("omega2_", names(dat_pandemic_df))][1:2]
)

# gather block scores into long format
dat_pandemic_omega2_df <- dat_pandemic_omega2_df %>%
  gather(key = "block", value = "omega2", omega2_1, omega2_2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_pandemic_omega2 <- ggboxplot(
  dat_pandemic_omega2_df, x = "period", y = "omega2",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_pandemic_omega2


# anova
pandemic_omega2_aov <- anova_test(
  data = dat_pandemic_omega2_df, dv = omega2, wid = id,
  within = block, between = c(task, period)
)
get_anova_table(pandemic_omega2_aov)



####################################################################################

# three-way mixed ANOVA: kappa
dat_pandemic_kappa_df <- data.frame(id = 1:nrow(dat_pandemic_df),
                                     period = dat_pandemic_df$period,
                                     task = dat_pandemic_df$task,
                                     group = dat_pandemic_df$group,
                                     dat_pandemic_df[, grepl("kappa2_", names(dat_pandemic_df))][1:2]
)

# gather block scores into long format
dat_pandemic_kappa_df <- dat_pandemic_kappa_df %>%
  gather(key = "block", value = "kappa", kappa2_1, kappa2_2) %>%
  convert_as_factor(id, block)

# simple visualization
bxp_pandemic_kappa <- ggboxplot(
  dat_pandemic_kappa_df, x = "period", y = "kappa",
  color = "block", palette = "jco",
  facet.by = "task", short.panel.labs = FALSE
)
bxp_pandemic_kappa


# anova
pandemic_kappa_aov <- anova_test(
  data = dat_pandemic_kappa_df, dv = kappa, wid = id,
  within = block, between = c(task, period)
)
get_anova_table(pandemic_kappa_aov)
