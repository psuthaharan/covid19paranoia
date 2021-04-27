### Analysis: Investigate paranoia, state proactivity, task behavior and belief updating during a pandemic
###
### Description: Perform two-way ANOVA to observe any interaction effects between proactivity and period
###                 
###                 
### Figure: 2
### Written by: Praveen Suthaharan

# clear environment
rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/revisions/data")

# load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
#library(devtools)
#library(easyGgplot2)
#library(openintro)
#library(plotrix)
#library(gridExtra)
#library(tidyverse)
library(rstatix)
library(ggpattern)
library(lsr)
library(cowplot)
library(readr)
source("C:\\Pandemic_2020\\revisions\\code\\rainCloud\\RainCloudPlots-master\\tutorial_R\\R_rainclouds.R") # source: https://github.com/RainCloudPlots/RainCloudPlots/tree/master/tutorial_R
source("C:\\Pandemic_2020\\revisions\\code\\rainCloud\\RainCloudPlots-master\\tutorial_R\\summarySE.R") # source: https://github.com/RainCloudPlots/RainCloudPlots/tree/master/tutorial_R
source("C:\\Pandemic_2020\\revisions\\code\\rainCloud\\RainCloudPlots-master\\tutorial_R\\simulateData.R") # source: https://github.com/RainCloudPlots/RainCloudPlots/tree/master/tutorial_R
source("C:/Pandemic_2020/revisions/code/theme_publication.R") # source: https://rpubs.com/Koundy/71792


# read data
dat <- read.csv('pandemicPRL.csv')

# subset for pandemic data
dat_pandemic <- dat[which(dat$dataset == "pandemic"),] 

## calculate paranoia score
dat_pandemic$paranoia_score <- rowMeans(dat_pandemic[, grepl("rgpts_", names(dat_pandemic))], na.rm = TRUE)
# calculate paranoia grouping
dat_pandemic$paranoia_group <- ifelse(rowSums(dat_pandemic[, grepl("rgpts_per", names(dat_pandemic))], na.rm = TRUE) > 10, "high","low")


# order pandemic period variable
dat_pandemic$period <- factor(dat_pandemic$period, levels = c("prelockdown",
                                                              "lockdown",
                                                              "postlockdown"))



####################################################################
########################## FIGURE 2A ###############################
####################################################################


# Conduct one-way ANOVA for paranoia
aov1 <- aov(dat_pandemic$paranoia_score ~ dat_pandemic$period)
summary(aov1) 
aov1_fig2a <- Anova(aov1, type = "II") # significant; p = 5.99e-07 ***
aov1_fig2a

# calculate partial eta squared
etaSquared(aov1, type = 2, anova = TRUE) # eta_sq_part = 0.053

# create a dataframe with only paranoia scores and period
dat_pandemic_df <- data.frame(period = dat_pandemic$period,
                              dat_pandemic[,grepl("_score", names(dat_pandemic))]
)
colnames(dat_pandemic_df) <- c("period","paranoia")

# add a group variable (there will be only 1 - i.e., paranoia group - but this code will accommodate for other groups)
df_fig2a <- dat_pandemic_df %>%
  gather(key = "group",
         value = "score", paranoia)


# compute summary statistics of paranoia by period
sumrepdat <- summarySE(df_fig2a, measurevar = "score", groupvars=c("group", "period"))



# Figure 2A
fig_2a <- ggplot(df_fig2a, aes(x = period, 
                                  y = score)
) +
  geom_flat_violin(aes(fill = period),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(period)-.275, y = score, colour = period),position = position_jitter(width = .05), size = 1.5, shape = 21)+
  geom_boxplot(aes(x = period, y = score, fill=period),
               width = .3,
               lwd = 1,
               colour = "black")+
scale_colour_manual(values = c("#f2a097","#ed7669","#e84c3b"))+ 
  scale_fill_manual(values = c("#f2a097","#ed7669","#e84c3b"))+ 
  scale_x_discrete(labels=c("prelockdown" = "Pre-Lockdown", "lockdown" = "Lockdown",
                            "postlockdown" = "Post-Lockdown")) +
  labs(title="", x="Pandemic period", y="Paranoia")
 

fig_2a + theme_Publication() + theme(axis.title.y = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.text = element_blank(), 
                                     axis.line = element_line(colour="black", size = 1.5),
                                     axis.ticks = element_line(colour="black", size = 1.5),
                                     legend.text = element_blank())  


####################################################################
####################### FIGURE 2B ##################################
####################################################################

# create dataframe with relevant features 
df_fig2b <- data.frame(period = dat_pandemic$period,
                          paranoia_score = dat_pandemic$paranoia_score,
                          paranoia_group = dat_pandemic$paranoia_group,
                          sabotage = dat_pandemic$sabotage_decks_avatars,
                          proactivity = dat_pandemic$state_proactivity,
                          mask_mandate = dat_pandemic$state_mask_mandate,
                          wsr = rowMeans(cbind(dat_pandemic$wsr_block1,dat_pandemic$wsr_block2)),
                          lsr = rowMeans(cbind(dat_pandemic$lsr_block1,dat_pandemic$lsr_block2)),
                          kappa = rowMeans(cbind(dat_pandemic$kappa2_1,dat_pandemic$kappa2_2)),
                          mu02 = rowMeans(cbind(dat_pandemic$mu02_1,dat_pandemic$mu02_2)),
                          mu03 = rowMeans(cbind(dat_pandemic$mu03_1,dat_pandemic$mu03_2)),
                          omega2 = rowMeans(cbind(dat_pandemic$omega2_1,dat_pandemic$omega2_2)),
                          omega3 = rowMeans(cbind(dat_pandemic$omega3_1,dat_pandemic$omega3_2))
)

# correctly order the factor variables
df_fig2b$period <- factor(df_fig2b$period, levels = c("prelockdown", "lockdown", "postlockdown"))
df_fig2b$paranoia_group <- factor(df_fig2b$paranoia_group, levels = c("low", "high"))

# create proactivity labeling based on median
df_fig2b$proactivity <- ifelse(df_fig2b$proactivity <= median(df_fig2b$proactivity,na.rm = TRUE), "less proactive","more proactive")

# create mask_mandate labeling
df_fig2b$mask_mandate <- ifelse(df_fig2b$mask_mandate == 0, "recommended","required")
df_fig2b$mask_mandate <- factor(df_fig2b$mask_mandate, levels = c("recommended","required"))


## The following is how we labeled proactivity for each period:
# prelockdown: based on lockdown proactivity
# lockdown: based on lockdown proactivity
# postlockdown: based on mask_mandate 

# pre-lockdown follows lockdown proactivity levels, post-lockdown proactivity based on mask-mandate status
proactivity_prelockdown_lockdown <- as.data.frame(df_fig2b[which(df_fig2b$period == "prelockdown" | df_fig2b$period == "lockdown"),]$proactivity)
colnames(proactivity_prelockdown_lockdown) <- "proactivity"
proactivity_postlockdown <- as.data.frame(df_fig2b[which(df_fig2b$period == "postlockdown"),]$mask_mandate)
proactivity_postlockdown <- ifelse(proactivity_postlockdown == "recommended","less proactive","more proactive")
colnames(proactivity_postlockdown) <- "proactivity"

proactivity <- rbind(proactivity_prelockdown_lockdown, proactivity_postlockdown)



## The following is an alternative coding scheme we tried for each period:
# prelockdown: based on mask_mandate
# lockdown: based on lockdown proactivity
# postlockdown: based on mask_mandate 

# pre-lockdown follows mask_mandate status, post-lockdown proactivity based on mask_mandate status
proactivity_lock <- as.data.frame(df_fig2b[which(df_fig2b$period == "lockdown"),]$proactivity)
colnames(proactivity_lock) <- "proactivity_alternative"
proactivity_pre_post <- as.data.frame(df_fig2b[which(df_fig2b$period == "prelockdown" | df_fig2b$period == "postlockdown"),]$mask_mandate)
proactivity_pre_post <- ifelse(proactivity_pre_post == "recommended","less proactive","more proactive")
colnames(proactivity_pre_post) <- "proactivity_alternative"

proactivity_pre <- as.data.frame(proactivity_pre_post[1:130,])
colnames(proactivity_pre) <- "proactivity_alternative"
proactivity_pre_lock <- rbind(proactivity_pre,proactivity_lock)

proactivity_post <- as.data.frame(proactivity_pre_post[131:302,])
colnames(proactivity_post) <- "proactivity_alternative"

proactivity_alternative <- rbind(proactivity_pre_lock, proactivity_post)


# create new data frame with specific coding of proactivity
df <- data.frame(id = 1:nrow(df_fig2b),
                 period = df_fig2b$period,
                 proactivity,
                 proactivity_alternative,
                 sabotage = df_fig2b$sabotage,
                 paranoia_score = df_fig2b$paranoia_score,
                 paranoia_group = df_fig2b$paranoia_group,
                 wsr = df_fig2b$wsr,
                 lsr = df_fig2b$lsr,
                 mu02 = df_fig2b$mu02,
                 mu03 = df_fig2b$mu03,
                 kappa = df_fig2b$kappa,
                 omega2 = df_fig2b$omega2,
                 omega3 = df_fig2b$omega3)


fig2b_paranoia <- ggplot(df, aes(x = period,
                          y = paranoia_score,
                          fill=proactivity)) +
  geom_boxplot_pattern(aes(fill=proactivity),pattern=c("stripe","none",
                                                       "stripe","none",
                                                       "stripe","none"),pattern_colour = "black",alpha = c(0.6, 0.6,
                                                                                                           0.8, 0.8,
                                                                                                           1.0, 1.0), lwd = 1) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(0.5), show.legend = FALSE) +
  scale_fill_manual(values = c("#f2a097","#e84c3b"))+ 
  scale_x_discrete(labels=c("prelockdown" = "Pre-Lockdown", "lockdown" = "Lockdown",
                            "postlockdown" = "Post-Lockdown")) +
  labs(title="", x="Pandemic period", y="Paranoia") +  guides(fill = guide_legend(override.aes = list(pattern=c("stripe","none")))
                                                              )


fig2b_paranoia + theme_Publication() +  theme(axis.title.y = element_blank(),
                                                       axis.title.x = element_blank(),
                                                       axis.text = element_blank(), 
                                                       axis.line = element_line(colour="black", size = 1.5),
                                                       axis.ticks = element_line(colour="black", size = 1.5),
                                                       legend.text = element_blank(),
                                                       legend.position = "none")


# Two-way ANOVA
aov2_fig2b_paranoia <- aov(paranoia_score ~ proactivity * period, data = df)
summary(aov2_fig2b_paranoia)
Anova(aov2_fig2b_paranoia, type="III")

# calculate partial eta squared
etaSquared(aov2_fig2b_paranoia, type = 3, anova = TRUE)

# We observe a main effect of period [F(2,527) = 4.948, p=0.009, eta_sq_part = 0.018] and 
# a significant interaction between the effects of proactivity and period on 
# paranoia [F(2,527) = 4.785, p=0.009, eta_sq_part = 0.018]

TukeyHSD(aov2_fig2b_paranoia)


# two-sample t-test 
df_lockdown <- df[which(df$period == "lockdown"),]
t.test(df_lockdown$paranoia_score ~ df_lockdown$proactivity,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(paranoia_score ~ proactivity, data = df_lockdown)
# We observe a significant mean difference (mean[less_proactive]=0.773, mean[more_proactive]=0.468)
# of paranoia [t(229)=2.57, p=0.011, Cohen's d = 0.334, 95% CI=(0.071,0.539)] between
# less proactive and more proactive states 
# in lockdown




fig2b_wsr <- ggplot(df, aes(x = period,
                          y = wsr,
                          fill=proactivity)) +
  geom_boxplot_pattern(aes(fill=proactivity),pattern=c("stripe","none",
                                                       "stripe","none",
                                                       "stripe","none"),pattern_colour = "black",alpha = c(0.6, 0.6,
                                                                                                           0.8, 0.8,
                                                                                                           1.0, 1.0), lwd = 1
  ) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(0.5), show.legend = FALSE) +
  scale_fill_manual(values = c("#f2a097","#e84c3b"))+
  scale_x_discrete(labels=c("prelockdown" = "Pre-Lockdown", "lockdown" = "Lockdown",
                            "postlockdown" = "Post-Lockdown")) +
  labs(title="", x="Pandemic period", y="Paranoia") +  guides(fill = guide_legend(override.aes = list(pattern=c("stripe","none")))
  )


fig2b_wsr + theme_Publication() + theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank(),
                                        legend.position = "none")




aov2_wsr <- aov(wsr ~ proactivity * period, data = df)
summary(aov2_wsr)
Anova(aov2_wsr, type="III") 

etaSquared(aov2_wsr, type = 3, anova = TRUE)

# We observe a significant interaction between the effects of proactivity and period on 
# paranoia [F(2,527) = 8.747, p<0.001 (fdr corrected), eta_sq_part = 0.032]

df_lockdown <- df[which(df$period == "lockdown"),]
t.test(df_lockdown$wsr ~ df_lockdown$proactivity,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(wsr ~ proactivity, data = df_lockdown)
# We observe a significant mean difference 
# of paranoia [t(216)=2.73, p=0.007, Cohen's d = 0.351, 95% CI=(0.019,0.117)] between
# less proactive and more proactive states 
# in lockdown


df_postlockdown <- df[which(df$period == "postlockdown"),]
t.test(df_postlockdown$wsr ~ df_postlockdown$proactivity,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(wsr ~ proactivity, data = df_postlockdown)
# We observe a significant mean difference 
# of paranoia [t(67)=-2.39, p=0.02, Cohen's d = 0.483, 95% CI=(-0.164,-0.015)] between
# less proactive and more proactive states 
# in reopening



aov2_lsr <- aov(lsr ~ proactivity * period, data = df)
summary(aov2_lsr)
Anova(aov2_lsr, type="III") 

pvalues_behavior <- c(0.0001832, 0.04047)
pvalues_behavior_sorted <- sort(pvalues_behavior)
pvalues_behavior_sorted
p.adjust(pvalues_behavior_sorted, method = "BH")


fig2b_3 <- ggplot(df, aes(x = period,
                          y = mu03,
                          fill=proactivity)) +
  geom_boxplot_pattern(aes(fill=proactivity),pattern=c("stripe","none",
                                                       "stripe","none",
                                                       "stripe","none"),pattern_colour = "black",alpha = c(0.6, 0.6,
                                                                                                           0.8, 0.8,
                                                                                                           1.0, 1.0),
                       #width = .7,
                       lwd = 1#,
                       #position = position_dodge(width=0.8)
  ) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(0.5), show.legend = FALSE) +
  scale_fill_manual(values = c("#f2a097","#e84c3b"))+ #5c1a33
  scale_x_discrete(labels=c("prelockdown" = "Pre-Lockdown", "lockdown" = "Lockdown",
                            "postlockdown" = "Post-Lockdown")) +
  labs(title="", x="Pandemic period", y="Paranoia") +  guides(#pattern = guide_legend(override.aes = list(fill = "white")),
    fill = guide_legend(override.aes = list(pattern=c("stripe","none")))
  )


fig2b_3 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                       axis.title.x = element_blank(),
                                       axis.text = element_blank(), 
                                       axis.line = element_line(colour="black", size = 1.5),
                                       axis.ticks = element_line(colour="black", size = 1.5),
                                       legend.text = element_blank(),
                                       legend.position = "none")

aov2_mu03 <- aov(mu03 ~ proactivity * period, data = df)
summary(aov2_mu03)
Anova(aov2_mu03, type="III") 

etaSquared(aov2_mu03, type = 3, anova = TRUE)

# We observe a significant interaction between the effects of proactivity and period on 
# paranoia [F(2,527) = , p=0.001 (fdr corrected), eta_sq_part = 0.032]


df_lockdown <- df[which(df$period == "lockdown"),]
t.test(df_lockdown$mu03 ~ df_lockdown$proactivity,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(mu03 ~ proactivity, data = df_lockdown)

# We observe a significant mean difference 
# of paranoia [t(217)=4.22, p<0.001, Cohen's d = 0.561, 95% CI=(0.401,1.10)] between
# less proactive and more proactive states 
# in lockdown

aov2_omega3 <- aov(omega3 ~ proactivity * period, data = df)
summary(aov2_omega3) 
Anova(aov2_omega3, type="III") # p=0.1357
aov2_mu02 <- aov(mu02 ~ proactivity * period, data = df)
summary(aov2_mu02)
Anova(aov2_mu02, type="III") # p=0.02755
aov2_omega2 <- aov(omega2 ~ proactivity * period, data = df)
summary(aov2_omega2)
Anova(aov2_omega2, type="III") # p =0.08729
aov2_kappa <- aov(kappa ~ proactivity * period, data = df)
summary(aov2_kappa)
Anova(aov2_kappa, type="III") # p=0.02393

pvalues_belief <- c(0.0002066, 0.1357, 0.02755, 0.08729, 0.02393)
pvalues_belief_sorted <- sort(pvalues_belief)
pvalues_belief_sorted
p.adjust(pvalues_belief_sorted, method = "BH")

fig2b_3 <- fig2b_3 + theme_Publication() + theme(legend.direction = "horizontal",
                                                 legend.key.size= unit(2, "cm"),
                                                 legend.spacing.x = unit(4.0, 'cm'),
                                                 legend.text = element_blank())

# Using the cowplot package
legend <- cowplot::get_legend(fig2b_3)

grid.newpage()
grid.draw(legend)



#############################################
#############################################
########### ALTERNATIVE CODING ##############
#############################################
#############################################
fig2b_paranoia_alternative <- ggplot(df, aes(x = period,
                                 y = paranoia_score)) +
  geom_boxplot_pattern(aes(fill=proactivity_alternative),pattern=c("stripe","none",
                                                       "stripe","none",
                                                       "stripe","none"),pattern_colour = "black",alpha = c(0.6, 0.6,
                                                                                                           0.8, 0.8,
                                                                                                           1.0, 1.0), lwd = 1) +
  scale_fill_manual(values = c("#f2a097","#e84c3b"))+ 
  scale_x_discrete(labels=c("prelockdown" = "Pre-Lockdown", "lockdown" = "Lockdown",
                            "postlockdown" = "Post-Lockdown")) +
  labs(title="", x="Pandemic period", y="Paranoia") +  guides(fill = guide_legend(override.aes = list(pattern=c("stripe","none")))
  )


fig2b_paranoia_alternative + theme_Publication() +  theme(axis.title.y = element_blank(),
                                              axis.title.x = element_blank(),
                                              axis.text = element_blank(), 
                                              axis.line = element_line(colour="black", size = 1.5),
                                              axis.ticks = element_line(colour="black", size = 1.5),
                                              legend.text = element_blank(),
                                              legend.position = "none")

# Two-way ANOVA
aov2_fig2b_paranoia_alternative <- aov(paranoia_score ~ proactivity_alternative * period, data = df)
summary(aov2_fig2b_paranoia_alternative)
Anova(aov2_fig2b_paranoia_alternative, type="III")

# calculate partial eta squared
etaSquared(aov2_fig2b_paranoia_alternative, type = 3, anova = TRUE)

# We observe a main effect of period [F(2,527) = 6.154, p=0.0002, eta_sq_part = 0.023] and 
# a significant interaction between the effects of proactivity and period on 
# paranoia [F(2,527) = 5.099, p=0.006, eta_sq_part = 0.019]

TukeyHSD(aov2_fig2b_paranoia)


# two-sample t-test 
df_lockdown <- df[which(df$period == "lockdown"),]
t.test(df_lockdown$paranoia_score ~ df_lockdown$proactivity_alternative,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)
# We observe a significant mean difference 
# of paranoia [t(212)=2.69, p=0.008, Cohen's d = 0.354, 95% CI=(0.086,0.558)] between
# less proactive and more proactive states 
# in lockdown

cohensD(paranoia_score ~ proactivity_alternative, data = df_lockdown)


fig2b_wsr_alternative <- ggplot(df, aes(x = period,
                            y = wsr)) +
  geom_boxplot_pattern(aes(fill=proactivity_alternative),pattern=c("stripe","none",
                                                       "stripe","none",
                                                       "stripe","none"),pattern_colour = "black",alpha = c(0.6, 0.6,
                                                                                                           0.8, 0.8,
                                                                                                           1.0, 1.0), lwd = 1
  ) +
  scale_fill_manual(values = c("#f2a097","#e84c3b"))+
  scale_x_discrete(labels=c("prelockdown" = "Pre-Lockdown", "lockdown" = "Lockdown",
                            "postlockdown" = "Post-Lockdown")) +
  labs(title="", x="Pandemic period", y="Paranoia") +  guides(fill = guide_legend(override.aes = list(pattern=c("stripe","none")))
  )


fig2b_wsr_alternative + theme_Publication() + theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank(),
                                        legend.position = "none")




aov2_wsr_alternative <- aov(wsr ~ proactivity_alternative * period, data = df)
summary(aov2_wsr_alternative)
Anova(aov2_wsr_alternative, type="III") # F(2,527) = 4.972, p = 0.007

etaSquared(aov2_wsr_alternative, type = 3, anova = TRUE)

# We observe a significant interaction between the effects of proactivity and period on 
# paranoia [F(2,527) = 4.972, p=0.015 (fdr corrected), eta_sq_part = 0.019]

df_postlockdown <- df[which(df$period == "postlockdown"),]
t.test(df_postlockdown$wsr ~ df_postlockdown$proactivity_alternative,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

# We observe a significant mean difference 
# of paranoia [t(67)=-2.39, p=0.02, Cohen's d = 0.483, 95% CI=(-0.164,-0.015)] between
# less proactive and more proactive states 
# in reopening

cohensD(wsr ~ proactivity_alternative, data = df_postlockdown)

aov2_lsr_alternative <- aov(lsr ~ proactivity_alternative * period, data = df)
summary(aov2_lsr_alternative)
Anova(aov2_lsr_alternative, type="III") # F(2,527) = 3.86, p = 0.022

pvalues_behavior <- c(0.0001539418, 0.04802)
pvalues_behavior_sorted <- sort(pvalues_behavior)
pvalues_behavior_sorted
p.adjust(pvalues_behavior_sorted, method = "BH")


fig2b_mu03_alternative <- ggplot(df, aes(x = period,
                          y = mu03)) +
  geom_boxplot_pattern(aes(fill=proactivity_alternative),pattern=c("stripe","none",
                                                       "stripe","none",
                                                       "stripe","none"),pattern_colour = "black",alpha = c(0.6, 0.6,
                                                                                                           0.8, 0.8,
                                                                                                           1.0, 1.0),
                       #width = .7,
                       lwd = 1#,
                       #position = position_dodge(width=0.8)
  ) +
  #scale_colour_manual(values = c("#f2a097","#ed7669","#e84c3b"))+ #5c1a33
  scale_fill_manual(values = c("#f2a097","#e84c3b"))+ #5c1a33
  scale_x_discrete(labels=c("prelockdown" = "Pre-Lockdown", "lockdown" = "Lockdown",
                            "postlockdown" = "Post-Lockdown")) +
  labs(title="", x="Pandemic period", y="Paranoia") +  guides(#pattern = guide_legend(override.aes = list(fill = "white")),
    fill = guide_legend(override.aes = list(pattern=c("stripe","none")))
  )


fig2b_mu03_alternative + theme_Publication() +  theme(axis.title.y = element_blank(),
                                                      axis.title.x = element_blank(),
                                                      axis.text = element_blank(), 
                                                      axis.line = element_line(colour="black", size = 1.5),
                                                      axis.ticks = element_line(colour="black", size = 1.5),
                                                      legend.text = element_blank(),
                                                      legend.position = "none")

aov2_mu03_alternative <- aov(mu03 ~ proactivity_alternative * period, data = df)
summary(aov2_mu03_alternative)
Anova(aov2_mu03_alternative, type="III") # F(2,527) = 5.589, p = 0.004

etaSquared(aov2_mu03_alternative, type = 3, anova = TRUE)

# We observe a significant interaction between the effects of proactivity and period on 
# paranoia [F(2,527) = 5.589, p=0.02 (fdr corrected), eta_sq_part = 0.021]


df_lockdown <- df[which(df$period == "lockdown"),]
t.test(df_lockdown$mu03 ~ df_lockdown$proactivity_alternative,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

# We observe a significant mean difference 
# of paranoia [t(221)=3.03, p=0.003, Cohen's d = 0.399, 95% CI=(0.191,0.900)] between
# less proactive and more proactive states 
# in lockdown

cohensD(mu03 ~ proactivity_alternative, data = df_lockdown)

aov2_omega3_alternative <- aov(omega3 ~ proactivity_alternative * period, data = df)
summary(aov2_omega3_alternative) 
Anova(aov2_omega3_alternative, type="III") # p=0.2377
aov2_mu02_alternative <- aov(mu02 ~ proactivity_alternative * period, data = df)
summary(aov2_mu02_alternative)
Anova(aov2_mu02_alternative, type="III") # p=0.02772
aov2_omega2_alternative <- aov(omega2 ~ proactivity_alternative * period, data = df)
summary(aov2_omega2_alternative)
Anova(aov2_omega2_alternative, type="III") # p =0.08315
aov2_kappa_alternative <- aov(kappa ~ proactivity_alternative * period, data = df)
summary(aov2_kappa_alternative)
Anova(aov2_kappa_alternative, type="III") # p=0.01947

pvalues_belief <- c(0.0001663475, 0.2377, 0.02772, 0.08315, 0.01947)
pvalues_belief_sorted <- sort(pvalues_belief)
pvalues_belief_sorted
p.adjust(pvalues_belief_sorted, method = "BH")

fig2b_3 <- fig2b_3 + theme_Publication() + theme(legend.direction = "horizontal",
                                                 legend.key.size= unit(2, "cm"),
                                                 legend.spacing.x = unit(4.0, 'cm'),
                                                 legend.text = element_blank())

# Using the cowplot package
legend <- cowplot::get_legend(fig2b_3)

grid.newpage()
grid.draw(legend)






