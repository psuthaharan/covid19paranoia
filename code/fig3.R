### Analysis: Investigate sabotage beliefs and the effects of lockdown (social task)
###
### Description: Perform one-way anova to see change in sabotage beliefs across period, and
###              look at correlation between paranoia and sabotage belief,
###               and differences in behavior and beliefs between proactive states
### Figure: 3
### Written by: Praveen Suthaharan

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
library(weights)
library(ggpattern)

library(cowplot)
#library(dplyr)
library(readr)
source("C:\\Pandemic_2020\\revisions\\code\\rainCloud\\RainCloudPlots-master\\tutorial_R\\R_rainclouds.R")
source("C:\\Pandemic_2020\\revisions\\code\\rainCloud\\RainCloudPlots-master\\tutorial_R\\summarySE.R")
source("C:\\Pandemic_2020\\revisions\\code\\rainCloud\\RainCloudPlots-master\\tutorial_R\\simulateData.R")


source("C:/Pandemic_2020/revisions/code/theme_publication.R") # source: https://rpubs.com/Koundy/71792


# read data
dat <- read.csv('pandemicPRL.csv')

# subset for pandemic data
dat_pandemic <- dat %>%
  dplyr::filter(dataset == "pandemic")

# extract relevant features into dataframe
pandemic_df <- data.frame(period = dat_pandemic$period,
                          task_type = dat_pandemic$task_type,
                          proactivity = dat_pandemic$state_proactivity,
                          mask_mandate = dat_pandemic$state_mask_mandate,
                          sabotage = dat_pandemic$sabotage_decks_avatars +3
)

pandemic_df$period <- factor(pandemic_df$period, levels = c("prelockdown",
                                                                          "lockdown",
                                                                          "postlockdown"))

pandemic_df$proactivity <- ifelse(pandemic_df$proactivity <= median(pandemic_df$proactivity,na.rm = TRUE), "less proactive","more proactive")
pandemic_df$mask_mandate <- ifelse(pandemic_df$mask_mandate == 0, "recommended","required")
pandemic_df$mask_mandate <- factor(pandemic_df$mask_mandate, levels = c("recommended","required"))


# pre-lockdown follows lockdown proactivity levels, post-lockdown proactivity based on mask-mandate status
proactivity_prelockdown_lockdown <- as.data.frame(pandemic_df[which(pandemic_df$period == "prelockdown" | pandemic_df$period == "lockdown"),]$proactivity)
colnames(proactivity_prelockdown_lockdown) <- "proactivity"
proactivity_postlockdown <- as.data.frame(pandemic_df[which(pandemic_df$period == "postlockdown"),]$mask_mandate)
proactivity_postlockdown <- ifelse(proactivity_postlockdown == "recommended","less proactive","more proactive")
colnames(proactivity_postlockdown) <- "proactivity"

proactivity <- rbind(proactivity_prelockdown_lockdown, proactivity_postlockdown)

df <- data.frame(id = 1:nrow(pandemic_df),
                 period = pandemic_df$period,
                 proactivity,
                 sabotage = pandemic_df$sabotage)

pandemic_df_social <- pandemic_df[which(pandemic_df$task_type == "social"),]

aov2_sabotage <- aov(sabotage ~ proactivity * period, data = pandemic_df_social)
summary(aov2_sabotage)
Anova(aov2_sabotage, type="II")

# We observe a trend in period (p=0.061) and no main effect of proactivity or interaction effect

etaSquared(aov2_sabotage, type = 3, anova = TRUE)

# subset only social task (since that's where we ascertain sabotage)
pandemic_social_df <- pandemic_df[which(pandemic_df$task_type == "social"),]

pandemic_social_df$period <- factor(pandemic_social_df$period, levels = c("prelockdown",
                                                                          "lockdown",
                                                                          "postlockdown"))


# Conduct ANOVA tests for sabotage across period
#boxplot(pandemic_social_df$sabotage ~ pandemic_social_df$period)
ANOVA1 <- aov(pandemic_social_df$sabotage ~ pandemic_social_df$period)
summary(ANOVA1) # trend; p = 0.0554
Anova(ANOVA1, type = "II")


# Welch's t-test for sabotage between pre- and post-lockdown
pandemic_social_prepostlockdown <- pandemic_social_df[-which(pandemic_social_df$period == "lockdown"),]
pandemic_social_prepostlockdown$period <- factor(pandemic_social_prepostlockdown$period, levels = c("prelockdown",
                                                                                                    "postlockdown"))
t.test(pandemic_social_prepostlockdown$sabotage ~ pandemic_social_prepostlockdown$period,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(sabotage ~ period, data = pandemic_social_prepostlockdown)

# mean(prelockdown) = 2.148, 
# mean(postlockdown) = 2.753 
# t(145) = -2.348
# significant; p = 0.02
# cohen's d: 0.349
nrow(pandemic_social_prepostlockdown[which(!is.na(pandemic_social_prepostlockdown$sabotage)),]) # N=205


pandemic_sabotage_summary <- pandemic_social_df %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(sabotage_mean = mean(sabotage, na.rm = TRUE),
            sabotage_sem = std.error(sabotage, na.rm = TRUE),
            sabotage_mean = mean(sabotage, na.rm = TRUE),
            sabotage_sem = std.error(sabotage, na.rm = TRUE))



fig_3a <- ggplot(pandemic_social_df, aes(x = period, 
                                         y = sabotage)
              ) +
  geom_flat_violin(aes(fill = period),position = position_nudge(x = .2, y = 0), adjust = 1.5, trim = TRUE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(period)-.275, y = sabotage, colour = period),position = position_jitter(width = .05), size = 1.5, shape = 21)+
  geom_boxplot(aes(x = period, y = sabotage, fill=period),outlier.shape = NA,
               width = .3,
               lwd = 1.2,
               colour = "black"
               )+
  scale_colour_manual(values = c("#A9A9A9","#add8e6","#659EC7"))+ #5c1a33
  scale_fill_manual(values = c("#A9A9A9","#add8e6","#659EC7"))+ #5c1a33
  scale_x_discrete(labels=c("prelockdown" = "Pre-Lockdown", "lockdown" = "Lockdown",
                            "postlockdown" = "Post-Lockdown")) +
  labs(title="", x="Pandemic period", y="Sabotage belief")   



fig_3a + theme_Publication() + 
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_blank(), 
        axis.line = element_line(colour="black", size = 1.5),
        axis.ticks = element_line(colour="black", size = 1.5),
        legend.text = element_blank())


##############################################
# extract relevant features into dataframe
pandemic_lockdown <- dat_pandemic %>%
  dplyr::filter(period == "lockdown")

## calculate paranoia score
pandemic_lockdown$paranoia_score <- rowMeans(pandemic_lockdown[, grepl("rgpts_", names(pandemic_lockdown))], na.rm = TRUE)


pandemic_lockdown_df <- data.frame(state_proactivity = pandemic_lockdown$state_proactivity,
                                   paranoia = pandemic_lockdown$paranoia_score,
                                   sabotage = pandemic_lockdown$sabotage_decks_avatars + 3,
                                   wsr_block1 = pandemic_lockdown$wsr_block1,
                                   wsr_block2 = pandemic_lockdown$wsr_block2,
                                   wsr = rowMeans(cbind(pandemic_lockdown$wsr_block1,
                                                        pandemic_lockdown$wsr_block2), na.rm = TRUE),
                                   lsr_block1 = pandemic_lockdown$lsr_block1,
                                   lsr_block2 = pandemic_lockdown$lsr_block2,
                                   lsr = rowMeans(cbind(pandemic_lockdown$lsr_block1,
                                                        pandemic_lockdown$lsr_block2), na.rm = TRUE),
                                   mu03_block1 = pandemic_lockdown$mu03_1,
                                   mu03_block2 = pandemic_lockdown$mu03_2,
                                   mu03 = rowMeans(cbind(pandemic_lockdown$mu03_1,
                                                        pandemic_lockdown$mu03_2), na.rm = TRUE),
                                   mu03_precision_block1 = pandemic_lockdown$mu03_1_precision,
                                   mu03_precision_block2 = pandemic_lockdown$mu03_2_precision,
                                   mu03_precision = rowMeans(cbind(pandemic_lockdown$mu03_1_precision,
                                                                  pandemic_lockdown$mu03_2_precision), na.rm = TRUE),
                                   omega3_block1 = pandemic_lockdown$omega3_1,
                                   omega3_block2 = pandemic_lockdown$omega3_2,
                                   omega3 = rowMeans(cbind(pandemic_lockdown$omega3_1,
                                                         pandemic_lockdown$omega3_2), na.rm = TRUE),
                                   omega3_precision_block1 = pandemic_lockdown$omega3_1_precision,
                                   omega3_precision_block2 = pandemic_lockdown$omega3_2_precision,
                                   omega3_precision = rowMeans(cbind(pandemic_lockdown$omega3_1_precision,
                                                                   pandemic_lockdown$omega3_2_precision), na.rm = TRUE),
                                   mu02_block1 = pandemic_lockdown$mu02_1,
                                   mu02_block2 = pandemic_lockdown$mu02_2,
                                   mu02 = rowMeans(cbind(pandemic_lockdown$mu02_1,
                                                         pandemic_lockdown$mu02_2), na.rm = TRUE),
                                   mu02_precision_block1 = pandemic_lockdown$mu02_1_precision,
                                   mu02_precision_block2 = pandemic_lockdown$mu02_2_precision,
                                   mu02_precision = rowMeans(cbind(pandemic_lockdown$mu02_1_precision,
                                                                   pandemic_lockdown$mu02_2_precision), na.rm = TRUE),
                                   omega2_block1 = pandemic_lockdown$omega2_1,
                                   omega2_block2 = pandemic_lockdown$omega2_2,
                                   omega2 = rowMeans(cbind(pandemic_lockdown$omega2_1,
                                                           pandemic_lockdown$omega2_2), na.rm = TRUE),
                                   omega2_precision_block1 = pandemic_lockdown$omega2_1_precision,
                                   omega2_precision_block2 = pandemic_lockdown$omega2_2_precision,
                                   omega2_precision = rowMeans(cbind(pandemic_lockdown$omega2_1_precision,
                                                                     pandemic_lockdown$omega2_2_precision), na.rm = TRUE),
                                   kappa_block1 = pandemic_lockdown$kappa2_1,
                                   kappa_block2 = pandemic_lockdown$kappa2_2,
                                   kappa = rowMeans(cbind(pandemic_lockdown$kappa2_1,
                                                         pandemic_lockdown$kappa2_2), na.rm = TRUE),
                                   kappa_precision_block1 = pandemic_lockdown$kappa2_1_precision,
                                   kappa_precision_block2 = pandemic_lockdown$kappa2_2_precision,
                                   kappa_precision = rowMeans(cbind(pandemic_lockdown$kappa2_1_precision,
                                                                     pandemic_lockdown$kappa2_2_precision), na.rm = TRUE))


# create proactivity labeling based on median
pandemic_lockdown_df$state_proactivity_group <- ifelse(pandemic_lockdown_df$state_proactivity <= median(pandemic_lockdown_df$state_proactivity,na.rm = TRUE), "less proactive","more proactive")

# Paranoia and sabotage during lockdown
fig_3b1 <- ggplot(pandemic_lockdown_df, aes(x=sabotage, y=paranoia)) + 
  geom_point(shape=16,color="black",alpha=0.4, show.legend = FALSE) + 
  geom_smooth(method = "lm",size=1.5, alpha=0.5, show.legend = FALSE) + #stat_cor(label.x = 1,label.y = 3.5) + 
  labs(title ="",x="Sabotage Belief",y="Paranoia")

fig_3b1 + theme_Publication() + theme(axis.title.y = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.text = element_blank(), 
                                      axis.line = element_line(colour="black", size = 1.5),
                                      axis.ticks = element_line(colour="black", size = 1.5),
                                      legend.text = element_blank())

t.test(pandemic_lockdown_df$wsr ~ pandemic_lockdown_df$state_proactivity_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(wsr ~ state_proactivity_group, data = pandemic_lockdown_df)

fig3b_2 <- ggplot(pandemic_lockdown_df, aes(x = state_proactivity_group,
                                            y = wsr,
                                            fill=state_proactivity_group)) +
  geom_boxplot_pattern(aes(fill=state_proactivity_group),pattern=c("stripe","none"),pattern_colour = "black",#alpha = c(0.6, 0.6,
                       #0.8, 0.8,
                       #1.0, 1.0),
                       #width = .7,
                       lwd = 1.2#,
                       #position = position_dodge(width=0.8)
  ) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(0.5), show.legend = FALSE) +
  scale_fill_manual(values = c("#add8e6","#659EC7"))+ #5c1a33
  scale_x_discrete(labels=c("less proactive" = "Less Proactive", "more proactive" = "More Proactive")) +
  labs(title="", x="State Proactivity", y="Win-Switch Rate") +  guides(#pattern = guide_legend(override.aes = list(fill = "white")),
    fill = guide_legend(override.aes = list(pattern=c("stripe","none")))
  )


fig3b_2 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                       axis.title.x = element_blank(),
                                       axis.text = element_blank(), 
                                       axis.line = element_line(colour="black", size = 1.5),
                                       axis.ticks = element_line(colour="black", size = 1.5),
                                       legend.text = element_blank(),
                                       legend.position = "none")


t.test(pandemic_lockdown_df$lsr ~ pandemic_lockdown_df$state_proactivity_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)


# fdr correction using WSR and LSR
pvalues_behavior <- c(0.006783, 0.04382)
pvalues_behavior_sorted <- sort(pvalues_behavior)
p.adjust(pvalues_behavior_sorted, method = "BH")



# weighted t-test: mu03
mu03_wtdttest <- wtd.t.test(x=pandemic_lockdown_df$mu03[pandemic_lockdown_df$state_proactivity_group=="less proactive"],
                            y=pandemic_lockdown_df$mu03[pandemic_lockdown_df$state_proactivity_group=="more proactive"],
                            weight=pandemic_lockdown_df$mu03_precision[pandemic_lockdown_df$state_proactivity_group=="less proactive"],
                            weighty=pandemic_lockdown_df$mu03_precision[pandemic_lockdown_df$state_proactivity_group=="more proactive"],
                            samedata=FALSE)
# t(223) = 4.29
# p-value = p<0.001 (2.563324e-05)
# wtd.mu[recommended] = -0.603
# wtd.mu[required] = -1.427

# 95% CI calculation
t_score <- mu03_wtdttest$coefficients[1]
sample_se <- mu03_wtdttest$additional[4]
sample_mean_difference <- mu03_wtdttest$additional[1]
margin_error <- t_score*sample_se
lower_bound <- sample_mean_difference - margin_error
upper_bound <- sample_mean_difference + margin_error
print(c(lower_bound,upper_bound))

# Cohen's d
Cohen_d = sample_mean_difference/sample_se


fig3b_3 <- ggplot(pandemic_lockdown_df, aes(x = state_proactivity_group,
                                            y = mu03,
                                            fill=state_proactivity_group,
                                            size=mu03_precision)) +
  geom_boxplot_pattern(aes(fill=state_proactivity_group),pattern=c("stripe","none"),pattern_colour = "black",#alpha = c(0.6, 0.6,
                       #0.8, 0.8,
                       #1.0, 1.0),
                       #width = .7,
                       lwd = 1.2#,
                       #position = position_dodge(width=0.8)
  ) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(0.5), show.legend = FALSE) +
  scale_fill_manual(values = c("#add8e6","#659EC7"))+ #5c1a33
  scale_x_discrete(labels=c("less proactive" = "Less Proactive", "more proactive" = "More Proactive")) +
  labs(title="", x="State Proactivity", y="Volatility Belief") +  guides(#pattern = guide_legend(override.aes = list(fill = "white")),
    fill = guide_legend(override.aes = list(pattern=c("stripe","none")))
  )


fig3b_3 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                       axis.title.x = element_blank(),
                                       axis.text = element_blank(), 
                                       axis.line = element_line(colour="black", size = 1.5),
                                       axis.ticks = element_line(colour="black", size = 1.5),
                                       legend.text = element_blank(),
                                       legend.position = "none")


fig3b_3 <- fig3b_3 + theme_Publication() + theme(legend.direction = "horizontal",
                                                 legend.key.size= unit(2, "cm"),
                                                 legend.spacing.x = unit(4.0, 'cm'),
                                                 legend.text = element_blank())

# Using the cowplot package
legend <- cowplot::get_legend(fig3b_3)

grid.newpage()
grid.draw(legend)



# weighted t-test: mu02
mu02_wtdttest <- wtd.t.test(x=pandemic_lockdown_df$mu02[pandemic_lockdown_df$state_proactivity_group=="less proactive"],
                            y=pandemic_lockdown_df$mu02[pandemic_lockdown_df$state_proactivity_group=="more proactive"],
                            weight=pandemic_lockdown_df$mu02_precision[pandemic_lockdown_df$state_proactivity_group=="less proactive"],
                            weighty=pandemic_lockdown_df$mu02_precision[pandemic_lockdown_df$state_proactivity_group=="more proactive"],
                            samedata=FALSE)
# t(189) = 3.16
# p-value = p<0.001 (1.851316e-03)
# wtd.mu[recommended] = -0.203
# wtd.mu[required] = -0.344

# 95% CI calculation
t_score <- mu02_wtdttest$coefficients[1]
sample_se <- mu02_wtdttest$additional[4]
sample_mean_difference <- mu02_wtdttest$additional[1]
margin_error <- t_score*sample_se
lower_bound <- sample_mean_difference - margin_error
upper_bound <- sample_mean_difference + margin_error
print(c(lower_bound,upper_bound))

# Cohen's d
Cohen_d = sample_mean/sample_se

# weighted t-test: omega3
omega3_wtdttest <- wtd.t.test(x=pandemic_lockdown_df$omega3[pandemic_lockdown_df$state_proactivity_group=="less proactive"],
                            y=pandemic_lockdown_df$omega3[pandemic_lockdown_df$state_proactivity_group=="more proactive"],
                            weight=pandemic_lockdown_df$omega3_precision[pandemic_lockdown_df$state_proactivity_group=="less proactive"],
                            weighty=pandemic_lockdown_df$omega3_precision[pandemic_lockdown_df$state_proactivity_group=="more proactive"],
                            samedata=FALSE)
# weighted t-test: omega2
omega2_wtdttest <- wtd.t.test(x=pandemic_lockdown_df$omega2[pandemic_lockdown_df$state_proactivity_group=="less proactive"],
                            y=pandemic_lockdown_df$omega2[pandemic_lockdown_df$state_proactivity_group=="more proactive"],
                            weight=pandemic_lockdown_df$omega2_precision[pandemic_lockdown_df$state_proactivity_group=="less proactive"],
                            weighty=pandemic_lockdown_df$omega2_precision[pandemic_lockdown_df$state_proactivity_group=="more proactive"],
                            samedata=FALSE)
# weighted t-test: kappa
kappa_wtdttest <- wtd.t.test(x=pandemic_lockdown_df$kappa[pandemic_lockdown_df$state_proactivity_group=="less proactive"],
                            y=pandemic_lockdown_df$kappa[pandemic_lockdown_df$state_proactivity_group=="more proactive"],
                            weight=pandemic_lockdown_df$kappa_precision[pandemic_lockdown_df$state_proactivity_group=="less proactive"],
                            weighty=pandemic_lockdown_df$kappa_precision[pandemic_lockdown_df$state_proactivity_group=="more proactive"],
                            samedata=FALSE)


# fdr correction using mu03, mu02, omega3, omega2, kappa
pvalues_belief <- c(2.563324e-05, 1.851316e-03,0.2494116,0.94777949,9.552034e-03 )
pvalues_belief_sorted <- sort(pvalues_belief)
pvalues_belief_sorted
p.adjust(pvalues_belief_sorted, method = "BH")





