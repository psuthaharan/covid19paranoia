### Analysis: Investigate relationship between QAnon beliefs and paranoia/task behavior
###
### Description: Perform pearson correlations, relating individual beliefs
###                 about the QAnon movement to paranoia, task behavior, and
###                 belief updating parameters (volatility & contingency)
### Figure: 7
### Written by: Praveen Suthaharan

# clear environment 
rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/revisions/data")


# load libraries/other packages
library(dplyr)
library(ggpubr)
source("C:/Pandemic_2020/revisions/code/theme_publication.R") # source: https://rpubs.com/Koundy/71792
library(weights) # needed for wtd.cor

# read in data
dat <- read.csv("data.csv", 
                stringsAsFactors = FALSE)

# subset for replication data
dat_replication <- dat[which(dat$dataset == "replication"),]



# calculate paranoia scores
dat_replication$paranoia_score <- rowMeans(dat_replication[, grepl("rgpts_", names(dat_replication))], na.rm = TRUE)

# calculate paranoia grouping
dat_replication$paranoia_group <- ifelse(rowSums(dat_replication[, grepl("rgpts_per", names(dat_replication))], na.rm = TRUE) > 10, "high","low")


# recode and calculate covid vaccine conspiracy belief score and vaccine attitude
dat_replication_df <- dat_replication %>%
  select(paranoia_score, paranoia_group,
         qanon_rating,
         #covid_conspiracy_vaccine_1:covid_conspiracy_vaccine_5,
         #vaccine_scale_1:vaccine_scale_5,
         names(dat_replication[, grepl("wsr_", names(dat_replication))]),
         names(dat_replication[, grepl("lsr_", names(dat_replication))]),
         names(dat_replication[, grepl("mu02_", names(dat_replication))]),
         names(dat_replication[, grepl("mu03_", names(dat_replication))]),
         names(dat_replication[, grepl("kappa2_", names(dat_replication))]),
         names(dat_replication[, grepl("omega2_", names(dat_replication))]),
         names(dat_replication[, grepl("omega3_", names(dat_replication))]))

# calculate task behavior/beliefs
dat_replication_df$wsr <- rowMeans(cbind(dat_replication_df$wsr_block1,
                                         dat_replication_df$wsr_block2), na.rm = TRUE)
dat_replication_df$lsr <- rowMeans(cbind(dat_replication_df$lsr_block1,
                                         dat_replication_df$lsr_block2), na.rm = TRUE)
dat_replication_df$mu02 <- rowMeans(cbind(dat_replication_df$mu02_1,
                                          dat_replication_df$mu02_2), na.rm = TRUE)
dat_replication_df$mu02_precision <- rowMeans(cbind(dat_replication_df$mu02_1_precision,
                                                    dat_replication_df$mu02_2_precision), na.rm = TRUE)
dat_replication_df$mu03 <- rowMeans(cbind(dat_replication_df$mu03_1,
                                          dat_replication_df$mu03_2), na.rm = TRUE)
dat_replication_df$mu03_precision <- rowMeans(cbind(dat_replication_df$mu03_1_precision,
                                                    dat_replication_df$mu03_2_precision), na.rm = TRUE)


# how many participants responded to QAnon rating?
length(dat_replication_df[which(!is.na(dat_replication_df$qanon_rating)),]$qanon_rating)
# Of those participants, how many are low paranoia?
length(dat_replication_df[which(!is.na(dat_replication_df$qanon_rating) & dat_replication_df$paranoia_group == "low"),]$qanon_rating)
# how many are high paranoia?
length(dat_replication_df[which(!is.na(dat_replication_df$qanon_rating) & dat_replication_df$paranoia_group == "high"),]$qanon_rating)


# Figure 7A
fig_7a <- ggplot(dat_replication_df, aes(x=qanon_rating, y=paranoia_score)) +
  geom_point(shape=16,color="#5c1a33",alpha=0.4) + #stat_cor(label.x=25, label.y = 3.5) + 
  geom_smooth(method="lm" , color="#5445b1", fill="#5445b1", size=1.5, alpha=0.4) + 
  labs(title ="",x="QAnon belief",y="Paranoia")

fig_7a <- fig_7a + theme_Publication() +  theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank()) 
fig_7a # r=0.5, p < 2.2e-16

# Figure 7B
fig_7b <- ggplot(dat_replication_df, aes(x=qanon_rating, y=wsr)) +
  geom_point(shape=16,color="#5c1a33",alpha=0.4) + #stat_cor(label.x = 25,label.y = 0.5) + 
  geom_smooth(method="lm" , color="#5445b1", fill="#5445b1", size=1.5, alpha=0.5) + 
  labs(title ="",x="QAnon belief",y="Win-switch rate")

fig_7b <- fig_7b + theme_Publication() +  theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank()) 
fig_7b # r=0.44, p<7.4e-16

# Figure 7 (not included in manuscript but used for p-value correction)
fig7_lsr <- ggplot(dat_replication_df, aes(x=qanon_rating, y=lsr)) +
  geom_point(shape=16,color="#5c1a33",alpha=0.4) + #stat_cor(label.x = 25,label.y = 0.5) + 
  geom_smooth(method="lm" , color="#5445b1", fill="#5445b1", size=1.5, alpha=0.5) + 
  labs(title ="",x="QAnon belief",y="Lose-stay rate")

fig7_lsr <- fig7_lsr + theme_Publication() +  theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank()) 
fig7_lsr # r=-0.16, p=0.0051


# FDR correction - Benjamini-Hochberg - task behaviors (WSR & LSR)
pvalues_behavior <- c(7.4e-16,0.0051)
pvalues_behavior_ordered <- sort(pvalues_behavior)
pvalues_behavior_ordered 
p.adjust(pvalues_behavior_ordered, method = "BH")



# view summary output of weighted least squares regression
summary(lm(mu03 ~ qanon_rating, data=dat_replication_df, weights = dat_replication_df$mu03_precision))

# compute weighted correlation
wtd.cor(dat_replication_df$mu03, dat_replication_df$qanon_rating, weight = dat_replication_df$mu03_precision)

# Figure 7C
fig_7c <- ggplot(dat_replication_df, aes(x=qanon_rating, y=mu03, size=mu03_precision)) + 
  geom_point(shape=16,color="#5c1a33",alpha=0.4, show.legend = FALSE) + 
  geom_smooth(method="lm" , mapping = aes(weight = mu03_precision),
              color="#5445b1", fill="#5445b1", size=1.5, alpha=0.5, show.legend = FALSE) +
  labs(title ="", x="QAnon belief",y=expression(bold(mu[bold("3")]^bold("0"))))

fig_7c <- fig_7c + theme_Publication() +  theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank()) 
fig_7c # r=0.31, p=2.670861e-08


# view summary output of weighted least squares regression
summary(lm(mu02 ~ qanon_rating, data=dat_replication_df, weights = dat_replication_df$mu02_precision))

# compute weighted correlation
wtd.cor(dat_replication_df$mu02, dat_replication_df$qanon_rating, weight = dat_replication_df$mu02_precision)

# Figure 7 (not included in manuscript but used for p-value correction)
fig7_mu02 <- ggplot(dat_replication_df, aes(x=qanon_rating, y=mu02, size=mu02_precision)) + 
  geom_point(shape=16,color="#5c1a33",alpha=0.4, show.legend = FALSE) + 
  geom_smooth(method="lm" , mapping = aes(weight = mu02_precision),
              color="#5445b1", fill="#5445b1", size=1.5, alpha=0.5, show.legend = FALSE) +
  labs(title ="", x="QAnon belief",y=expression(bold(mu[bold("2")]^bold("0"))))

fig7_mu02 <- fig7_mu02 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank()) 
fig7_mu02 # r=0.16, p=0.00472



# FDR correction - Benjamini-Hochberg - for the beliefs (volatility & contingency)
pvalues_beliefs <- c(2.671e-08,0.00472)
pvalues_beliefs_ordered <- sort(pvalues_beliefs)
pvalues_beliefs_ordered 
p.adjust(pvalues_beliefs_ordered, method = "BH")

