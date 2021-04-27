### Analysis: Investigate relationship between vaccine conspiracy and paranoia/task behavior
###
### Description: Perform pearson correlations, relating individual beliefs
###                 regarding covid19 vaccine-related conspiracies to 
###                 paranoia, task behavior, and belief updating 
###                 parameters (volatility & contingency)
### Figure: 6
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


# recode and calculate covid vaccine conspiracy belief score
dat_replication_df <- dat_replication %>%
  select(paranoia_score, paranoia_group,
         covid_conspiracy_vaccine_1:covid_conspiracy_vaccine_5,
         names(dat_replication[, grepl("wsr_", names(dat_replication))]),
         names(dat_replication[, grepl("lsr_", names(dat_replication))]),
         names(dat_replication[, grepl("mu02_", names(dat_replication))]),
         names(dat_replication[, grepl("mu03_", names(dat_replication))]),
         names(dat_replication[, grepl("kappa2_", names(dat_replication))]),
         names(dat_replication[, grepl("omega2_", names(dat_replication))]),
         names(dat_replication[, grepl("omega3_", names(dat_replication))]))

dat_replication_df[, grepl("covid_conspiracy_vaccine_", names(dat_replication_df))] <- ifelse(dat_replication_df[, grepl("covid_conspiracy_vaccine_", names(dat_replication_df))] == "Strongly disagree",1,
                                                                                              ifelse(dat_replication_df[, grepl("covid_conspiracy_vaccine_", names(dat_replication_df))] == "Disagree",2,
                                                                                                     ifelse(dat_replication_df[, grepl("covid_conspiracy_vaccine_", names(dat_replication_df))] == "Somewhat disagree",3,
                                                                                                            ifelse(dat_replication_df[, grepl("covid_conspiracy_vaccine_", names(dat_replication_df))] == "Neutral",4,
                                                                                                                   ifelse(dat_replication_df[, grepl("covid_conspiracy_vaccine_", names(dat_replication_df))] == "Somewhat agree",5,
                                                                                                                          ifelse(dat_replication_df[, grepl("covid_conspiracy_vaccine_", names(dat_replication_df))] == "Agree",6,
                                                                                                                                 ifelse(dat_replication_df[, grepl("covid_conspiracy_vaccine_", names(dat_replication_df))] == "Strongly agree",7,"")))))))

# convert character to numeric
dat_replication_df[, grepl("covid_conspiracy_vaccine_", names(dat_replication_df))] <- as.data.frame(sapply(dat_replication_df[, grepl("covid_conspiracy_vaccine_", names(dat_replication_df))], as.numeric))

# calculate covid vaccine conspiracy belief scores
dat_replication_df$covid_vacc_consp_score <- rowMeans(dat_replication_df[, grepl("covid_conspiracy_vaccine_", names(dat_replication_df))], na.rm = TRUE)


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
length(dat_replication_df[which(!is.na(dat_replication_df$covid_vacc_consp_score)),]$covid_vacc_consp_score)
# Of those participants, how many are low paranoia?
length(dat_replication_df[which(!is.na(dat_replication_df$covid_vacc_consp_score) & dat_replication_df$paranoia_group == "low"),]$covid_vacc_consp_score)
# how many are high paranoia?
length(dat_replication_df[which(!is.na(dat_replication_df$covid_vacc_consp_score) & dat_replication_df$paranoia_group == "high"),]$covid_vacc_consp_score)





# Figure 6A
fig_6a <- ggplot(dat_replication_df, aes(x=covid_vacc_consp_score, y=paranoia_score)) +
  geom_point(shape=16,color="#37474f",alpha=0.4) + #stat_cor(label.x=2, label.y = 3.5) + 
  geom_smooth(method="lm" , color="#f57f17", fill="#f57f17", size=1.5, alpha=0.4) + 
  labs(title ="",x="COVID-19 vaccine conspiracy belief",y="Paranoia")

fig_6a <- fig_6a + theme_Publication() +  theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank()) 
fig_6a # r=0.61, p < 2.2e-16

# Figure 6B
fig_6b <- ggplot(dat_replication_df, aes(x=covid_vacc_consp_score, y=wsr)) +
  geom_point(shape=16,color="#37474f",alpha=0.4) + stat_cor(label.x = 1,label.y = 0.5) + 
  geom_smooth(method="lm" , color="#f57f17", fill="#f57f17", size=1.5, alpha=0.5) + 
  labs(title ="",x="COVID-19 vaccine conspiracy belief",y="Win-switch rate")

fig_6b <- fig_6b + theme_Publication() +  theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank()) 
fig_6b # r=0.44, p<2.2e-16

# Figure 6 (not included in manuscript but used for correction)
fig6_lsr <- ggplot(dat_replication_df, aes(x=covid_vacc_consp_score, y=lsr)) +
  geom_point(shape=16,color="#37474f",alpha=0.4) + #stat_cor(label.x = 1,label.y = 0.5) + 
  geom_smooth(method="lm" , color="#f57f17", fill="#f57f17", size=1.5, alpha=0.5) + 
  labs(title ="",x="COVID-19 vaccine conspiracy belief",y="Lose-stay rate")

fig6_lsr <- fig6_lsr + theme_Publication() +  theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank()) 
fig6_lsr # r=-0.19, p=0.00014


# FDR correction - Benjamini-Hochberg - for task behavior (WSR & LSR)
pvalues_behavior <- c(2.2e-16,0.00014)
pvalues_behavior_ordered <- sort(pvalues_behavior)
pvalues_behavior_ordered
p.adjust(pvalues_behavior_ordered, method = "BH")


# view summary output of weighted least squares regression
summary(lm(mu03 ~ covid_vacc_consp_score, data=dat_replication_df, weights = dat_replication_df$mu03_precision))

wtd.cor(dat_replication_df$mu03, dat_replication_df$covid_vacc_consp_score, weight = dat_replication_df$mu03_precision)

fig_6c <- ggplot(dat_replication_df, aes(x=covid_vacc_consp_score, y=mu03, size=mu03_precision)) + 
  geom_point(shape=16,color="#37474f",alpha=0.4, show.legend = FALSE) + 
  geom_smooth(method="lm" , mapping = aes(weight = mu03_precision),
              color="#f57f17", fill="#f57f17", size=1.5, alpha=0.5, show.legend = FALSE) +
  labs(title ="", x="COVID-19 vaccine conspiracy belief",y=expression(bold(mu[bold("3")]^bold("0"))))

fig_6c <- fig_6c + theme_Publication() +  theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank()) 
fig_6c # r=0.34, p=1.532e-12


# view summary output of weighted least squares regression
summary(lm(mu02 ~ covid_vacc_consp_score, data=dat_replication_df, weights = dat_replication_df$mu02_precision))

cor(dat_replication_df$mu02, dat_replication_df$covid_vacc_consp_score, use = "complete.obs")
wtd.cor(dat_replication_df$mu02, dat_replication_df$covid_vacc_consp_score, weight = dat_replication_df$mu02_precision)

# Figure 6 (not included in manuscript but used for correction)
fig6_mu02 <- ggplot(dat_replication_df, aes(x=covid_vacc_consp_score, y=mu02, size=mu02_precision)) + 
  geom_point(shape=16,color="#37474f",alpha=0.4, show.legend = FALSE) + 
  geom_smooth(method="lm" , mapping = aes(weight = mu02_precision),
              color="#f57f17", fill="#f57f17", size=1.5, alpha=0.5, show.legend = FALSE) +
  labs(title ="", x="COVID-19 vaccine conspiracy belief",y=expression(bold(mu[bold("2")]^bold("0"))))

fig6_mu02 <- fig6_mu02 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.text = element_blank(), 
                                        axis.line = element_line(colour="black", size = 1.5),
                                        axis.ticks = element_line(colour="black", size = 1.5),
                                        legend.text = element_blank()) 
fig6_mu02 # r=0.16, p=0.00089


# FDR correction - Benjamini-Hochberg - for beliefs (volatility & contingency)
pvalues_beliefs <- c(1.532e-12,0.00089)
pvalues_beliefs_ordered <- sort(pvalues_beliefs)
pvalues_beliefs_ordered
p.adjust(pvalues_beliefs_ordered, method = "BH")

