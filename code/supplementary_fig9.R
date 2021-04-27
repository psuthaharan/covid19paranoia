# clear environment 
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
library(weights)

# read in data
dat <- read.csv("pandemicPRL.csv", 
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
         qanon_political_4,
         #covid_conspiracy_vaccine_1:covid_conspiracy_vaccine_5,
         #vaccine_scale_1:vaccine_scale_5,
         names(dat_replication[, grepl("wsr_", names(dat_replication))]),
         names(dat_replication[, grepl("lsr_", names(dat_replication))]),
         names(dat_replication[, grepl("mu02_", names(dat_replication))]),
         names(dat_replication[, grepl("mu03_", names(dat_replication))]),
         names(dat_replication[, grepl("kappa2_", names(dat_replication))]),
         names(dat_replication[, grepl("omega2_", names(dat_replication))]),
         names(dat_replication[, grepl("omega3_", names(dat_replication))]))



dat_replication_df$qanon_political_4 <- ifelse(dat_replication_df$qanon_political_4 == "1 - Republican Party","Republican",
                                          ifelse(dat_replication_df$qanon_political_4 == "2 - Democratic Party","Democrat",
                                               ifelse(dat_replication_df$qanon_political_4 == "3 - Neither","Neither",99)))



republicans_count <- length(dat_replication_df$qanon_political_4[which(dat_replication_df$qanon_political_4 == "Republican")])
democrats_count <- length(dat_replication_df$qanon_political_4[which(dat_replication_df$qanon_political_4 == "Democrat")])
neither_count <- length(dat_replication_df$qanon_political_4[which(dat_replication_df$qanon_political_4 == "Neither")])
decline_count <- length(dat_replication_df$qanon_political_4[which(dat_replication_df$qanon_political_4 == 99)])


#Format all percents into a table
political_affiliation_summary <- data.frame(republicans_count, democrats_count, neither_count, decline_count)
#Transposes dataframe to verticle orientation
political_affiliation_table <- t(political_affiliation_summary)
#Names each row of the data table
rownames(political_affiliation_table) <- c("Republicans",
                                           "Democrats",
                                           "Neither",
                                           "Decline")
#Eliminates names of the table columns
colnames(political_affiliation_table) <- "Count"
#Prints the table
political_affiliation_table <- print(political_affiliation_table, quote = FALSE)



dat_replication_df$wsr <- rowMeans(cbind(dat_replication_df$wsr_block1,dat_replication_df$wsr_block2))
dat_replication_df$lsr <- rowMeans(cbind(dat_replication_df$lsr_block1,dat_replication_df$lsr_block2))
dat_replication_df$mu02 <- rowMeans(cbind(dat_replication_df$mu02_1,dat_replication_df$mu02_2))
dat_replication_df$mu03 <- rowMeans(cbind(dat_replication_df$mu03_1,dat_replication_df$mu03_2))

# dat.df$omega2.block1 <- dat$omega2_1
# dat.df$omega2.block2 <- dat$omega2_2
# dat.df$omega2 <- rowMeans(cbind(dat.df$omega2.block1,dat.df$omega2.block2))

# dat.df$omega3.block1 <- dat$omega3_1
# dat.df$omega3.block2 <- dat$omega3_2
# dat.df$omega3 <- rowMeans(cbind(dat.df$omega3.block1,dat.df$omega3.block2))

# dat.df$kappa.block1 <- dat$kappa2_1
# dat.df$kappa.block2 <- dat$kappa2_2
# dat.df$kappa <- rowMeans(cbind(dat.df$kappa.block1,dat.df$kappa.block2))


# include precisions to dataframe

dat_replication_df$mu02_precision <- rowMeans(cbind(dat_replication_df$mu02_1_precision,
                                                    dat_replication_df$mu02_2_precision))
dat_replication_df$mu03_precision <- rowMeans(cbind(dat_replication_df$mu03_1_precision,
                                                    dat_replication_df$mu03_2_precision))




dat_replication_subset_df <- dat_replication_df[which(dat_replication_df$qanon_political_4 == "Republican" | dat_replication_df$qanon_political_4 == "Democrat"),]


# QAnon rating
p100 <- ggplot(dat_replication_subset_df, aes(x=qanon_political_4, 
                                              y=qanon_rating,
                                              fill = qanon_political_4)) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_y_continuous(name = "QAnon belief") +
  scale_x_discrete(name = "Political party") + theme_Publication() +
  scale_fill_manual(name = "Political party", 
                    values = c("Democrat" = "#0015BC", "Republican" = "#FF0000"))

p100 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.text = element_blank(), 
                                    axis.line = element_line(colour="black", size = 1.5),
                                    axis.ticks = element_line(colour="black", size = 1.5),
                                    legend.text = element_blank(),
                                    legend.position =  "none") 


# paranoia
p101 <- ggplot(dat_replication_subset_df, aes(x=qanon_political_4, 
                                              y=paranoia_score,
                                              fill = qanon_political_4)) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_y_continuous(name = "Paranoia") +
  scale_x_discrete(name = "Political party") + theme_Publication() +
  scale_fill_manual(name = "Political party", 
                    values = c("Democrat" = "#0015BC", "Republican" = "#FF0000"))

p101 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.text = element_blank(), 
                                    axis.line = element_line(colour="black", size = 1.5),
                                    axis.ticks = element_line(colour="black", size = 1.5),
                                    legend.text = element_blank(),
                                    legend.position = "none")


# Win-switch rate
p102 <- ggplot(dat_replication_subset_df, aes(x=qanon_political_4, 
                                              y=wsr,
                                              fill = qanon_political_4)) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_y_continuous(name = "Win-switch rate") +
  scale_x_discrete(name = "Political party") + theme_Publication() +
  scale_fill_manual(name = "Political party", 
                    values = c("Democrat" = "#0015BC", "Republican" = "#FF0000"))

p102 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.text = element_blank(), 
                                    axis.line = element_line(colour="black", size = 1.5),
                                    axis.ticks = element_line(colour="black", size = 1.5),
                                    legend.text = element_blank(),
                                    legend.position = "none")

# Lose-stay rate
p103 <- ggplot(dat_replication_subset_df, aes(x=qanon_political_4, 
                                              y=lsr,
                                              fill = qanon_political_4)) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_y_continuous(name = "Lose-stay rate") +
  scale_x_discrete(name = "Political party") + theme_Publication() +
  scale_fill_manual(name = "Political party", 
                    values = c("Democrat" = "#0015BC", "Republican" = "#FF0000"))

p103 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.text = element_blank(), 
                                    axis.line = element_line(colour="black", size = 1.5),
                                    axis.ticks = element_line(colour="black", size = 1.5),
                                    legend.text = element_blank(),
                                    legend.position = "none")

# mu03 (weighted)
p104 <- ggplot(dat_replication_subset_df, aes(x=qanon_political_4, 
                                              y=mu03,
                                              fill = qanon_political_4,
                                              size = mu03_precision)) +
  geom_point(shape=16,color="black", alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(mapping = aes(weight = mu03_precision),
               alpha = 0.7, width=0.5, lwd=1.2) +
  scale_y_continuous(name = expression(bold(mu[bold("3")]^(bold("0"))))) +
  scale_x_discrete(name = "Political party") + theme_Publication() +
  scale_fill_manual(name = "Political party", 
                    values = c("Democrat" = "#0015BC", "Republican" = "#FF0000"))

p104 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.text = element_blank(), 
                                    axis.line = element_line(colour="black", size = 1.5),
                                    axis.ticks = element_line(colour="black", size = 1.5),
                                    legend.text = element_blank(),
                                    legend.position = "none")


# mu02 (weighted)
p105 <- ggplot(dat_replication_subset_df, aes(x=qanon_political_4, 
                                              y=mu02,
                                              fill = qanon_political_4,
                                              size = mu02_precision)) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(mapping = aes(weight = mu02_precision),
               alpha = 0.7, width=0.5, lwd=1.2) +
  scale_y_continuous(name = expression(bold(mu[bold("2")]^(bold("0"))))) +
  scale_x_discrete(name = "Political party") + theme_Publication() +
  scale_fill_manual(name = "Political party", 
                    values = c("Democrat" = "#0015BC", "Republican" = "#FF0000"))

p105 <- p105 + theme(legend.text = element_blank(),
                     legend.key.size= unit(3, "cm")) + theme_Publication() +  theme(axis.title.y = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.text = element_blank(), 
                                    axis.line = element_line(colour="black", size = 1.5),
                                    axis.ticks = element_line(colour="black", size = 1.5),
                                    legend.text = element_blank(),
                                    legend.position = "none")

library(cowplot)
legend <- cowplot::get_legend(p105)

grid.newpage()
grid.draw(legend)

grid.arrange(p100,p101,p102,p103,p104,p105, layout_matrix = rbind(c(1,1,3,5),
                                                                  c(2,2,4,6)))



# t-test: QAnon belief
t.test(dat_replication_subset_df$qanon_rating ~ dat_replication_subset_df$qanon_political_4,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)  # t(163) = -7.11
                  # p-value = 3.37e-11
                  # mu[dem] = 12.93
                  # mu[rep] = 37.12
library(lsr)
cohensD(qanon_rating ~ qanon_political_4, data = dat_replication_subset_df)

# t-test: paranoia
t.test(dat_replication_subset_df$paranoia_score ~ dat_replication_subset_df$qanon_political_4,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)  # t(207) = -3.28
                  # p-value = 0.001
                  # mu[dem] = 0.54
                  # mu[rep] = 0.91

cohensD(paranoia_score ~ qanon_political_4, data = dat_replication_subset_df)

# t-test: WSR
t.test(dat_replication_subset_df$wsr ~ dat_replication_subset_df$qanon_political_4,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)  # t(181) = -4.12
                  # p-value = 5.74e-05
                  # mu[dem] = 0.07
                  # mu[rep] = 0.15

cohensD(wsr ~ qanon_political_4, data = dat_replication_subset_df)

# t-test: LSR
t.test(dat_replication_subset_df$lsr ~ dat_replication_subset_df$qanon_political_4,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)  # t(316) = 1.85
                  # p-value = 0.065
                  # mu[dem] = 0.28
                  # mu[rep] = 0.24



# weighted t-test: mu3
mu03_qanon_wtdttest <- wtd.t.test(x=dat_replication_subset_df$mu03[dat_replication_subset_df$qanon_political_4=="Democrat"],
           y=dat_replication_subset_df$mu03[dat_replication_subset_df$qanon_political_4=="Republican"],
           weight=dat_replication_subset_df$mu03_precision[dat_replication_subset_df$qanon_political_4=="Democrat"],
           weighty=dat_replication_subset_df$mu03_precision[dat_replication_subset_df$qanon_political_4=="Republican"],
           samedata=FALSE) # t(263) = -3.00
                           # p-value = 0.003
                           # wtd.mu[dem] = -1.22
                           # wtd.mu[rep] = -0.74


# 95% CI calculation
t_score <- mu03_qanon_wtdttest$coefficients[1]
sample_se <- mu03_qanon_wtdttest$additional[4]
sample_mean_difference <- mu03_qanon_wtdttest$additional[1]
margin_error <- t_score*sample_se
lower_bound <- sample_mean_difference - margin_error
upper_bound <- sample_mean_difference + margin_error
print(c(lower_bound,upper_bound))

# Cohen's d
Cohen_d = sample_mean_difference/sample_se
Cohen_d

# weighted t-test: mu2
wtd.t.test(x=dat_replication_subset_df$mu02[dat_replication_subset_df$qanon_political_4=="Democrat"],
           y=dat_replication_subset_df$mu02[dat_replication_subset_df$qanon_political_4=="Republican"],
           weight=dat_replication_subset_df$mu02_precision[dat_replication_subset_df$qanon_political_4=="Democrat"],
           weighty=dat_replication_subset_df$mu02_precision[dat_replication_subset_df$qanon_political_4=="Republican"],
           samedata=FALSE) # t(270) = 0.62
                           # p-value = 0.534
                           # wtd.mu[dem] = -0.23
                           # wtd.mu[rep] = -0.25




pvalues_wsrlsr <- c(5.74e-05,0.065)
pvalues_wsrlsr_sorted <- sort(pvalues_wsrlsr)
pvalues_mus <- c(0.003, 0.534)
pvalues_mus_sorted <- sort(pvalues_mus)

pvalues_wsrlsr_sorted 
p.adjust(pvalues_wsrlsr_sorted, method = "BH")

pvalues_mus_sorted
p.adjust(pvalues_mus_sorted, method = "BH")
