### Analysis: Investigating impact of mask policy on paranoia and belief updating
###
### Description: 
###                 
###                 
###                 
### Figure: 4
### Written by: Praveen Suthaharan


# clear environment
rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/revisions/data")


#install.packages("usmap")
library(usmap)
library(ggplot2)
library(dplyr)

# read in mask data
mask <- read.csv("Figure4A_mapData.csv")

# convert mask policy from integer to factor
mask$mwr <- as.factor(mask$mwr)

# how many states recommend mask-wearing?
length(mask[which(mask$mwr == 0),]$abbr)

# how many states require?
length(mask[which(mask$mwr == 1),]$abbr)

# Figure 4A: generate US map with mask-policy labels
plot_usmap(data = mask, values = "mwr", color="black", labels = TRUE, label_color = "black") + 
  scale_fill_manual(values = c('0' = "#FFA500", '1' = "#008000"), name = "Mask Policy",
                    labels = c("Recommended","Mandated")) +
  theme(legend.position = "none")



################################################################################################
################################################################################################
################################################################################################
rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/revisions/data")

# load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(devtools)
library(openintro)
library(plotrix)
library(gridExtra)
library(boot) # library to perform bootstrap

source('C:\\Pandemic_2020\\revisions\\code\\theme_publication.R')

# read data
dat <- read.csv('pandemicPRL.csv')


# extract pandemic dataset from January to July (i.e., pre-lockdown to post-lockdown)

dat_pandemic <- dat[which(dat$dataset == "pandemic" & (dat$period == "prelockdown" |
                                                         dat$period == "lockdown" |
                                                           dat$period == "postlockdown")),]

# calculate paranoia score
dat_pandemic$paranoia_score <- rowMeans(dat_pandemic[, grepl("rgpts_", names(dat_pandemic))], na.rm = TRUE)


# create dummy variables for the period (time) and group membership (mask_policy)
dat_pandemic = dat_pandemic %>%
  mutate(time = month >= 4,
         mask_policy = state_mask_mandate >= 1)

# renaming boolean values to appropriate labels
dat_pandemic$mask_policy[which(dat_pandemic$mask_policy == "FALSE")] <- "Recommended"
dat_pandemic$mask_policy[which(dat_pandemic$mask_policy == "TRUE")] <- "Required"



# creating dataframe with relevant features
did_df <- data.frame(month = dat_pandemic$month,
                     time = dat_pandemic$time,
                     mask_policy = dat_pandemic$mask_policy,
                     paranoia = dat_pandemic$paranoia_score
                    )


# function to return DiD estimate [(C-A)-(D-B)]
run_DiD <- function(my_data, indices){
  d <- my_data[indices,]
  return(
    mean(d[which(d$time == "TRUE" & d$mask_policy == "Required"),]$paranoia) - 
      mean(d[which(d$time == "FALSE" & d$mask_policy == "Required"),]$paranoia) - 
      (mean(d[which(d$time == "TRUE" & d$mask_policy == "Recommended"),]$paranoia) - 
         mean(d[which(d$time == "FALSE" & d$mask_policy == "Recommended"),]$paranoia))
  )
}


regression_DiD = lm(paranoia_score ~ time*mask_policy, data = dat_pandemic)
summary(regression_DiD)
## Statistics
# F(3,529) = 10.62
# p(DiD) = 0.0328
# DiD estimate = 0.39621 (or a 39.6% increase)


# perform bootstrapping
set.seed(555)
boot_est <- boot(did_df, run_DiD, R=1000, parallel="multicore", ncpus = 2)
boot_est

# calculate bootstrapped p-value
pvalue <- mean(abs(boot_est$t0) < abs(boot_est$t-mean(boot_est$t)))
pvalue
# 0.038

# other measures to calculate using bootstrapped values
quantile(boot_est$t, c(0.025, 0.975))
boot_est$t0/sd(boot_est$t)
plot(density(boot_est$t))
plot(boot_est$t, type="l")


# Figure 4B: DiD 
fig_4b <- ggplot(did_df, aes(x=month, y=paranoia, color = mask_policy)) +
  stat_summary(geom = 'smooth', size=2) +
  geom_vline(xintercept = 3, linetype = 'dashed', size=1) +
  labs(x = "",
       y = "",
       color = "")+ 
  scale_color_manual(values=c("#FFA500","#008000"))+
  scale_x_continuous(breaks = c(1,2,3,4,5),
                     labels = c("Jan","Mar","Apr","Jun","Jul"))

fig_4b + theme_Publication() + theme(axis.title.y = element_blank(),
                                 axis.title.x = element_blank(),
                                 axis.text = element_blank(), 
                                 axis.line = element_line(colour="black", size = 1.5),
                                 axis.ticks = element_line(colour="black", size = 1.5),
                                 legend.text = element_blank(),
                                 legend.position = "none")

################################################################################################
################################################################################################
################################################################################################
rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/revisions/data")

# load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
#install.packages("devtools")
library(devtools)
#devtools::install_github("kassambara/easyGgplot2")
#library(easyGgplot2)
library(openintro)
library(plotrix)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(plotrix)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(reshape2)
library(openintro)
library(lubridate)
library(anytime)

source('C:\\Pandemic_2020\\revisions\\code\\theme_publication.R')

# read data
dat <- read.csv('data.csv')


#### WSR, Mu3 and protesting in required vs recommended states
pandemic_postlockdown <- dat[which(dat$dataset == "pandemic" & dat$period == "postlockdown"),]


pandemic_postlockdown_df <- data.frame(mwr = pandemic_postlockdown$state_mask_mandate,
                                       wsr = rowMeans(cbind(pandemic_postlockdown$wsr_block1,
                                                            pandemic_postlockdown$wsr_block2)),
                                       lsr = rowMeans(cbind(pandemic_postlockdown$lsr_block1,
                                                            pandemic_postlockdown$lsr_block2)),
                                       mu03_block1 = pandemic_postlockdown$mu03_1,
                                       mu03_block1_precision = pandemic_postlockdown$mu03_1_precision,
                                       mu02_block1 = pandemic_postlockdown$mu02_1,
                                       mu02_block1_precision = pandemic_postlockdown$mu02_1_precision
                                    )

# how many participants in the reopening period?
length(pandemic_postlockdown_df$mwr)

# Of those, how many are located in mask-recommended states?
length(pandemic_postlockdown_df[which(pandemic_postlockdown_df$mwr == 0),]$mwr)

# mask-required states?
length(pandemic_postlockdown_df[which(pandemic_postlockdown_df$mwr == 1),]$mwr)


### 3.1.2: Win-switch rate (top figure)
fig4b_1 <- ggplot(pandemic_postlockdown_df, aes(x = mwr,
                                                y= wsr,
                                                fill = as.factor(mwr))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("0" = "#FFA500", "1" = "#008000"))


fig4b_1 + theme_Publication() + theme(axis.title.y = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.text = element_blank(), 
                                      axis.line = element_line(colour="black", size = 1.5),
                                      axis.ticks = element_line(colour="black", size = 1.5),
                                      legend.text = element_blank(),
                                      legend.position = "none")
# WSR
t.test(pandemic_postlockdown_df$wsr~ pandemic_postlockdown_df$mwr,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

# Effect size
install.packages("lsr")
library(lsr)

cohensD(wsr ~ mwr,
        data = pandemic_postlockdown_df)

# LSR
t.test(pandemic_postlockdown_df$lsr~ pandemic_postlockdown_df$mwr,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)


# fdr correction using WSR and LSR
pvalues_behavior <- c(0.01986, 0.1357)
pvalues_behavior_sorted <- sort(pvalues_behavior)
p.adjust(pvalues_behavior_sorted, method = "BH")




### 3.3: weighted Mu3 
fig4b_2 <- ggplot(pandemic_postlockdown_df, aes(x = mwr,
                                                y= mu03_block1,
                                                fill = as.factor(mwr),
                                                size = mu03_block1_precision)) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(mapping = aes(weight = mu03_block1_precision),
               alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("0" = "#FFA500", "1" = "#008000"))


fig4b_2 + theme_Publication() + theme(axis.title.y = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.text = element_blank(), 
                                      axis.line = element_line(colour="black", size = 1.5),
                                      axis.ticks = element_line(colour="black", size = 1.5),
                                      legend.text = element_blank(),
                                      legend.position = "none")


# weighted t-test: mu03
library(weights)
mu03_wtdttest <- wtd.t.test(x=pandemic_postlockdown_df$mu03_block1[pandemic_postlockdown_df$mwr==0],
           y=pandemic_postlockdown_df$mu03_block1[pandemic_postlockdown_df$mwr==1],
           weight=pandemic_postlockdown_df$mu03_block1_precision[pandemic_postlockdown_df$mwr==0],
           weighty=pandemic_postlockdown_df$mu03_block1_precision[pandemic_postlockdown_df$mwr==1],
           samedata=FALSE)# t(141) = 0.62
                          # p-value = 0.00027 (2.680758e-04)
                          # wtd.mu[recommended] = -0.51
                          # wtd.mu[required] = 0.28

# 95% CI calculation
t_score <- mu03_wtdttest$coefficients[1]
sample_se <- mu03_wtdttest$additional[4]
sample_mean_difference <- mu03_wtdttest$additional[1]
margin_error <- t_score*sample_se
lower_bound <- sample_mean_difference - margin_error
upper_bound <- sample_mean_difference + margin_error
print(c(lower_bound,upper_bound))

# Cohen's d
Cohen_d = sample_mean/sample_se

# weighted t-test: mu02
wtd.t.test(x=pandemic_postlockdown_df$mu02_block1[pandemic_postlockdown_df$mwr==0],
           y=pandemic_postlockdown_df$mu02_block1[pandemic_postlockdown_df$mwr==1],
           weight=pandemic_postlockdown_df$mu02_block1_precision[pandemic_postlockdown_df$mwr==0],
           weighty=pandemic_postlockdown_df$mu02_block1_precision[pandemic_postlockdown_df$mwr==1],
           samedata=FALSE)# t(141) = 0.62
# p-value = 0.02 (0.02370412)
# wtd.mu[recommended] = -0.21
# wtd.mu[required] = -0.10





# fdr correction using mu03 and mu02
pvalues_belief <- c(2.680758e-04, 0.02370412)
pvalues_belief_sorted <- sort(pvalues_belief)
p.adjust(pvalues_belief_sorted, method = "BH")


## Protests
dat_protest <- read.csv("Figure4B_protest.csv")
dat_protest <- dat_protest[which(dat_protest$EVENT_TYPE == "Protests"),]

protest_df <- data.frame(date = dat_protest$EVENT_DATE,
                         date2 = anydate(dat_protest$EVENT_DATE),
                         type = dat_protest$EVENT_TYPE,
                         #association = dat_protest$ASSOC_ACTOR_1,
                         state = dat_protest$ADMIN1)

protest_df$state <- state2abbr(protest_df$state)


mask_required_states <- c("CA","NM","MI","IL","NY","MA","RI","MD","VA","ME","DE")
mask_recommend_states <- c("AK","AL","AR","AZ","CO","CT","DC","FL","GA","HI","IA",
                          "ID","IN","KS","KY","LA","MN","MO","MS","MT","NC","ND",
                          "NE","NH","NJ","NV","OH","OK","OR","PA","SC","SD","TN",
                          "TX","UT","VT","WA","WI","WV","WY")


protest_df$mask <- ifelse(protest_df$state %in% mask_required_states, "required",
                          ifelse(protest_df$state %in% mask_recommend_states, "recommended","")
)
# analyze data up to July 17th (end date of our pandemic data collection)
protest_subset_df <- protest_df[c(1:8656),]


protest_count_summary <- protest_subset_df %>%
  dplyr::group_by(date2,mask) %>%
  dplyr::summarise(protest_count = n())

protest_count_summary$mask <- factor(protest_count_summary$mask, levels = c("recommended","required"))

fig4b_3 <- ggplot(protest_count_summary, aes(x = mask,
                                             y= protest_count,
                                             fill = as.factor(mask))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("recommended" = "#FFA500", "required" = "#008000"))


fig4b_3 + theme_Publication() + theme(axis.title.y = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.text = element_blank(), 
                                      axis.line = element_line(colour="black", size = 1.5),
                                      axis.ticks = element_line(colour="black", size = 1.5),
                                      legend.text = element_blank(),
                                      legend.position = "none")

# Two-sample Welch's t-test for protest count
t.test(protest_count_summary$protest_count ~ protest_count_summary$mask,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(protest_count ~ mask, protest_count_summary)

################################################################################################
################################################################################################
################################################################################################
rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/revisions/data")

# load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(plotrix)

source("C:/Pandemic_2020/revisions/code/theme_publication.R")
# read data
dat <- read.csv('data.csv')
dynata <- read.csv('dynata.csv')


# extract pandemic, postlockdown data
pandemic_reopening <- dat[which(dat$dataset == "pandemic" & dat$period == "postlockdown"),]


# june.masks.req.nine <- c("CA","NM","MI","IL","NY","MA","RI","MD","VA")
# june.states.mask.rec <- c("AK","AL","AR","AZ","CO","CT","DC","FL","GA","HI","IA",
#                           "ID","IN","KS","KY","LA","MN","MO","MS","MT","NC","ND",
#                           "NE","NH","NJ","NV","OH","OK","OR","PA","SC","SD","TN",
#                           "TX","UT","VT","WA","WI","WV","WY")

# list of states with mask policies
states_june_mwr <- c("CA","NM","MI","IL","NY","MA","RI","MD","VA",
                     "AK","AL","AR","AZ","CO","CT","DC","FL","GA","HI","IA",
                     "ID","IN","KS","KY","LA","MN","MO","MS","MT","NC","ND",
                     "NE","NH","NJ","NV","OH","OK","OR","PA","SC","SD","TN",
                     "TX","UT","VT","WA","WI","WV","WY")

# extract dynata data for those states
dynata <- dynata[which(dynata$state %in% states_june_mwr),]
dynata.df <- data.frame(state = dynata$state,
                        mask.response = dynata$mask,
                        value = dynata$respondents)

# recode mask_response 
dynata.df$mask.response <- ifelse(dynata.df$mask.response == "Always",5,
                                  ifelse(dynata.df$mask.response == "Frequently",4,
                                         ifelse(dynata.df$mask.response == "Sometimes",3,
                                                ifelse(dynata.df$mask.response == "Rarely",2,
                                                       ifelse(dynata.df$mask.response == "Not at all",1,"")))))

dynata.df$mask.response <- as.numeric(dynata.df$mask.response)

dynata.count <- dynata.df %>%
  dplyr::group_by(state, mask.response) %>%
  dplyr::summarise(count.respondents = sum(value, na.rm = TRUE))


dynata.sum <- dynata.count %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(sum.count = sum(count.respondents, na.rm = TRUE))

dynata.relFreq <- dynata.sum %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(rel.freq = count.respondents/sum.count)

dynata.fiveAlways <- dynata.relFreq[which(dynata.relFreq$mask.response == 5),]


colnames(dynata.fiveAlways) <- c("State","mask.response","count.respondents","sum.count","rel.freq")





# create dataframe with relevant features
pandemic_reopening_df <- data.frame(State = pandemic_reopening$state,
                                    MWR = pandemic_reopening$state_mask_mandate,
                                    CTL = pandemic_reopening$state_ctl,
                                    wsr = rowMeans(cbind(pandemic_reopening$wsr_block1,
                                                         pandemic_reopening$wsr_block2)),
                                    lsr = rowMeans(cbind(pandemic_reopening$lsr_block1,
                                                         pandemic_reopening$lsr_block2)),
                                    mu03 = rowMeans(cbind(pandemic_reopening$mu03_1,
                                                         pandemic_reopening$mu03_2)),
                                    mu02 = rowMeans(cbind(pandemic_reopening$mu02_1,
                                                         pandemic_reopening$mu02_2)),
                                    mu03_precision = rowMeans(cbind(pandemic_reopening$mu03_1_precision,
                                                                   pandemic_reopening$mu03_2_precision)),
                                    mu02_precision = rowMeans(cbind(pandemic_reopening$mu02_1_precision,
                                                                   pandemic_reopening$mu02_2_precision)))


pandemic_reopening_df$CTL_group <- ifelse(pandemic_reopening_df$CTL <= median(pandemic_reopening_df$CTL, na.rm = TRUE), "loose","tight")


pandemic_reopening_dynata <- left_join(pandemic_reopening_df, dynata.fiveAlways)


# recommended states
pandemic_reopening_dynata_rec <- pandemic_reopening_dynata[which(pandemic_reopening_dynata$MWR == 0),]

# How many loose, recommended states?
nrow(pandemic_reopening_dynata_rec[which(pandemic_reopening_dynata_rec$CTL_group == "loose"),])
# tight, recommended states?
nrow(pandemic_reopening_dynata_rec[which(pandemic_reopening_dynata_rec$CTL_group == "tight"),])


stats_rec <- pandemic_reopening_dynata_rec %>%
  dplyr::group_by(CTL_group) %>%
  dplyr::summarise(mask_wearing_belief_mean = mean(rel.freq, na.rm = TRUE),
                   mask_wearing_belief_sem = std.error(rel.freq, na.rm = TRUE))



fig4c_1 <- ggplot(stats_rec, aes(x = CTL_group, y= mask_wearing_belief_mean, fill = as.factor(CTL_group))) +
  geom_bar(stat="identity", width=.5, color = "black", lwd=1.2) +
  geom_errorbar(aes(x=CTL_group, ymin=mask_wearing_belief_mean-mask_wearing_belief_sem, ymax=mask_wearing_belief_mean+mask_wearing_belief_sem), width=0.2, colour="black", alpha=0.9, size=2) +
  geom_point(data=pandemic_reopening_dynata_rec, aes(x=CTL_group, y=rel.freq), position = position_jitter(width = .15),
             shape=21, fill="black", size=1.5) +
  scale_fill_manual("",values = c("#9370DB", "#8B008B"))

fig4c_1 + theme_Publication() + theme(axis.title.y = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.text = element_blank(), 
                                      axis.line = element_line(colour="black", size = 1.5),
                                      axis.ticks = element_line(colour="black", size = 1.5),
                                      legend.text = element_blank(),
                                      legend.position = "none")


# t-test: mask-wearing belief difference by CTL in mask-recommended states
# 38 - loose, 82 - tight
t.test(pandemic_reopening_dynata_rec$rel.freq ~ pandemic_reopening_dynata_rec$CTL_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(rel.freq ~ CTL_group, pandemic_reopening_dynata_rec)


# required states
pandemic_reopening_dynata_req <- pandemic_reopening_dynata[which(pandemic_reopening_dynata$MWR == 1),]

# How many loose, required states?
nrow(pandemic_reopening_dynata_req[which(pandemic_reopening_dynata_req$CTL_group == "loose"),])
# tight, required states?
nrow(pandemic_reopening_dynata_req[which(pandemic_reopening_dynata_req$CTL_group == "tight"),])



stats_req <- pandemic_reopening_dynata_req %>%
  dplyr::group_by(CTL_group) %>%
  dplyr::summarise(mask_wearing_belief_mean = mean(rel.freq, na.rm = TRUE),
                   mask_wearing_belief_sem = std.error(rel.freq, na.rm = TRUE))



fig4c_2 <- ggplot(stats_req, aes(x = CTL_group, y= mask_wearing_belief_mean, fill = as.factor(CTL_group))) +
  geom_bar(stat="identity", width=.5, color = "black", lwd=1.2) +
  geom_errorbar(aes(x=CTL_group, ymin=mask_wearing_belief_mean-mask_wearing_belief_sem, ymax=mask_wearing_belief_mean+mask_wearing_belief_sem), width=0.2, colour="black", alpha=0.9, size=2) +
  geom_point(data=pandemic_reopening_dynata_req, aes(x=CTL_group, y=rel.freq), position = position_jitter(width = .15),
             shape=21, fill="black", size=1.5) +
  scale_fill_manual("",values = c("#9370DB", "#8B008B"))

fig4c_2 + theme_Publication() + theme(axis.title.y = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.text = element_blank(), 
                                      axis.line = element_line(colour="black", size = 1.5),
                                      axis.ticks = element_line(colour="black", size = 1.5),
                                      legend.text = element_blank(),
                                      legend.position = "none")


# t-test: mask-wearing belief difference by CTL in mask-required states
# 48 - loose, 4 - tight
t.test(pandemic_reopening_dynata_req$rel.freq ~ pandemic_reopening_dynata_req$CTL_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(rel.freq ~ CTL_group, pandemic_reopening_dynata_req)

## sample size summary
# recommended states by CTL
rec_total <- nrow(pandemic_reopening_dynata_rec)
rec_loose <- nrow(pandemic_reopening_dynata_rec[which(pandemic_reopening_dynata_rec$CTL_group == "loose"),])
rec_tight <- nrow(pandemic_reopening_dynata_rec[which(pandemic_reopening_dynata_rec$CTL_group == "tight"),])
# required states by CTL
req_total <- nrow(pandemic_reopening_dynata_req)
req_loose <- nrow(pandemic_reopening_dynata_req[which(pandemic_reopening_dynata_req$CTL_group == "loose"),])
req_tight <- nrow(pandemic_reopening_dynata_req[which(pandemic_reopening_dynata_req$CTL_group == "tight"),])


# Figure 4C (recommended states)
p1 <- ggplot(pandemic_reopening_dynata_rec, aes(x=CTL_group, 
                                                y=rel.freq,
                                                fill = CTL_group)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(name = "Mask-wearing belief") +
  scale_x_discrete(name = "CTL") +
  labs(title = "Mask-recommended states",
       subtitle = bquote("(N" == .(rec_total) * 
                           ";" ~ 
                           n[loose] == .(rec_loose) ~
                           "," ~
                           n[tight] == .(rec_tight)*
                           ")" )
  ) +
  scale_fill_manual(name = "CTL", 
                    values = c("loose" = "#9370DB", "tight" = "#8B008B"))

p1 <- p1 + theme_Publication()
p1


# Figure 4C (required states)
p2 <- ggplot(pandemic_reopening_dynata_req, aes(x=CTL_group, 
                                                y=rel.freq,
                                                fill = CTL_group)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(name = "Mask-wearing belief") +
  scale_x_discrete(name = "CTL") +
  labs(title = "Mask-mandated states",
       subtitle = bquote("(N" == .(req_total) * 
                           ";" ~ 
                           n[loose] == .(req_loose) ~
                           "," ~
                           n[tight] == .(req_tight)*
                           ")" )
  ) +
  scale_fill_manual(name = "CTL", 
                    values = c("loose" = "#9370DB", "tight" = "#8B008B"))

p2 <- p2 + theme_Publication()
p2



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
dat <- read.csv("data.csv", 
                stringsAsFactors = FALSE)


# subset for replication data
dat_replication <- dat[which(dat$dataset == "replication"),]



# calculate paranoia scores
dat_replication$paranoia_score <- rowMeans(dat_replication[, grepl("gpts_", names(dat_replication))], na.rm = TRUE)

# calculate paranoia grouping
dat_replication$paranoia_group <- ifelse(rowSums(dat_replication[, grepl("gpts_per", names(dat_replication))], na.rm = TRUE) > 10, "high","low")


# recode and calculate covid vaccine conspiracy belief score and vaccine attitude
dat_replication_df <- dat_replication %>%
  select(paranoia_score, paranoia_group,
         mask_behavior_1:mask_behavior_16,
         #qanon_rating,
         #covid_conspiracy_vaccine_1:covid_conspiracy_vaccine_5,
         #vaccine_scale_1:vaccine_scale_5,
         names(dat_replication[, grepl("wsr_", names(dat_replication))]),
         names(dat_replication[, grepl("lsr_", names(dat_replication))]),
         names(dat_replication[, grepl("mu02_", names(dat_replication))]),
         names(dat_replication[, grepl("mu03_", names(dat_replication))]),
         names(dat_replication[, grepl("kappa2_", names(dat_replication))]),
         names(dat_replication[, grepl("omega2_", names(dat_replication))]),
         names(dat_replication[, grepl("omega3_", names(dat_replication))]))


# calculate mask attitude score (did not include Q2,Q4,Q5)
# Question 1
dat_replication_df$mask_behavior_1 <- ifelse(dat_replication_df$mask_behavior_1 == "Yes",1,
                                             ifelse(dat_replication_df$mask_behavior_1 == "No",-1,""))

# convert character to numeric
dat_replication_df$mask_behavior_1 <- as.data.frame(sapply(dat_replication_df$mask_behavior_1, as.numeric))


# Question 2
dat_replication_df$mask_behavior_2 <- ifelse(dat_replication_df$mask_behavior_2 == "Yes",1,
                                             ifelse(dat_replication_df$mask_behavior_2 == "No",-1,""))

#if prior question was a 'no'
#dat_replication_df <- within(dat_replication_df, mask_behavior_2[mask_behavior_1 != -1 & !is.na(mask_behavior_2)] <- '')


# convert character to numeric
dat_replication_df$mask_behavior_2 <- as.data.frame(sapply(dat_replication_df$mask_behavior_2, as.numeric))


# Question 3
dat_replication_df[,5:10] <- ifelse(dat_replication_df[,5:10] == "Extremely unlikely",-1,
                                    ifelse(dat_replication_df[,5:10] == "Unlikely",-0.5,
                                           ifelse(dat_replication_df[,5:10] == "No plans to go",0,
                                                  ifelse(dat_replication_df[,5:10] == "Likely",0.5,
                                                         ifelse(dat_replication_df[,5:10] == "Extremely likely",1,"")))))

# convert character to numeric
dat_replication_df[,5:10] <- as.data.frame(sapply(dat_replication_df[,5:10], as.numeric))


# Question 4
dat_replication_df$mask_behavior_9  <- ifelse(dat_replication_df$mask_behavior_9 == "Yes",1,
                                              ifelse(dat_replication_df$mask_behavior_9 == "Not sure",0,
                                                     ifelse(dat_replication_df$mask_behavior_9 == "No",-1,"")))

# convert character to numeric
dat_replication_df$mask_behavior_9 <- as.data.frame(sapply(dat_replication_df$mask_behavior_9, as.numeric))


# Question 5
dat_replication_df[,12:14] <- ifelse(dat_replication_df[,12:14] >= 0 & dat_replication_df[,12:14] < 25,-1,
                                     ifelse(dat_replication_df[,12:14] >=25 & dat_replication_df[,12:14] < 50,-0.5,
                                            ifelse(dat_replication_df[,12:14] == 50,0,
                                                   ifelse(dat_replication_df[,12:14] > 50 & dat_replication_df[,12:14] <= 75,0.5,
                                                          ifelse(dat_replication_df[,12:14] > 75 & dat_replication_df[,12:14] <= 100,1,"")))))


# convert character to numeric
dat_replication_df[,12:14] <- as.data.frame(sapply(dat_replication_df[,12:14], as.numeric))




# Question 6
dat_replication_df$mask_behavior_13 <- ifelse(dat_replication_df$mask_behavior_13 == "More comfortable",1,
                                              ifelse(dat_replication_df$mask_behavior_13 == "Indifferent",0,
                                                     ifelse(dat_replication_df$mask_behavior_13 == "Less comfortable",-1,"")))

# convert character to numeric
dat_replication_df$mask_behavior_13 <- as.data.frame(sapply(dat_replication_df$mask_behavior_13, as.numeric))

# Question 7
dat_replication_df$mask_behavior_14 <- ifelse(dat_replication_df$mask_behavior_14 == "More comfortable",1,
                                              ifelse(dat_replication_df$mask_behavior_14 == "Indifferent",0,
                                                     ifelse(dat_replication_df$mask_behavior_14 == "Less comfortable",-1,"")))

# convert character to numeric
dat_replication_df$mask_behavior_14 <- as.data.frame(sapply(dat_replication_df$mask_behavior_14, as.numeric))

# Question 8
dat_replication_df$mask_behavior_15 <- ifelse(dat_replication_df$mask_behavior_15 == "Yes, a lot",1,
                                              ifelse(dat_replication_df$mask_behavior_15 == "Yes, some",0.5,
                                                     ifelse(dat_replication_df$mask_behavior_15 == "Not sure",0,
                                                            ifelse(dat_replication_df$mask_behavior_15 == "No, it does nothing",-0.5,
                                                                   ifelse(dat_replication_df$mask_behavior_15 == "No, it increases the spread",-1,"")))))

# convert character to numeric
dat_replication_df$mask_behavior_15 <- as.data.frame(sapply(dat_replication_df$mask_behavior_15, as.numeric))

# Question 9
dat_replication_df$mask_behavior_16 <- ifelse(dat_replication_df$mask_behavior_16 == "Yes",-1,
                                              ifelse(dat_replication_df$mask_behavior_16 == "Not sure",0,
                                                     ifelse(dat_replication_df$mask_behavior_16 == "No",1,"")))
# convert character to numeric
dat_replication_df$mask_behavior_16 <- as.data.frame(sapply(dat_replication_df$mask_behavior_16, as.numeric))


# compute mask attitude score
dat_replication_df$mask_attitude_score <- rowMeans(dat_replication_df[, c(3:18)], na.rm = TRUE)


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


dat_replication_df$paranoia_group <- factor(dat_replication_df$paranoia_group, levels = c("low","high"))


# how many participants in the replication study set?
nrow(dat_replication_df)
# how many are low paranoia?
nrow(dat_replication_df[which(dat_replication_df$paranoia_group == "low"),])
# high?
nrow(dat_replication_df[which(dat_replication_df$paranoia_group == "high"),])

fig4d_1 <- ggplot(dat_replication_df, aes(x = paranoia_group,
                                          y= mask_attitude_score,
                                          fill = as.factor(paranoia_group))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("low" = "#FADBD8", "high" = "#E74C3C"))


fig4d_1 + theme_Publication() + theme(axis.title.y = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.text = element_blank(), 
                                      axis.line = element_line(colour="black", size = 1.5),
                                      axis.ticks = element_line(colour="black", size = 1.5),
                                      legend.text = element_blank(),
                                      legend.position = "none")



# Mask attitude
t.test(dat_replication_df$mask_attitude_score ~ dat_replication_df$paranoia_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(mask_attitude_score ~ paranoia_group, dat_replication_df)





fig4d_2 <- ggplot(dat_replication_df, aes(x = paranoia_group,
                                          y= wsr,
                                          fill = as.factor(paranoia_group))) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("low" = "#FADBD8", "high" = "#E74C3C"))


fig4d_2 + theme_Publication() + theme(axis.title.y = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.text = element_blank(), 
                                      axis.line = element_line(colour="black", size = 1.5),
                                      axis.ticks = element_line(colour="black", size = 1.5),
                                      legend.text = element_blank(),
                                      legend.position = "none")



# WSR
t.test(dat_replication_df$wsr ~ dat_replication_df$paranoia_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(wsr ~ paranoia_group, dat_replication_df)

# LSR
t.test(dat_replication_df$lsr ~ dat_replication_df$paranoia_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)


# fdr correction using WSR and LSR
pvalues_behavior <- c(5.077e-09, 0.0003023)
pvalues_behavior_sorted <- sort(pvalues_behavior)
pvalues_behavior_sorted 
p.adjust(pvalues_behavior_sorted, method = "BH")





### 3.3: weighted Mu3 
fig4d_3 <- ggplot(dat_replication_df, aes(x = paranoia_group,
                                          y= mu03,
                                          fill = as.factor(paranoia_group),
                                          size = mu03_precision)) +
  geom_point(shape=16,color="black",alpha=0.4,position = position_jitterdodge(), show.legend = FALSE) + 
  geom_boxplot(mapping = aes(weight = mu03_precision),
               alpha = 0.7, width=0.5, lwd=1.2) +
  scale_fill_manual(name = "", 
                    values = c("low" = "#FADBD8", "high" = "#E74C3C"))


fig4d_3 + theme_Publication() + theme(axis.title.y = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.text = element_blank(), 
                                      axis.line = element_line(colour="black", size = 1.5),
                                      axis.ticks = element_line(colour="black", size = 1.5),
                                      legend.text = element_blank(),
                                      legend.position = "none")


# weighted t-test: mu03
mu03_mask_attitude_wtdttest <- wtd.t.test(x=dat_replication_df$mu03[dat_replication_df$paranoia_group=="low"],
           y=dat_replication_df$mu03[dat_replication_df$paranoia_group=="high"],
           weight=dat_replication_df$mu03_precision[dat_replication_df$paranoia_group=="low"],
           weighty=dat_replication_df$mu03_precision[dat_replication_df$paranoia_group=="high"],
           samedata=FALSE)# t(138) = -6.041
                          # p-value < 0.001 (1.349674e-08)
                          # wtd.mu[low] = -1.23
                          # wtd.mu[high] = -0.20


# 95% CI calculation
t_score <- mu03_mask_attitude_wtdttest$coefficients[1]
sample_se <- mu03_mask_attitude_wtdttest$additional[4]
sample_mean_difference <- mu03_mask_attitude_wtdttest$additional[1]
margin_error <- t_score*sample_se
lower_bound <- sample_mean_difference - margin_error
upper_bound <- sample_mean_difference + margin_error
print(c(lower_bound,upper_bound))

# Cohen's d
Cohen_d = sample_mean/sample_se
Cohen_d

# weighted t-test: mu02
wtd.t.test(x=dat_replication_df$mu02[dat_replication_df$paranoia_group=="low"],
           y=dat_replication_df$mu02[dat_replication_df$paranoia_group=="high"],
           weight=dat_replication_df$mu02_precision[dat_replication_df$paranoia_group=="low"],
           weighty=dat_replication_df$mu02_precision[dat_replication_df$paranoia_group=="high"],
           samedata=FALSE)# t(177) = -3.286
                          # p-value < 0.001 (0.001225297)
                          # wtd.mu[low] = -0.258
                          # wtd.mu[high] = -0.151



# fdr correction using mu03 and mu02
pvalues_belief <- c(1.349674e-08, 0.001225297)
pvalues_belief_sorted <- sort(pvalues_belief)
pvalues_belief_sorted
p.adjust(pvalues_belief_sorted, method = "BH")


































