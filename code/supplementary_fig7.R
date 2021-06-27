### Analysis: Investigate differences in demographic variables across period
###
### Description: Perform one way ANOVA on demographic features
###                 
###                 
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
library(plotrix)
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


dat_pandemic_df <- data.frame(period = dat_pandemic$period,
                              gender = as.numeric(dat_pandemic$demo_1),
                              age = dat_pandemic$demo_2,
                              race = as.numeric(dat_pandemic$demo_4),
                              edu = as.numeric(dat_pandemic$demo_5),
                              emp = as.numeric(dat_pandemic$demo_6),
                              inc = as.numeric(dat_pandemic$demo_7),
                              medi = as.numeric(dat_pandemic$demo_8),
                              diag = as.numeric(dat_pandemic$demo_9),
                              mask = as.numeric(dat_pandemic$state_mask_mandate))

# code employment as not employed (0) or employed (1)
#dat_pandemic_df$emp <- ifelse(dat_pandemic_df$emp == 1 | dat_pandemic_df$emp == 2 | dat_pandemic_df$emp == 8,1,0)


# Conduct one-way ANOVA for gender
dat_pandemic_genderDF <- dat_pandemic_df[which(dat_pandemic_df$gender == 1 | dat_pandemic_df$gender == 2),]
aov1_gender <- aov(dat_pandemic_genderDF$gender ~ dat_pandemic_genderDF$period)
summary(aov1_gender) 
aov1_genderII <- Anova(aov1_gender, type = "II") # not significant; p=0.711
aov1_genderII
etaSquared(aov1_gender, type = 2, anova = TRUE)

# Compute Bayes Factor for one-way ANOVA (gender)
dat_pandemic_genderDF$period <- as.factor(dat_pandemic_genderDF$period)
anovaBF(gender ~ period, data = dat_pandemic_genderDF) # BF(10) = 0.03


# Conduct one-way ANOVA for age
aov1_age <- aov(dat_pandemic_df$age ~ dat_pandemic_df$period)
summary(aov1_age) 
aov1_ageII <- Anova(aov1_age, type = "II") # not significant; p=0.101
aov1_ageII
etaSquared(aov1_age, type = 2, anova = TRUE)

# Compute Bayes Factor for one-way ANOVA (age)
dat_pandemic_ageDF <- dat_pandemic_df[-which(is.na(dat_pandemic_df$age)),]
dat_pandemic_ageDF$period <- as.factor(dat_pandemic_ageDF$period)
anovaBF(age ~ period, data = dat_pandemic_ageDF) # BF(10) = 0.19


# Conduct one-way ANOVA for race
aov1_race <- aov(dat_pandemic_df$race ~ dat_pandemic_df$period)
summary(aov1_race) 
aov1_raceII <- Anova(aov1_race, type = "II") # not significant; p=0.333
aov1_raceII
etaSquared(aov1_race, type = 2, anova = TRUE)

# Compute Bayes Factor for one-way ANOVA (race)
dat_pandemic_raceDF <- dat_pandemic_df[-which(is.na(dat_pandemic_df$race)),]
dat_pandemic_raceDF$period <- as.factor(dat_pandemic_raceDF$period)
anovaBF(race ~ period, data = dat_pandemic_raceDF) # BF(10) = 0.06

# Conduct one-way ANOVA for edu
aov1_edu <- aov(dat_pandemic_df$edu ~ dat_pandemic_df$period)
summary(aov1_edu) 
aov1_eduII <- Anova(aov1_edu, type = "II") # not significant; p=0.616
aov1_eduII
etaSquared(aov1_edu, type = 2, anova = TRUE)

# Compute Bayes Factor for one-way ANOVA (edu)
dat_pandemic_df$period <- as.factor(dat_pandemic_df$period)
anovaBF(edu ~ period, data = dat_pandemic_df) # BF(10) = 0.039

# Conduct one-way ANOVA for emp
aov1_emp <- aov(dat_pandemic_df$emp ~ dat_pandemic_df$period)
summary(aov1_emp) 
aov1_empII <- Anova(aov1_emp, type = "II") # not significant; p=0.856
aov1_empII
etaSquared(aov1_emp, type = 2, anova = TRUE)

# Compute Bayes Factor for one-way ANOVA (emp)
dat_pandemic_empDF <- dat_pandemic_df[-which(is.na(dat_pandemic_df$emp)),]
dat_pandemic_empDF$period <- as.factor(dat_pandemic_empDF$period)
anovaBF(emp ~ period, data = dat_pandemic_empDF) # BF(10) = 0.025

# Conduct one-way ANOVA for income
aov1_inc <- aov(as.numeric(dat_pandemic_df$inc) ~ dat_pandemic_df$period)
summary(aov1_inc) 
aov1_incomeII <- Anova(aov1_inc, type = "II") # not significant; p=0.834
aov1_incomeII
etaSquared(aov1_inc, type = 2, anova = TRUE)

# Compute Bayes Factor for one-way ANOVA (inc)
dat_pandemic_incDF <- dat_pandemic_df[-which(is.na(dat_pandemic_df$inc)),]
dat_pandemic_incDF$period <- as.factor(dat_pandemic_incDF$period)
anovaBF(inc ~ period, data = dat_pandemic_incDF) # BF(10) = 0.076

# Conduct one-way ANOVA for self-report medication
aov1_medi <- aov(as.numeric(dat_pandemic_df$medi) ~ dat_pandemic_df$period)
summary(aov1_medi) 
aov1_mediII <- Anova(aov1_medi, type = "II") # not significant; p=0.766
aov1_mediII
etaSquared(aov1_medi, type = 2, anova = TRUE)

# Compute Bayes Factor for one-way ANOVA (medication)
dat_pandemic_mediDF <- dat_pandemic_df[-which(is.na(dat_pandemic_df$medi)),]
dat_pandemic_mediDF$period <- as.factor(dat_pandemic_mediDF$period)
anovaBF(medi ~ period, data = dat_pandemic_mediDF) # BF(10) = 0.036

# Conduct one-way ANOVA for diag
aov1_diag <- aov(as.numeric(dat_pandemic_df$diag) ~ dat_pandemic_df$period)
summary(aov1_diag) 
aov1_diagII <- Anova(aov1_diag, type = "II") # significant; p=0.0357
aov1_diagII
etaSquared(aov1_diag, type = 2, anova = TRUE)

# Compute Bayes Factor for one-way ANOVA (diagnosis)
dat_pandemic_diagDF <- dat_pandemic_df[-which(is.na(dat_pandemic_df$diag)),]
dat_pandemic_diagDF$period <- as.factor(dat_pandemic_diagDF$period)
anovaBF(diag ~ period, data = dat_pandemic_diagDF) # BF(10) = 0.620

pvalues_covariates <- c(0.711, 0.101, 0.517, 0.616, 0.856, 0.834, 0.766, 0.036)
pvalues_covariates_sorted <- sort(pvalues_covariates)
pvalues_covariates_sorted
p.adjust(pvalues_covariates_sorted, method = "BH")


### Visualization
rm(list=ls())

setwd("C:/Pandemic_2020/revisions/data")

library(dplyr)
library(lessR)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(plyr)


# read data
dat <- read.csv("pandemicPRL.csv")

# subset for pandemic data
dat_pandemic <- dat[which(dat$dataset == "elife2020" | dat$dataset == "pandemic"),] 


demo.df <- data.frame(record_id = dat_pandemic$study_id,
                      period = dat_pandemic$period, 
                      Age = dat_pandemic$demo_2,
                      gender = dat_pandemic$demo_1,
                      race = dat_pandemic$demo_4,
                      income = dat_pandemic$demo_7)

demo.df$gender <- ifelse(demo.df$gender == "1",1,
                         ifelse(demo.df$gender == "2",2,
                                ifelse(demo.df$gender == "Other","","")))

demo.df$period <- factor(demo.df$period, levels = c("prelockdown",
                                                    "lockdown",
                                                    "postlockdown"))


ANOVA1_age<- aov(demo.df$Age ~ demo.df$period)
summary(ANOVA1_age)
Anova(ANOVA1_age, type = "II")

ANOVA1_gender<- aov(as.numeric(demo.df$gender) ~ demo.df$period)
summary(ANOVA1_gender)
Anova(ANOVA1_gender, type = "II")

ANOVA1_race<- aov(demo.df$race ~ demo.df$period)
summary(ANOVA1_race)
Anova(ANOVA1_race, type = "II")

demo.df.pre <- demo.df[which(demo.df$period == "prelockdown"),]
demo.df.lock <- demo.df[which(demo.df$period == "lockdown"),]
demo.df.reopen <- demo.df[which(demo.df$period == "postlockdown"),]


## GENDER
# Data prep
demo.df.pre$genderLabel <- ifelse(demo.df.pre$gender == 1, "Male",
                                  ifelse(demo.df.pre$gender == 2, "Female",
                                         ifelse(demo.df.pre$gender == "Other","Other","")))
demo.df.lock$genderLabel <- ifelse(demo.df.lock$gender == 1, "Male",
                                   ifelse(demo.df.lock$gender == 2, "Female",
                                          ifelse(demo.df.lock$gender == "Other","Other","")))
demo.df.reopen$genderLabel <- ifelse(demo.df.reopen$gender == 1, "Male",
                                     ifelse(demo.df.reopen$gender == 2, "Female",
                                            ifelse(demo.df.reopen$gender == "Other","Other","")))


demo.df$genderLabel <- ifelse(demo.df$gender == 1, "Male",
                              ifelse(demo.df$gender == 2, "Female",
                                     ifelse(demo.df$gender == "Other","Other","")))

demo.df <- demo.df[-which(demo.df$genderLabel == ""),]


# Statistics
demo.df.prop <- ddply(demo.df, .(period), summarise,
                      prop = table(gender),
                      gender = names(table(genderLabel)))

demo.df.prop$gender <- factor(demo.df.prop$gender, levels = c("Male", "Female"))

G1_M = 108
G1_F = 94
G2_M = 138
G2_F = 87
G3_M = 101
G3_F = 70

genderG = data.frame(G1 = c(G1_M,G1_F),
                     G2 = c(G2_M, G2_F),
                     G3 = c(G3_M, G3_F))

rownames(genderG) <- c("Male","Female")
colnames(genderG) <- c("Pre-lockdown","Lockdown","Reopening")

chisq.test(genderG, correct = F)

# Plot
PieChart(genderLabel, data = demo.df.pre, fill = c("hotpink","lightblue","#B19CD9"),
         values_size = 2, main = "",labels_cex = 0)

PieChart(genderLabel, data = demo.df.lock, fill = c("hotpink","lightblue","#B19CD9"),
         values_size = 2, main = "",labels_cex = 0)

PieChart(genderLabel, data = demo.df.reopen, fill = c("hotpink","lightblue","#B19CD9"),
         values_size = 2, main = "",labels_cex = 0)

########################################
########################################
########################################


## AGE
# Date prep
# remove NA
missing.age.pre <- is.na(demo.df.pre$Age)
missing.age.lock <- is.na(demo.df.lock$Age)
missing.age.reopen <- is.na(demo.df.reopen$Age)

df.pre <- demo.df.pre[!missing.age.pre,]
df.lock <- demo.df.lock[!missing.age.lock,]
df.reopen <- demo.df.reopen[!missing.age.reopen,]

# name age group
labs <- c(paste(seq(0,95,by=5), seq(0+5-1,100-1,by=5),
                sep = "-"), paste(100,"+",sep = ""))
labs

df.pre$AgeGroup <- cut(df.pre$Age, breaks = c(seq(0,100,by=5),Inf),labels = labs,right = FALSE)
df.lock$AgeGroup <- cut(df.lock$Age, breaks = c(seq(0,100,by=5),Inf),labels = labs,right = FALSE)
df.reopen$AgeGroup <- cut(df.reopen$Age, breaks = c(seq(0,100,by=5),Inf),labels = labs,right = FALSE)

#head(df[c("Age","AgeGroup")],15)

df <- rbind(df.pre,df.lock,df.reopen)

# Statistics
mean(df.pre$Age)
mean(df.lock$Age)
mean(df.reopen$Age)

# Plot
stats <- aggregate(Age~period, df, function(x) c(mean=mean(x), sd=sd(x)))
stats <- data.frame(period=stats[,1],stats[,2])
stats


ggplot(df, aes(x=Age, fill=period)) +
  #geom_histogram(binwidth = .5, colour = "blue", fill = "white") +
  geom_histogram(aes(y=..density..), color="grey30")+ 
  with(stats[stats$period=="Pre-lockdown" ,],stat_function(data=df[df$period=="Pre-lockdown",],fun=dnorm, args=list(mean=mean, sd=sd), color="black",size=2))+
  with(stats[stats$period=="Lockdown" ,],stat_function(data=df[df$period=="Lockdown",],fun=dnorm, args=list(mean=mean, sd=sd), color="black",size=2))+
  with(stats[stats$period=="Reopening" ,],stat_function(data=df[df$period=="Reopening",],fun=dnorm, args=list(mean=mean, sd=sd), color="black",size=2))+
  #with(stats.merge[stats.merge$period=="Lockdown",],stat_function(data=df[df$period=="Lockdown",],fun=dnorm, args=list(mean=mean, sd=sd)))+
  #with(stats.merge[stats.merge$period=="Lockdown",],stat_function(fun=dnorm, n=177, args=list(mean=38.989, sd=2.187))) +
  #with(stats.merge[stats.merge$period=="Reopening",],stat_function(data=df[df$period=="Reopening",],fun=dnorm, args=list(mean=mean, sd=sd)))+
  #with(stats.merge[stats.merge$period=="Reopening",],stat_function(fun=dnorm, n=468, args=list(mean=38.344, sd=2.916))) +
  facet_grid(period~.)+ geom_vline(data = stats, aes(xintercept=mean), linetype = "dashed",
                                   colour = "red4") +
  scale_fill_manual(name="Pandemic period",
                    values = c("#ffaf7b","#d76d77","#7d4a95")) +
  labs(x="",y="") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.line = element_line( colour = "black"),
        legend.position = "none",
        legend.box = "vertical",
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        #strip.background = element_rect(fill="blue"),
        strip.text.y = element_blank()
        #strip.background =element_rect(fill="grey")
        #aspect.ratio = 1
  )






########################################
########################################
########################################

## RACE
# Data prep
demo.df$race.name <- ifelse(demo.df$race == 1, "White",
                                   ifelse(demo.df$race == 2, "Black",
                                          ifelse(demo.df$race == 3, "Asian",
                                                 ifelse(demo.df$race == 4, "American Indian",
                                                        ifelse(demo.df$race == 5, "Multiracial","")))))

demo.df$race.name <- factor(demo.df$race.name, levels = c("White","Black","Asian","American Indian","Multiracial"))


# Statistics
demo.df.prop <- ddply(demo.df, .(period), summarise,
                      prop = table(race),
                      income = names(table(race.name)))

#G1_O = 4
G1_W = 161
G1_B = 19
G1_A = 8
G1_Am = 2
G1_M = 8

#G2_O = 6
G2_W = 186
G2_B = 25
G2_A = 9
G2_Am = 1
G2_M = 4

#G3_O = 3
G3_W = 137
G3_B = 15
G3_A = 7
G3_Am = 5
G3_M = 5

raceG = data.frame(G1 = c(G1_W,G1_B,G1_A,G1_Am,G1_M),
                   G2 = c(G2_W,G2_B,G2_A,G2_Am,G2_M),
                   G3 = c(G3_W,G3_B,G3_A,G3_Am,G3_M))

rownames(raceG) <- c("White","Black","Asian","American Indian","Multiracial")
colnames(raceG) <- c("prelockdown","lockdown","postlockdown")

chisq.test(raceG, correct = F)

# Plot
demo.df.perc <- ddply(demo.df, .(period), summarise,
                      percent = prop.table(table(race.name))*100,
                      race = names(table(race.name)))

demo.df.perc$period <- factor(demo.df.perc$period, levels = c("postlockdown","lockdown","prelockdown"))

ggplot(demo.df.perc, aes(x = period, y = percent)) +
  geom_col(aes(fill = race), width = 0.4) + coord_flip() +  scale_fill_manual("",values = c("#9897A8","#E9B606","#E97E06","#069010","#5595D4")#, 
                                                                              #labels = c("Pre-lockdown",
                                                                              #           "Lockdown",
                                                                              #           "Reopening")
  ) +
  labs(x="",y="") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.line = element_line( colour = "black"),
        #legend.position = "none",
        legend.box = "vertical",
        #axis.text.x=element_blank(),
        #axis.text.y = element_blank(),
        aspect.ratio = 1)


########################################
########################################
########################################

## INCOME
# Data prep
demo.df$income.name <- ifelse(demo.df$income == 1, "Less than $20,000",
                              ifelse(demo.df$income == 2, "$20,000 to $34,999",
                                     ifelse(demo.df$income == 3, "$35,000 to $49,999",
                                            ifelse(demo.df$income == 4, "$50,000 to $74,999",
                                                   ifelse(demo.df$income == 5, "$75,000 to $99,999",
                                                          ifelse(demo.df$income == 6, "Over $100,000",""))))))

demo.df$income.name <- factor(demo.df$income.name, levels = c("Less than $20,000","$20,000 to $34,999","$35,000 to $49,999",
                                                              "$50,000 to $74,999","$75,000 to $99,999","Over $100,000"))

# Statistics
demo.df.prop <- ddply(demo.df, .(period), summarise,
                      prop = table(income),
                      income = names(table(income.name)))

G1_1 = 29
G1_2 = 56
G1_3 = 31
G1_4 = 47
G1_5 = 23
G1_6 = 12

G2_1 = 39
G2_2 = 55
G2_3 = 42
G2_4 = 57
G2_5 = 22
G2_6 = 14

G3_1 = 22
G3_2 = 33
G3_3 = 41
G3_4 = 47
G3_5 = 17
G3_6 = 10

incomeG = data.frame(G1 = c(G1_1,G1_2,G1_3,G1_4,G1_5,G1_6),
                     G2 = c(G2_1,G2_2,G2_3,G2_4,G2_5,G2_6),
                     G3 = c(G3_1,G3_2,G3_3,G3_4,G3_5,G3_6))

rownames(incomeG) <- c("Less than $20,000","$20,000 to $34,999","$35,000 to $49,999","$50,000 to $74,999","$75,000 to $99,999","Over $100,000")
colnames(incomeG) <- c("Pre-lockdown","Lockdown","Reopening")

chisq.test(incomeG, correct = F)

# Plot
demo.df.Inc.perc <- ddply(demo.df, .(period), summarise,
                          percent = prop.table(table(income.name))*100,
                          income = names(table(income.name)))

demo.df.Inc.perc$income <- factor(demo.df.Inc.perc$income, levels = c("Less than $20,000",
                                                                      "$20,000 to $34,999",
                                                                      "$35,000 to $49,999",
                                                                      "$50,000 to $74,999",
                                                                      "$75,000 to $99,999",
                                                                      "Over $100,000"))



ggplot(demo.df.Inc.perc, aes(x = period, y = percent, fill=income)) +
  geom_bar(position="dodge", stat="identity", width = 0.4) + scale_fill_manual("",values = c("#605F6B","#343549","#864E3D","#D8A460","#CA6A36","#99170F")#, 
                                                                               #labels = c("Pre-lockdown",
                                                                               #           "Lockdown",
                                                                               #           "Reopening")
  ) +
  labs(x="",y="") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.line = element_line( colour = "black"),
        #legend.position = "none",
        legend.box = "vertical",
        #axis.text.x=element_blank(),
        #axis.text.y = element_blank(),
        aspect.ratio = 1)





# Calculate Bayes Factor of demographic differences in participants from states that mandated mask-wearing
# and states that recommended mask-wearing

# age
t.test(dat_pandemic_df$age ~ dat_pandemic_df$mask,
        mu=0,
        alt="two.sided",
        conf=0.95,
        var.eq=F,
        paired=F)

dat_pandemic_ageDF <- dat_pandemic_df[-which(is.na(dat_pandemic_df$age)),]
ttestBF(dat_pandemic_ageDF[which(dat_pandemic_ageDF$mask == 0),]$age,
        dat_pandemic_ageDF[which(dat_pandemic_ageDF$mask == 1),]$age)


# gender
t.test(dat_pandemic_df$gender ~ dat_pandemic_df$mask,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

dat_pandemic_genderDF <- dat_pandemic_df[-which(is.na(dat_pandemic_df$gender)),]
ttestBF(dat_pandemic_genderDF[which(dat_pandemic_genderDF$mask == 0),]$gender,
        dat_pandemic_genderDF[which(dat_pandemic_genderDF$mask == 1),]$gender)


# race
t.test(dat_pandemic_df$race ~ dat_pandemic_df$mask,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

dat_pandemic_raceDF <- dat_pandemic_df[-which(is.na(dat_pandemic_df$race)),]
ttestBF(dat_pandemic_raceDF[which(dat_pandemic_raceDF$mask == 0),]$race,
        dat_pandemic_raceDF[which(dat_pandemic_raceDF$mask == 1),]$race)




# Calculate Bayes Factor of demographic differences in participants pre- vs post-reopening 
# between states that mandated mask-wearing and states that recommended mask-wearing

dat_pandemic <- dat[which(dat$dataset == "pandemic" & (dat$period == "prelockdown" |
                                                         dat$period == "lockdown" |
                                                         dat$period == "postlockdown")),]

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
                     gender = dat_pandemic$demo_1,
                     age = dat_pandemic$demo_2,
                     race = dat_pandemic$demo_4
)


# pre-reopening
pre_reopening_dat <- did_df[which(did_df$time == "FALSE"),]

# age
t.test(pre_reopening_dat$age ~ pre_reopening_dat$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(age ~ mask_policy, pre_reopening_dat)

pre_reopening_ageDF <- pre_reopening_dat[-which(is.na(pre_reopening_dat$age)),]
ttestBF(pre_reopening_ageDF[which(pre_reopening_ageDF$mask_policy == "Recommended"),]$age,
        pre_reopening_ageDF[which(pre_reopening_ageDF$mask_policy == "Required"),]$age)

# gender
t.test(pre_reopening_dat$gender ~ pre_reopening_dat$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

pre_reopening_genderDF <- pre_reopening_dat[-which(is.na(pre_reopening_dat$gender)),]
ttestBF(pre_reopening_genderDF[which(pre_reopening_genderDF$mask_policy == "Recommended"),]$gender,
        pre_reopening_genderDF[which(pre_reopening_genderDF$mask_policy == "Required"),]$gender)

# race
t.test(pre_reopening_dat$race ~ pre_reopening_dat$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

pre_reopening_raceDF <- pre_reopening_dat[-which(is.na(pre_reopening_dat$race)),]
ttestBF(pre_reopening_raceDF[which(pre_reopening_raceDF$mask_policy == "Recommended"),]$race,
        pre_reopening_raceDF[which(pre_reopening_raceDF$mask_policy == "Required"),]$race)


# post-reopening
post_reopening_dat <- did_df[which(did_df$time == "TRUE"),]

# age
t.test(post_reopening_dat$age ~ post_reopening_dat$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

post_reopening_ageDF <- post_reopening_dat[-which(is.na(post_reopening_dat$age)),]
ttestBF(post_reopening_ageDF[which(post_reopening_ageDF$mask_policy == "Recommended"),]$age,
        post_reopening_ageDF[which(post_reopening_ageDF$mask_policy == "Required"),]$age)

# gender
t.test(post_reopening_dat$gender ~ post_reopening_dat$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

post_reopening_genderDF <- post_reopening_dat[-which(is.na(post_reopening_dat$gender)),]
ttestBF(post_reopening_genderDF[which(post_reopening_genderDF$mask_policy == "Recommended"),]$gender,
        post_reopening_genderDF[which(post_reopening_genderDF$mask_policy == "Required"),]$gender)

# race
t.test(post_reopening_dat$race ~ post_reopening_dat$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

post_reopening_raceDF <- post_reopening_dat[-which(is.na(post_reopening_dat$race)),]
ttestBF(post_reopening_raceDF[which(post_reopening_raceDF$mask_policy == "Recommended"),]$race,
        post_reopening_raceDF[which(post_reopening_raceDF$mask_policy == "Required"),]$race)