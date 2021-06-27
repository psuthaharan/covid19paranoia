rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/revisions/data")

# load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(openintro)
source("C:/Pandemic_2020/revisions/code/theme_publication.R") # source: https://rpubs.com/Koundy/71792
library(BayesFactor)


cases_deaths <- read.csv('Figure5_us_cases_deaths.csv') # See source of data in paper (Suthaharan et.al., 2021)
cases_deaths$state <- state2abbr(cases_deaths$state)

date_april <- "4/5/2020"
date_july <- "7/17/2020"

cases_deaths_april <- cases_deaths[which(cases_deaths$date == date_april),]
cases_deaths_july <- cases_deaths[which(cases_deaths$date == date_july),]


mask_required_states <- c("CA","NM","MI","IL","NY","MA","RI","MD","VA","DE","ME")
mask_recommended_states <- c("AK","AL","AR","AZ","CO","CT","DC","FL","GA","HI","IA",
                             "ID","IN","KS","KY","LA","MN","MO","MS","MT","NC","ND",
                             "NE","NH","NJ","NV","OH","OK","OR","PA","SC","SD","TN",
                             "TX","UT","VT","WA","WI","WV","WY")

cases_deaths_april$mask_policy <- ifelse(cases_deaths_april$state %in% mask_required_states,"required","recommended")
cases_deaths_july$mask_policy <- ifelse(cases_deaths_july$state %in% mask_required_states,"required","recommended")


## April
# Two-sample Welch's t-test for cases and deaths
# cases
t.test(cases_deaths_april$cases ~ cases_deaths_april$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)


# Compute Bayes Factor: cases
ttestBF(cases_deaths_april[which(cases_deaths_april$mask_policy == "recommended"),]$cases,
        cases_deaths_april[which(cases_deaths_april$mask_policy == "required"),]$cases)

# Two-sample Welch's t-test for deaths
# deaths
t.test(cases_deaths_april$deaths ~ cases_deaths_april$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)


# Compute Bayes Factor: deaths
ttestBF(cases_deaths_april[which(cases_deaths_april$mask_policy == "recommended"),]$deaths,
        cases_deaths_april[which(cases_deaths_april$mask_policy == "required"),]$deaths)


## July
# Two-sample Welch's t-test for cases and deaths
# cases
t.test(cases_deaths_july$cases ~ cases_deaths_july$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

# Compute Bayes Factor: cases
ttestBF(cases_deaths_july[which(cases_deaths_july$mask_policy == "recommended"),]$cases,
        cases_deaths_july[which(cases_deaths_july$mask_policy == "required"),]$cases)


# Two-sample Welch's t-test for deaths
# deaths
t.test(cases_deaths_july$deaths ~ cases_deaths_july$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

# Compute Bayes Factor: deaths
ttestBF(cases_deaths_july[which(cases_deaths_july$mask_policy == "recommended"),]$deaths,
        cases_deaths_july[which(cases_deaths_july$mask_policy == "required"),]$deaths)



##############
# Unemployment

unemployment <- read.csv("us-unemployment.csv") # See source of data in paper (Suthaharan et. al., 2021)
colnames(unemployment) <- c("state","ue_april","ue_june","ue_avg")
unemployment$state <- state2abbr(unemployment$state)

mask_required_states <- c("CA","NM","MI","IL","NY","MA","RI","MD","VA","DE","ME")
mask_recommended_states <- c("AK","AL","AR","AZ","CO","CT","DC","FL","GA","HI","IA",
                             "ID","IN","KS","KY","LA","MN","MO","MS","MT","NC","ND",
                             "NE","NH","NJ","NV","OH","OK","OR","PA","SC","SD","TN",
                             "TX","UT","VT","WA","WI","WV","WY")

unemployment$ue_diff <- unemployment$ue_june-unemployment$ue_april

unemployment$mask_policy <- ifelse(unemployment$state %in% mask_required_states,"required","recommended")


## April
# Two-sample Welch's t-test for unemployment
t.test(unemployment$ue_april ~ unemployment$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

# Compute Bayes Factor: deaths
ttestBF(unemployment[which(unemployment$mask_policy == "recommended"),]$ue_april,
        unemployment[which(unemployment$mask_policy == "required"),]$ue_april)

t.test(unemployment$ue_diff ~ unemployment$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

# Compute Bayes Factor: deaths
ttestBF(unemployment[which(unemployment$mask_policy == "recommended"),]$ue_diff,
        unemployment[which(unemployment$mask_policy == "required"),]$ue_diff)


##################
# gini
gini <- read.csv('gini.csv') # See source of data in paper (Suthaharan et. al., 2021)
colnames(gini) <- c("state","gini_coeff")
gini$state <- state2abbr(gini$state)

mask_required_states <- c("CA","NM","MI","IL","NY","MA","RI","MD","VA","DE","ME")
mask_recommended_states <- c("AK","AL","AR","AZ","CO","CT","DC","FL","GA","HI","IA",
                             "ID","IN","KS","KY","LA","MN","MO","MS","MT","NC","ND",
                             "NE","NH","NJ","NV","OH","OK","OR","PA","SC","SD","TN",
                             "TX","UT","VT","WA","WI","WV","WY")

gini$mask_policy <- ifelse(gini$state %in% mask_required_states,"required","recommended")

t.test(gini$gini_coeff ~ gini$mask_policy,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)


