### Analysis: Summarize subject characteristics in the pandemic lockdown dataset
###
### Description: Perform descriptive statistics on subject characteristics 
###
###
### Table: Supplementary Table 2
### Written by: Praveen Suthaharan

### clear environment
rm(list=ls())

### set working directory
setwd("C:/Pandemic_2020/revisions/checklist/github/covid19paranoia/data/qualtricsPRL")

### install libraries (if you have not loaded a specific library before)


### load libraries
library(dplyr)

### read data
dat <- read.csv("pandemicPRL.csv")

### subset data; we want the data used in our lockdown analysis
dat_lockdown <- dat %>%
  dplyr::filter(dat$dataset == "pandemic" & dat$period == "lockdown")

dat_lockdown$paranoia_group <- ifelse(rowSums(dat_lockdown[, grepl("rgpts_per", names(dat_lockdown))], na.rm = TRUE) > 10, "high","low")

### number of low and high paranoia individuals between nonsocial and social tasks
## nonsocial
dat_lockdown_nonsocial <- dat_lockdown[which(dat_lockdown$task_type == "nonsocial"),]  
dat_lockdown_nonsocial_low <- dat_lockdown_nonsocial[which(dat_lockdown_nonsocial$paranoia_group == "low"),]
nrow(dat_lockdown_nonsocial_low) # total low paranoid individuals who completed nonsocial task: 99

dat_lockdown_nonsocial_high <- dat_lockdown_nonsocial[which(dat_lockdown_nonsocial$paranoia_group == "high"),]
nrow(dat_lockdown_nonsocial_high) # total high paranoid individuals who completed nonsocial task: 20

## social
dat_lockdown_social <- dat_lockdown[which(dat_lockdown$task_type == "social"),]  
dat_lockdown_social_low <- dat_lockdown_social[which(dat_lockdown_social$paranoia_group == "low"),]
nrow(dat_lockdown_social_low) # total low paranoid individuals who completed social task: 110

dat_lockdown_social_high <- dat_lockdown_social[which(dat_lockdown_social$paranoia_group == "high"),]
nrow(dat_lockdown_social_high) # total high paranoid individuals who completed social task: 20




### DEMOGRAPHICS
## Age
# nonsocial; low paranoid individuals
mean(dat_lockdown_nonsocial_low$demo_2, na.rm = TRUE) #38.6
sd(dat_lockdown_nonsocial_low$demo_2, na.rm = TRUE) #11.0
# nonsocial; high paranoid individuals
mean(dat_lockdown_nonsocial_high$demo_2, na.rm = TRUE) #38.0
sd(dat_lockdown_nonsocial_high$demo_2, na.rm = TRUE) #13.1

# Welch's t-test (age difference between two paranoia groups; equal variances assumed)
t.test(dat_lockdown_nonsocial$demo_2 ~ dat_lockdown_nonsocial$paranoia_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=T,
       paired=F)


# social; low paranoid individuals
mean(dat_lockdown_social_low$demo_2, na.rm = TRUE) #37.9
sd(dat_lockdown_social_low$demo_2, na.rm = TRUE) #10.8
# social; high paranoid individuals
mean(dat_lockdown_social_high$demo_2, na.rm = TRUE) #34.9
sd(dat_lockdown_social_high$demo_2, na.rm = TRUE) #9.2

# Welch's t-test (age difference between two paranoia groups; equal variances assumed)
t.test(dat_lockdown_social$demo_2 ~ dat_lockdown_social$paranoia_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=T,
       paired=F)

########################

## Gender (1- male,2- female)
# nonsocial; low paranoid individuals
nonsocial_low_male <- dat_lockdown_nonsocial_low[which(dat_lockdown_nonsocial_low$demo_1 == 1),]
nonsocial_low_female <- dat_lockdown_nonsocial_low[which(dat_lockdown_nonsocial_low$demo_1 == 2),]
nonsocial_low_other <- dat_lockdown_nonsocial_low[-which(dat_lockdown_nonsocial_low$demo_1 == 1 | dat_lockdown_nonsocial_low$demo_1 == 2),]

# nonsocial; high paranoid individuals
nonsocial_high_male <- dat_lockdown_nonsocial_high[which(dat_lockdown_nonsocial_high$demo_1 == 1),]
nonsocial_high_female <- dat_lockdown_nonsocial_high[which(dat_lockdown_nonsocial_high$demo_1 == 2),]
nonsocial_high_other <- dat_lockdown_nonsocial_high[-which(dat_lockdown_nonsocial_high$demo_1 == 1 | dat_lockdown_nonsocial_high$demo_1 == 2),]

nonsocial_low_male_perc <- round(nrow(nonsocial_low_male)/nrow(dat_lockdown_nonsocial_low)*100,1) 
nonsocial_low_female_perc <- round(nrow(nonsocial_low_female)/nrow(dat_lockdown_nonsocial_low)*100,1) 
nonsocial_low_other_perc <- round(nrow(nonsocial_low_other)/nrow(dat_lockdown_nonsocial_low)*100,1) 
nonsocial_high_male_perc  <- round(nrow(nonsocial_high_male)/nrow(dat_lockdown_nonsocial_high)*100,1) 
nonsocial_high_female_perc <- round(nrow(nonsocial_high_female)/nrow(dat_lockdown_nonsocial_high)*100,1) 
nonsocial_high_other_perc <- round(nrow(nonsocial_high_other)/nrow(dat_lockdown_nonsocial_high)*100,1) 

# percentages
nonsocial_low_male_perc # 64.6%
nonsocial_low_female_perc # 34.3%
nonsocial_low_other_perc # 1.0%
nonsocial_high_male_perc # 70.0%
nonsocial_high_female_perc # 25.0%
nonsocial_high_other_perc # 5.0%

# counts
nrow(nonsocial_low_male) # 64
nrow(nonsocial_low_female) # 34
nrow(nonsocial_low_other) # 1
nrow(nonsocial_high_male) # 14
nrow(nonsocial_high_female) # 5
nrow(nonsocial_high_other) # 1

tibble(Gender = c("Female", "Male", "Other"), 
       Low = c(nonsocial_low_female_perc, nonsocial_low_male_perc, nonsocial_low_other_perc),
       High = c(nonsocial_high_female_perc, nonsocial_high_male_perc, nonsocial_high_other_perc))

# Perform gender | chi-squared test
Table_Line_1 = c(nrow(nonsocial_low_female),nrow(nonsocial_high_female))
Table_Line_2 = c(nrow(nonsocial_low_male), nrow(nonsocial_high_male))
#Table_Line_3 = c(nrow(nonsocial_low_other), nrow(nonsocial_high_other))

Gender = rbind(Table_Line_1,Table_Line_2)
rownames(Gender) = c("Female","Male")
colnames(Gender) = c("low","high")

chisq.test(Gender,correct = FALSE)


# social; low paranoid individuals
social_low_male <- dat_lockdown_social_low[which(dat_lockdown_social_low$demo_1 == 1),]
social_low_female <- dat_lockdown_social_low[which(dat_lockdown_social_low$demo_1 == 2),]
social_low_other <- dat_lockdown_social_low[-which(dat_lockdown_social_low$demo_1 == 1 | dat_lockdown_social_low$demo_1 == 2),]

# social; high paranoid individuals
social_high_male <- dat_lockdown_social_high[which(dat_lockdown_social_high$demo_1 == 1),]
social_high_female <- dat_lockdown_social_high[which(dat_lockdown_social_high$demo_1 == 2),]
social_high_other <- dat_lockdown_social_high[-which(dat_lockdown_social_high$demo_1 == 1 | dat_lockdown_social_high$demo_1 == 2),]

social_low_male_perc <- round(nrow(social_low_male)/nrow(dat_lockdown_social_low)*100,1) 
social_low_female_perc <- round(nrow(social_low_female)/nrow(dat_lockdown_social_low)*100,1) 
social_low_other_perc <- round(nrow(social_low_other)/nrow(dat_lockdown_social_low)*100,1) 
social_high_male_perc  <- round(nrow(social_high_male)/nrow(dat_lockdown_social_high)*100,1) 
social_high_female_perc <- round(nrow(social_high_female)/nrow(dat_lockdown_social_high)*100,1) 
social_high_other_perc <- round(nrow(social_high_other)/nrow(dat_lockdown_social_high)*100,1) 

# percentages
social_low_male_perc # 48.8%
social_low_female_perc # 46.3%
social_low_other_perc # 4.9%
social_high_male_perc # 66.7%
social_high_female_perc # 33.3%
social_high_other_perc # 0.0%

# counts
nrow(social_low_male) # 40
nrow(social_low_female) # 38
nrow(social_low_other) # 4
nrow(social_high_male) # 20
nrow(social_high_female) # 10
nrow(social_high_other) # 0

tibble(Gender = c("Female", "Male", "Other"), 
       Low = c(social_low_female_perc, social_low_male_perc, social_low_other_perc),
       High = c(social_high_female_perc, social_high_male_perc, social_high_other_perc))

# Perform gender | chi-squared test
Table_Line_1 = c(nrow(social_low_female),nrow(social_high_female))
Table_Line_2 = c(nrow(social_low_male), nrow(social_high_male))
#Table_Line_3 = c(nrow(nonsocial_low_other), nrow(nonsocial_high_other))

Gender = rbind(Table_Line_1,Table_Line_2)
rownames(Gender) = c("Female","Male")
colnames(Gender) = c("low","high")

chisq.test(Gender,correct = FALSE)



########################

## Ethnicity (1- Not hispanic, 2- Hispanic)
# nonsocial; low paranoid individuals
nonsocial_low_noHisp <- dat_lockdown_nonsocial_low[which(dat_lockdown_nonsocial_low$demo_3 == 1),]
nonsocial_low_Hisp <- dat_lockdown_nonsocial_low[which(dat_lockdown_nonsocial_low$demo_3 == 2),]
nonsocial_low_noResp <- dat_lockdown_nonsocial_low[-which(dat_lockdown_nonsocial_low$demo_3 == 1 | dat_lockdown_nonsocial_low$demo_3 == 2),]

# nonsocial; high paranoid individuals
nonsocial_high_noHisp <- dat_lockdown_nonsocial_high[which(dat_lockdown_nonsocial_high$demo_3 == 1),]
nonsocial_high_Hisp <- dat_lockdown_nonsocial_high[which(dat_lockdown_nonsocial_high$demo_3 == 2),]
nonsocial_high_noResp <- dat_lockdown_nonsocial_high[-which(dat_lockdown_nonsocial_high$demo_3 == 1 | dat_lockdown_nonsocial_high$demo_3 == 2),]

nonsocial_low_noHisp_perc <- round(nrow(nonsocial_low_noHisp)/nrow(dat_lockdown_nonsocial_low)*100,1) 
nonsocial_low_Hisp_perc <- round(nrow(nonsocial_low_Hisp)/nrow(dat_lockdown_nonsocial_low)*100,1) 
nonsocial_low_noResp_perc <- round(nrow(nonsocial_low_noResp)/nrow(dat_lockdown_nonsocial_low)*100,1) 
nonsocial_high_noHisp_perc  <- round(nrow(nonsocial_high_noHisp)/nrow(dat_lockdown_nonsocial_high)*100,1) 
nonsocial_high_Hisp_perc <- round(nrow(nonsocial_high_Hisp)/nrow(dat_lockdown_nonsocial_high)*100,1) 
nonsocial_high_noResp_perc <- round(nrow(nonsocial_high_noResp)/nrow(dat_lockdown_nonsocial_high)*100,1) 

# percentages
nonsocial_low_noHisp_perc # 92.9%
nonsocial_low_Hisp_perc # 7.1%
nonsocial_low_noResp_perc # 0.0%
nonsocial_high_noHisp_perc # 80.0%
nonsocial_high_Hisp_perc # 20.0%
nonsocial_high_noResp_perc # 0.0%

# counts
nrow(nonsocial_low_noHisp) # 92
nrow(nonsocial_low_Hisp) # 7
nrow(nonsocial_low_noResp) # 0
nrow(nonsocial_high_noHisp) # 16
nrow(nonsocial_high_Hisp) # 4
nrow(nonsocial_high_noResp) # 0

tibble(Ethnicity = c("Hispanic", "Not Hispanic", "Not Specified"), 
       Low = c(nonsocial_low_Hisp_perc, nonsocial_low_noHisp_perc, nonsocial_low_noResp_perc),
       High = c(nonsocial_high_Hisp_perc, nonsocial_high_noHisp_perc, nonsocial_high_noResp_perc))

# Perform gender | chi-squared test
Table_Line_1 = c(nrow(nonsocial_low_Hisp),nrow(nonsocial_high_Hisp))
Table_Line_2 = c(nrow(nonsocial_low_noHisp), nrow(nonsocial_high_noHisp))
#Table_Line_3 = c(nrow(nonsocial_low_noResp), nrow(nonsocial_high_noResp))

Ethnicity = rbind(Table_Line_1,Table_Line_2)
rownames(Ethnicity) = c("Hispanic","Not Hispanic")
colnames(Ethnicity) = c("low","high")

chisq.test(Ethnicity,correct = FALSE)


# social; low paranoid individuals
social_low_noHisp <- dat_lockdown_social_low[which(dat_lockdown_social_low$demo_3 == 1),]
social_low_Hisp <- dat_lockdown_social_low[which(dat_lockdown_social_low$demo_3 == 2),]
social_low_noResp <- dat_lockdown_social_low[-which(dat_lockdown_social_low$demo_3 == 1 | dat_lockdown_social_low$demo_3 == 2),]

# social; high paranoid individuals
social_high_noHisp <- dat_lockdown_social_high[which(dat_lockdown_social_high$demo_3 == 1),]
social_high_Hisp <- dat_lockdown_social_high[which(dat_lockdown_social_high$demo_3 == 2),]
social_high_noResp <- dat_lockdown_social_high[-which(dat_lockdown_social_high$demo_3 == 1 | dat_lockdown_social_high$demo_3 == 2),]

social_low_noHisp_perc <- round(nrow(social_low_noHisp)/nrow(dat_lockdown_social_low)*100,1) 
social_low_Hisp_perc <- round(nrow(social_low_Hisp)/nrow(dat_lockdown_social_low)*100,1) 
social_low_noResp_perc <- round(nrow(social_low_noResp)/nrow(dat_lockdown_social_low)*100,1) 
social_high_noHisp_perc  <- round(nrow(social_high_noHisp)/nrow(dat_lockdown_social_high)*100,1) 
social_high_Hisp_perc <- round(nrow(social_high_Hisp)/nrow(dat_lockdown_social_high)*100,1) 
social_high_noResp_perc <- round(nrow(social_high_noResp)/nrow(dat_lockdown_social_high)*100,1) 

# percentages
social_low_noHisp_perc # 96.3%
social_low_Hisp_perc # 3.7%
social_low_noResp_perc # 0.0%
social_high_noHisp_perc # 76.7%
social_high_Hisp_perc # 23.3%
social_high_noResp_perc # 0.0%

# counts
nrow(social_low_noHisp) # 79
nrow(social_low_Hisp) # 3
nrow(social_low_noResp) # 0
nrow(social_high_noHisp) # 23
nrow(social_high_Hisp) # 7
nrow(social_high_noResp) # 0

tibble(Ethnicity = c("Hispanic", "Not Hispanic", "Not Specified"), 
       Low = c(social_low_Hisp_perc, social_low_noHisp_perc, social_low_noResp_perc),
       High = c(social_high_Hisp_perc, social_high_noHisp_perc, social_high_noResp_perc))

# Perform gender | chi-squared test
Table_Line_1 = c(nrow(social_low_Hisp),nrow(social_high_Hisp))
Table_Line_2 = c(nrow(social_low_noHisp), nrow(social_high_noHisp))
#Table_Line_3 = c(nrow(nonsocial_low_noResp), nrow(nonsocial_high_noResp))

Ethnicity = rbind(Table_Line_1,Table_Line_2)
rownames(Ethnicity) = c("Hispanic","Not Hispanic")
colnames(Ethnicity) = c("low","high")

chisq.test(Ethnicity,correct = FALSE)


####################################

## Race
# nonsocial; low paranoid individuals
nonsocial_low_white <- dat_lockdown_nonsocial_low[which(dat_lockdown_nonsocial_low$demo_4 == 1),]
nonsocial_low_black <- dat_lockdown_nonsocial_low[which(dat_lockdown_nonsocial_low$demo_4 == 2),]
nonsocial_low_asian <- dat_lockdown_nonsocial_low[which(dat_lockdown_nonsocial_low$demo_4 == 3),]
nonsocial_low_americanIndian <- dat_lockdown_nonsocial_low[which(dat_lockdown_nonsocial_low$demo_4 == 4),]
nonsocial_low_multi <- dat_lockdown_nonsocial_low[which(dat_lockdown_nonsocial_low$demo_4 == 5),]
nonsocial_low_other <- dat_lockdown_nonsocial_low[-which(dat_lockdown_nonsocial_low$demo_4 == 1 | 
                                                              dat_lockdown_nonsocial_low$demo_4 == 2 |
                                                              dat_lockdown_nonsocial_low$demo_4 == 3 | 
                                                              dat_lockdown_nonsocial_low$demo_4 == 4 |
                                                              dat_lockdown_nonsocial_low$demo_4 == 5),]

# nonsocial; high paranoid individuals
nonsocial_high_white <- dat_lockdown_nonsocial_high[which(dat_lockdown_nonsocial_high$demo_4 == 1),]
nonsocial_high_black <- dat_lockdown_nonsocial_high[which(dat_lockdown_nonsocial_high$demo_4 == 2),]
nonsocial_high_asian <- dat_lockdown_nonsocial_high[which(dat_lockdown_nonsocial_high$demo_4 == 3),]
nonsocial_high_americanIndian <- dat_lockdown_nonsocial_high[which(dat_lockdown_nonsocial_high$demo_4 == 4),]
nonsocial_high_multi <- dat_lockdown_nonsocial_high[which(dat_lockdown_nonsocial_high$demo_4 == 5),]
nonsocial_high_other <- dat_lockdown_nonsocial_high[-which(dat_lockdown_nonsocial_high$demo_4 == 1 | 
                                                                dat_lockdown_nonsocial_high$demo_4 == 2 |
                                                                dat_lockdown_nonsocial_high$demo_4 == 3 | 
                                                                dat_lockdown_nonsocial_high$demo_4 == 4 |
                                                                dat_lockdown_nonsocial_high$demo_4 == 5),]

nonsocial_low_white_perc <- round(nrow(nonsocial_low_white)/nrow(dat_lockdown_nonsocial_low)*100,1) 
nonsocial_low_black_perc <- round(nrow(nonsocial_low_black)/nrow(dat_lockdown_nonsocial_low)*100,1) 
nonsocial_low_asian_perc <- round(nrow(nonsocial_low_asian)/nrow(dat_lockdown_nonsocial_low)*100,1)
nonsocial_low_americanIndian_perc <- round(nrow(nonsocial_low_americanIndian)/nrow(dat_lockdown_nonsocial_low)*100,1) 
nonsocial_low_multi_perc <- round(nrow(nonsocial_low_multi)/nrow(dat_lockdown_nonsocial_low)*100,1) 
nonsocial_low_other_perc <- round(nrow(nonsocial_low_other)/nrow(dat_lockdown_nonsocial_low)*100,1) 
nonsocial_high_white_perc <- round(nrow(nonsocial_high_white)/nrow(dat_lockdown_nonsocial_high)*100,1) 
nonsocial_high_black_perc <- round(nrow(nonsocial_high_black)/nrow(dat_lockdown_nonsocial_high)*100,1) 
nonsocial_high_asian_perc <- round(nrow(nonsocial_high_asian)/nrow(dat_lockdown_nonsocial_high)*100,1)
nonsocial_high_americanIndian_perc <- round(nrow(nonsocial_high_americanIndian)/nrow(dat_lockdown_nonsocial_high)*100,1) 
nonsocial_high_multi_perc <- round(nrow(nonsocial_high_multi)/nrow(dat_lockdown_nonsocial_high)*100,1) 
nonsocial_high_other_perc <- round(nrow(nonsocial_high_other)/nrow(dat_lockdown_nonsocial_high)*100,1) 

# Percentages
nonsocial_low_white_perc # 82.8%
nonsocial_low_black_perc # 6.1%
nonsocial_low_asian_perc # 4.0%
nonsocial_low_americanIndian_perc # 0.0%
nonsocial_low_multi_perc # 3.0%
nonsocial_low_other_perc # 4.0%
nonsocial_high_white_perc # 85.0%
nonsocial_high_black_perc # 10.0%
nonsocial_high_asian_perc # 0.0%
nonsocial_high_americanIndian_perc # 5.0%
nonsocial_high_multi_perc # 0.0%
nonsocial_high_other_perc # 0.0%

# Counts
nrow(nonsocial_low_white) # 82
nrow(nonsocial_low_black) # 6
nrow(nonsocial_low_asian) # 4
nrow(nonsocial_low_americanIndian) # 0
nrow(nonsocial_low_multi) # 3
nrow(nonsocial_low_other) # 4
nrow(nonsocial_high_white) # 17
nrow(nonsocial_high_black) # 2
nrow(nonsocial_high_asian) # 0
nrow(nonsocial_high_americanIndian) # 1
nrow(nonsocial_high_multi) # 0
nrow(nonsocial_high_other) # 0




tibble(Race = c("White", "Black or African American","Asian","American Indian or Alaska Native","Multiracial","Other"), 
       Low = c(nonsocial_low_white_perc, nonsocial_low_black_perc, nonsocial_low_asian_perc,
               nonsocial_low_americanIndian_perc, nonsocial_low_multi_perc, nonsocial_low_other_perc),
       High = c(nonsocial_high_white_perc, nonsocial_high_black_perc, nonsocial_high_asian_perc, 
                nonsocial_high_americanIndian_perc, nonsocial_high_multi_perc, nonsocial_high_other_perc))


# Perform race | chi-squared test
Table_Line_1 = c(nrow(nonsocial_low_white),nrow(nonsocial_high_white))
Table_Line_2 = c(nrow(nonsocial_low_black),nrow(nonsocial_high_black))
Table_Line_3 = c(nrow(nonsocial_low_asian),nrow(nonsocial_high_asian))
Table_Line_4 = c(nrow(nonsocial_low_americanIndian),nrow(nonsocial_high_americanIndian))
Table_Line_5 = c(nrow(nonsocial_low_multi),nrow(nonsocial_high_multi))
Table_Line_6 = c(nrow(nonsocial_low_other),nrow(nonsocial_high_other))


Race = rbind(Table_Line_1,Table_Line_2,Table_Line_3,Table_Line_4,Table_Line_5, Table_Line_6)
rownames(Race) = c("White","Black","Asian","American Indian","Multiracial","Other")
colnames(Race) = c("low","high")

chisq.test(Race, correct = FALSE)


# social; low paranoid individuals
social_low_white <- dat_lockdown_social_low[which(dat_lockdown_social_low$demo_4 == 1),]
social_low_black <- dat_lockdown_social_low[which(dat_lockdown_social_low$demo_4 == 2),]
social_low_asian <- dat_lockdown_social_low[which(dat_lockdown_social_low$demo_4 == 3),]
social_low_americanIndian <- dat_lockdown_social_low[which(dat_lockdown_social_low$demo_4 == 4),]
social_low_multi <- dat_lockdown_social_low[which(dat_lockdown_social_low$demo_4 == 5),]
social_low_other <- dat_lockdown_social_low[-which(dat_lockdown_social_low$demo_4 == 1 | 
                                                        dat_lockdown_social_low$demo_4 == 2 |
                                                        dat_lockdown_social_low$demo_4 == 3 | 
                                                        dat_lockdown_social_low$demo_4 == 4 |
                                                        dat_lockdown_social_low$demo_4 == 5),]

# nonsocial; high paranoid individuals
social_high_white <- dat_lockdown_social_high[which(dat_lockdown_social_high$demo_4 == 1),]
social_high_black <- dat_lockdown_social_high[which(dat_lockdown_social_high$demo_4 == 2),]
social_high_asian <- dat_lockdown_social_high[which(dat_lockdown_social_high$demo_4 == 3),]
social_high_americanIndian <- dat_lockdown_social_high[which(dat_lockdown_social_high$demo_4 == 4),]
social_high_multi <- dat_lockdown_social_high[which(dat_lockdown_social_high$demo_4 == 5),]
social_high_other <- dat_lockdown_social_high[-which(dat_lockdown_social_high$demo_4 == 1 | 
                                                          dat_lockdown_social_high$demo_4 == 2 |
                                                          dat_lockdown_social_high$demo_4 == 3 | 
                                                          dat_lockdown_social_high$demo_4 == 4 |
                                                          dat_lockdown_social_high$demo_4 == 5),]

social_low_white_perc <- round(nrow(social_low_white)/nrow(dat_lockdown_social_low)*100,1) 
social_low_black_perc <- round(nrow(social_low_black)/nrow(dat_lockdown_social_low)*100,1) 
social_low_asian_perc <- round(nrow(social_low_asian)/nrow(dat_lockdown_social_low)*100,1)
social_low_americanIndian_perc <- round(nrow(social_low_americanIndian)/nrow(dat_lockdown_social_low)*100,1) 
social_low_multi_perc <- round(nrow(social_low_multi)/nrow(dat_lockdown_social_low)*100,1) 
social_low_other_perc <- round(nrow(social_low_other)/nrow(dat_lockdown_social_low)*100,1) 
social_high_white_perc <- round(nrow(social_high_white)/nrow(dat_lockdown_social_high)*100,1) 
social_high_black_perc <- round(nrow(social_high_black)/nrow(dat_lockdown_social_high)*100,1) 
social_high_asian_perc <- round(nrow(social_high_asian)/nrow(dat_lockdown_social_high)*100,1)
social_high_americanIndian_perc <- round(nrow(social_high_americanIndian)/nrow(dat_lockdown_social_high)*100,1) 
social_high_multi_perc <- round(nrow(social_high_multi)/nrow(dat_lockdown_social_high)*100,1) 
social_high_other_perc <- round(nrow(social_high_other)/nrow(dat_lockdown_social_high)*100,1) 

# Percentages
social_low_white_perc # 81.7%
social_low_black_perc # 9.8%
social_low_asian_perc # 6.1%
social_low_americanIndian_perc # 0.0%
social_low_multi_perc # 1.2%
social_low_other_perc # 1.2%
social_high_white_perc # 66.7%
social_high_black_perc # 30.0%
social_high_asian_perc # 0.0%
social_high_americanIndian_perc # 0.0%
social_high_multi_perc # 0.0%
social_high_other_perc # 3.3%

# Counts
nrow(social_low_white) # 67
nrow(social_low_black) # 8
nrow(social_low_asian) # 5
nrow(social_low_americanIndian) # 0
nrow(social_low_multi) # 1
nrow(social_low_other) # 1
nrow(social_high_white) # 20
nrow(social_high_black) # 9
nrow(social_high_asian) # 0
nrow(social_high_americanIndian) # 0
nrow(social_high_multi) # 0
nrow(social_high_other) # 1




tibble(Race = c("White", "Black or African American","Asian","American Indian or Alaska Native","Multiracial","Other"), 
       Low = c(social_low_white_perc, social_low_black_perc, social_low_asian_perc,
               social_low_americanIndian_perc, social_low_multi_perc, social_low_other_perc),
       High = c(social_high_white_perc, social_high_black_perc, social_high_asian_perc, 
                social_high_americanIndian_perc, social_high_multi_perc, social_high_other_perc))


# Perform race | chi-squared test
Table_Line_1 = c(nrow(social_low_white),nrow(social_high_white))
Table_Line_2 = c(nrow(social_low_black),nrow(social_high_black))
Table_Line_3 = c(nrow(social_low_asian),nrow(social_high_asian))
#Table_Line_4 = c(nrow(social_low_americanIndian),nrow(social_high_americanIndian))
Table_Line_5 = c(nrow(social_low_multi),nrow(social_high_multi))
Table_Line_6 = c(nrow(social_low_other),nrow(social_high_other))


Race = rbind(Table_Line_1,Table_Line_2,Table_Line_3,Table_Line_5,Table_Line_6) # Removed Line_4 because 0 cell value
rownames(Race) = c("White","Black","Asian","Multiracial","Other")
colnames(Race) = c("low","high")

chisq.test(Race, correct = FALSE)
