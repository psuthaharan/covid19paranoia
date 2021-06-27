### Analysis: Summarize subject characteristics in the replication dataset
###
### Description: Perform descriptive statistics on subject characteristics 
###
###
### Table: Supplementary Table 5 
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

### subset data; we want the data used in our replication analysis
dat_replication <- dat %>%
                   dplyr::filter(dat$dataset == "replication")

dat_replication$paranoia_group <- ifelse(rowSums(dat_replication[, grepl("rgpts_per", names(dat_replication))], na.rm = TRUE) > 10, "high","low")
#social_group <- as.data.frame(social_dat$paranoia_group)
#colnames(social_group) <- "group"


### number of low and high paranoia individuals between nonsocial and social tasks
## nonsocial
dat_replication_nonsocial <- dat_replication[which(dat_replication$task_type == "nonsocial"),]  
dat_replication_nonsocial_low <- dat_replication_nonsocial[which(dat_replication_nonsocial$paranoia_group == "low"),]
nrow(dat_replication_nonsocial_low) # total low paranoid individuals who completed nonsocial task: 81

dat_replication_nonsocial_high <- dat_replication_nonsocial[which(dat_replication_nonsocial$paranoia_group == "high"),]
nrow(dat_replication_nonsocial_high) # total high paranoid individuals who completed nonsocial task: 18

## social
dat_replication_social <- dat_replication[which(dat_replication$task_type == "social"),]  
dat_replication_social_low <- dat_replication_social[which(dat_replication_social$paranoia_group == "low"),]
nrow(dat_replication_social_low) # total low paranoid individuals who completed social task: 233

dat_replication_social_high <- dat_replication_social[which(dat_replication_social$paranoia_group == "high"),]
nrow(dat_replication_social_high) # total high paranoid individuals who completed social task: 73




### DEMOGRAPHICS
## Age
# nonsocial; low paranoid individuals
mean(dat_replication_nonsocial_low$demo_2, na.rm = TRUE) #36.6
sd(dat_replication_nonsocial_low$demo_2, na.rm = TRUE) #9.6
# nonsocial; high paranoid individuals
mean(dat_replication_nonsocial_high$demo_2, na.rm = TRUE) #36.1
sd(dat_replication_nonsocial_high$demo_2, na.rm = TRUE) #8.8

# Welch's t-test (age difference between two paranoia groups; equal variances assumed)
t.test(dat_replication_nonsocial$demo_2 ~ dat_replication_nonsocial$paranoia_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=T,
       paired=F)


# social; low paranoid individuals
mean(dat_replication_social_low$demo_2, na.rm = TRUE) #37.9
sd(dat_replication_social_low$demo_2, na.rm = TRUE) #10.9
# social; high paranoid individuals
mean(dat_replication_social_high$demo_2, na.rm = TRUE) #32.9
sd(dat_replication_social_high$demo_2, na.rm = TRUE) #9.4

# Welch's t-test (age difference between two paranoia groups; equal variances assumed)
t.test(dat_replication_social$demo_2 ~ dat_replication_social$paranoia_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=T,
       paired=F)

########################

## Gender (1- male,2- female)
# nonsocial; low paranoid individuals
nonsocial_low_male <- dat_replication_nonsocial_low[which(dat_replication_nonsocial_low$demo_1 == 1),]
nonsocial_low_female <- dat_replication_nonsocial_low[which(dat_replication_nonsocial_low$demo_1 == 2),]
nonsocial_low_other <- dat_replication_nonsocial_low[-which(dat_replication_nonsocial_low$demo_1 == 1 | dat_replication_nonsocial_low$demo_1 == 2),]

# nonsocial; high paranoid individuals
nonsocial_high_male <- dat_replication_nonsocial_high[which(dat_replication_nonsocial_high$demo_1 == 1),]
nonsocial_high_female <- dat_replication_nonsocial_high[which(dat_replication_nonsocial_high$demo_1 == 2),]
nonsocial_high_other <- dat_replication_nonsocial_high[-which(dat_replication_nonsocial_high$demo_1 == 1 | dat_replication_nonsocial_high$demo_1 == 2),]

nonsocial_low_male_perc <- round(nrow(nonsocial_low_male)/nrow(dat_replication_nonsocial_low)*100,1) 
nonsocial_low_female_perc <- round(nrow(nonsocial_low_female)/nrow(dat_replication_nonsocial_low)*100,1) 
nonsocial_low_other_perc <- round(nrow(nonsocial_low_other)/nrow(dat_replication_nonsocial_low)*100,1) 
nonsocial_high_male_perc  <- round(nrow(nonsocial_high_male)/nrow(dat_replication_nonsocial_high)*100,1) 
nonsocial_high_female_perc <- round(nrow(nonsocial_high_female)/nrow(dat_replication_nonsocial_high)*100,1) 
nonsocial_high_other_perc <- round(nrow(nonsocial_high_other)/nrow(dat_replication_nonsocial_high)*100,1) 

# percentages
nonsocial_low_male_perc # 58%
nonsocial_low_female_perc # 40.7%
nonsocial_low_other_perc # 1.2%
nonsocial_high_male_perc # 66.7%
nonsocial_high_female_perc # 33.3%
nonsocial_high_other_perc # 0%

# counts
nrow(nonsocial_low_male) # 47
nrow(nonsocial_low_female) # 33
nrow(nonsocial_low_other) # 1
nrow(nonsocial_high_male) # 12
nrow(nonsocial_high_female) # 6
nrow(nonsocial_high_other) # 0

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
social_low_male <- dat_replication_social_low[which(dat_replication_social_low$demo_1 == 1),]
social_low_female <- dat_replication_social_low[which(dat_replication_social_low$demo_1 == 2),]
social_low_other <- dat_replication_social_low[-which(dat_replication_social_low$demo_1 == 1 | dat_replication_social_low$demo_1 == 2),]

# social; high paranoid individuals
social_high_male <- dat_replication_social_high[which(dat_replication_social_high$demo_1 == 1),]
social_high_female <- dat_replication_social_high[which(dat_replication_social_high$demo_1 == 2),]
social_high_other <- dat_replication_social_high[-which(dat_replication_social_high$demo_1 == 1 | dat_replication_social_high$demo_1 == 2),]

social_low_male_perc <- round(nrow(social_low_male)/nrow(dat_replication_social_low)*100,1) 
social_low_female_perc <- round(nrow(social_low_female)/nrow(dat_replication_social_low)*100,1) 
social_low_other_perc <- round(nrow(social_low_other)/nrow(dat_replication_social_low)*100,1) 
social_high_male_perc  <- round(nrow(social_high_male)/nrow(dat_replication_social_high)*100,1) 
social_high_female_perc <- round(nrow(social_high_female)/nrow(dat_replication_social_high)*100,1) 
social_high_other_perc <- round(nrow(social_high_other)/nrow(dat_replication_social_high)*100,1) 

# percentages
social_low_male_perc # 60.1%
social_low_female_perc # 38.6%
social_low_other_perc # 1.3%
social_high_male_perc # 61.6%
social_high_female_perc # 38.4%
social_high_other_perc # 0%

# counts
nrow(social_low_male) # 140
nrow(social_low_female) # 90
nrow(social_low_other) # 3
nrow(social_high_male) # 45
nrow(social_high_female) # 28
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
nonsocial_low_noHisp <- dat_replication_nonsocial_low[which(dat_replication_nonsocial_low$demo_3 == 1),]
nonsocial_low_Hisp <- dat_replication_nonsocial_low[which(dat_replication_nonsocial_low$demo_3 == 2),]
nonsocial_low_noResp <- dat_replication_nonsocial_low[-which(dat_replication_nonsocial_low$demo_3 == 1 | dat_replication_nonsocial_low$demo_3 == 2),]

# nonsocial; high paranoid individuals
nonsocial_high_noHisp <- dat_replication_nonsocial_high[which(dat_replication_nonsocial_high$demo_3 == 1),]
nonsocial_high_Hisp <- dat_replication_nonsocial_high[which(dat_replication_nonsocial_high$demo_3 == 2),]
nonsocial_high_noResp <- dat_replication_nonsocial_high[-which(dat_replication_nonsocial_high$demo_3 == 1 | dat_replication_nonsocial_high$demo_3 == 2),]

nonsocial_low_noHisp_perc <- round(nrow(nonsocial_low_noHisp)/nrow(dat_replication_nonsocial_low)*100,1) 
nonsocial_low_Hisp_perc <- round(nrow(nonsocial_low_Hisp)/nrow(dat_replication_nonsocial_low)*100,1) 
nonsocial_low_noResp_perc <- round(nrow(nonsocial_low_noResp)/nrow(dat_replication_nonsocial_low)*100,1) 
nonsocial_high_noHisp_perc  <- round(nrow(nonsocial_high_noHisp)/nrow(dat_replication_nonsocial_high)*100,1) 
nonsocial_high_Hisp_perc <- round(nrow(nonsocial_high_Hisp)/nrow(dat_replication_nonsocial_high)*100,1) 
nonsocial_high_noResp_perc <- round(nrow(nonsocial_high_noResp)/nrow(dat_replication_nonsocial_high)*100,1) 

# percentages
nonsocial_low_noHisp_perc # 93.8%
nonsocial_low_Hisp_perc # 6.2%
nonsocial_low_noResp_perc # 0%
nonsocial_high_noHisp_perc # 61.1%
nonsocial_high_Hisp_perc # 38.9%
nonsocial_high_noResp_perc # 0%

# counts
nrow(nonsocial_low_noHisp) # 76
nrow(nonsocial_low_Hisp) # 5
nrow(nonsocial_low_noResp) # 0
nrow(nonsocial_high_noHisp) # 11
nrow(nonsocial_high_Hisp) # 7
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
social_low_noHisp <- dat_replication_social_low[which(dat_replication_social_low$demo_3 == 1),]
social_low_Hisp <- dat_replication_social_low[which(dat_replication_social_low$demo_3 == 2),]
social_low_noResp <- dat_replication_social_low[-which(dat_replication_social_low$demo_3 == 1 | dat_replication_social_low$demo_3 == 2),]

# social; high paranoid individuals
social_high_noHisp <- dat_replication_social_high[which(dat_replication_social_high$demo_3 == 1),]
social_high_Hisp <- dat_replication_social_high[which(dat_replication_social_high$demo_3 == 2),]
social_high_noResp <- dat_replication_social_high[-which(dat_replication_social_high$demo_3 == 1 | dat_replication_social_high$demo_3 == 2),]

social_low_noHisp_perc <- round(nrow(social_low_noHisp)/nrow(dat_replication_social_low)*100,1) 
social_low_Hisp_perc <- round(nrow(social_low_Hisp)/nrow(dat_replication_social_low)*100,1) 
social_low_noResp_perc <- round(nrow(social_low_noResp)/nrow(dat_replication_social_low)*100,1) 
social_high_noHisp_perc  <- round(nrow(social_high_noHisp)/nrow(dat_replication_social_high)*100,1) 
social_high_Hisp_perc <- round(nrow(social_high_Hisp)/nrow(dat_replication_social_high)*100,1) 
social_high_noResp_perc <- round(nrow(social_high_noResp)/nrow(dat_replication_social_high)*100,1) 

# percentages
social_low_noHisp_perc # 93.6%
social_low_Hisp_perc # 6.0%
social_low_noResp_perc # 0.4%
social_high_noHisp_perc # 72.6%
social_high_Hisp_perc # 23.3%
social_high_noResp_perc # 4.1%

# counts
nrow(social_low_noHisp) # 218
nrow(social_low_Hisp) # 14
nrow(social_low_noResp) # 1
nrow(social_high_noHisp) # 53
nrow(social_high_Hisp) # 17
nrow(social_high_noResp) # 3

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
nonsocial_low_white <- dat_replication_nonsocial_low[which(dat_replication_nonsocial_low$demo_4 == 1),]
nonsocial_low_black <- dat_replication_nonsocial_low[which(dat_replication_nonsocial_low$demo_4 == 2),]
nonsocial_low_asian <- dat_replication_nonsocial_low[which(dat_replication_nonsocial_low$demo_4 == 3),]
nonsocial_low_americanIndian <- dat_replication_nonsocial_low[which(dat_replication_nonsocial_low$demo_4 == 4),]
nonsocial_low_multi <- dat_replication_nonsocial_low[which(dat_replication_nonsocial_low$demo_4 == 5),]
nonsocial_low_other <- dat_replication_nonsocial_low[-which(dat_replication_nonsocial_low$demo_4 == 1 | 
                                                            dat_replication_nonsocial_low$demo_4 == 2 |
                                                            dat_replication_nonsocial_low$demo_4 == 3 | 
                                                            dat_replication_nonsocial_low$demo_4 == 4 |
                                                            dat_replication_nonsocial_low$demo_4 == 5),]

# nonsocial; high paranoid individuals
nonsocial_high_white <- dat_replication_nonsocial_high[which(dat_replication_nonsocial_high$demo_4 == 1),]
nonsocial_high_black <- dat_replication_nonsocial_high[which(dat_replication_nonsocial_high$demo_4 == 2),]
nonsocial_high_asian <- dat_replication_nonsocial_high[which(dat_replication_nonsocial_high$demo_4 == 3),]
nonsocial_high_americanIndian <- dat_replication_nonsocial_high[which(dat_replication_nonsocial_high$demo_4 == 4),]
nonsocial_high_multi <- dat_replication_nonsocial_high[which(dat_replication_nonsocial_high$demo_4 == 5),]
nonsocial_high_other <- dat_replication_nonsocial_high[-which(dat_replication_nonsocial_high$demo_4 == 1 | 
                                                              dat_replication_nonsocial_high$demo_4 == 2 |
                                                              dat_replication_nonsocial_high$demo_4 == 3 | 
                                                              dat_replication_nonsocial_high$demo_4 == 4 |
                                                              dat_replication_nonsocial_high$demo_4 == 5),]

nonsocial_low_white_perc <- round(nrow(nonsocial_low_white)/nrow(dat_replication_nonsocial_low)*100,1) 
nonsocial_low_black_perc <- round(nrow(nonsocial_low_black)/nrow(dat_replication_nonsocial_low)*100,1) 
nonsocial_low_asian_perc <- round(nrow(nonsocial_low_asian)/nrow(dat_replication_nonsocial_low)*100,1)
nonsocial_low_americanIndian_perc <- round(nrow(nonsocial_low_americanIndian)/nrow(dat_replication_nonsocial_low)*100,1) 
nonsocial_low_multi_perc <- round(nrow(nonsocial_low_multi)/nrow(dat_replication_nonsocial_low)*100,1) 
nonsocial_low_other_perc <- round(nrow(nonsocial_low_other)/nrow(dat_replication_nonsocial_low)*100,1) 
nonsocial_high_white_perc <- round(nrow(nonsocial_high_white)/nrow(dat_replication_nonsocial_high)*100,1) 
nonsocial_high_black_perc <- round(nrow(nonsocial_high_black)/nrow(dat_replication_nonsocial_high)*100,1) 
nonsocial_high_asian_perc <- round(nrow(nonsocial_high_asian)/nrow(dat_replication_nonsocial_high)*100,1)
nonsocial_high_americanIndian_perc <- round(nrow(nonsocial_high_americanIndian)/nrow(dat_replication_nonsocial_high)*100,1) 
nonsocial_high_multi_perc <- round(nrow(nonsocial_high_multi)/nrow(dat_replication_nonsocial_high)*100,1) 
nonsocial_high_other_perc <- round(nrow(nonsocial_high_other)/nrow(dat_replication_nonsocial_high)*100,1) 

# Percentages
nonsocial_low_white_perc # 77.8%
nonsocial_low_black_perc # 9.9%
nonsocial_low_asian_perc # 7.4%
nonsocial_low_americanIndian_perc # 1.2%
nonsocial_low_multi_perc # 3.7%
nonsocial_low_other_perc # 0%
nonsocial_high_white_perc # 77.8%
nonsocial_high_black_perc # 16.7%
nonsocial_high_asian_perc # 5.6%
nonsocial_high_americanIndian_perc # 0%
nonsocial_high_multi_perc # 0%
nonsocial_high_other_perc # 0%

# Counts
nrow(nonsocial_low_white) # 63
nrow(nonsocial_low_black) # 8
nrow(nonsocial_low_asian) # 6
nrow(nonsocial_low_americanIndian) # 1
nrow(nonsocial_low_multi) # 3
nrow(nonsocial_low_other) # 0
nrow(nonsocial_high_white) # 14
nrow(nonsocial_high_black) # 3
nrow(nonsocial_high_asian) # 1
nrow(nonsocial_high_americanIndian) # 0
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
#Table_Line_6 = c(nrow(nonsocial_low_other),nrow(nonsocial_high_other))


Race = rbind(Table_Line_1,Table_Line_2,Table_Line_3,Table_Line_4,Table_Line_5)
rownames(Race) = c("White","Black","Asian","American Indian","Multiracial","Other")
colnames(Race) = c("low","high")

chisq.test(Race, correct = FALSE)


# social; low paranoid individuals
social_low_white <- dat_replication_social_low[which(dat_replication_social_low$demo_4 == 1),]
social_low_black <- dat_replication_social_low[which(dat_replication_social_low$demo_4 == 2),]
social_low_asian <- dat_replication_social_low[which(dat_replication_social_low$demo_4 == 3),]
social_low_americanIndian <- dat_replication_social_low[which(dat_replication_social_low$demo_4 == 4),]
social_low_multi <- dat_replication_social_low[which(dat_replication_social_low$demo_4 == 5),]
social_low_other <- dat_replication_social_low[-which(dat_replication_social_low$demo_4 == 1 | 
                                                              dat_replication_social_low$demo_4 == 2 |
                                                              dat_replication_social_low$demo_4 == 3 | 
                                                              dat_replication_social_low$demo_4 == 4 |
                                                              dat_replication_social_low$demo_4 == 5),]

# nonsocial; high paranoid individuals
social_high_white <- dat_replication_social_high[which(dat_replication_social_high$demo_4 == 1),]
social_high_black <- dat_replication_social_high[which(dat_replication_social_high$demo_4 == 2),]
social_high_asian <- dat_replication_social_high[which(dat_replication_social_high$demo_4 == 3),]
social_high_americanIndian <- dat_replication_social_high[which(dat_replication_social_high$demo_4 == 4),]
social_high_multi <- dat_replication_social_high[which(dat_replication_social_high$demo_4 == 5),]
social_high_other <- dat_replication_social_high[-which(dat_replication_social_high$demo_4 == 1 | 
                                                                dat_replication_social_high$demo_4 == 2 |
                                                                dat_replication_social_high$demo_4 == 3 | 
                                                                dat_replication_social_high$demo_4 == 4 |
                                                                dat_replication_social_high$demo_4 == 5),]

social_low_white_perc <- round(nrow(social_low_white)/nrow(dat_replication_social_low)*100,1) 
social_low_black_perc <- round(nrow(social_low_black)/nrow(dat_replication_social_low)*100,1) 
social_low_asian_perc <- round(nrow(social_low_asian)/nrow(dat_replication_social_low)*100,1)
social_low_americanIndian_perc <- round(nrow(social_low_americanIndian)/nrow(dat_replication_social_low)*100,1) 
social_low_multi_perc <- round(nrow(social_low_multi)/nrow(dat_replication_social_low)*100,1) 
social_low_other_perc <- round(nrow(social_low_other)/nrow(dat_replication_social_low)*100,1) 
social_high_white_perc <- round(nrow(social_high_white)/nrow(dat_replication_social_high)*100,1) 
social_high_black_perc <- round(nrow(social_high_black)/nrow(dat_replication_social_high)*100,1) 
social_high_asian_perc <- round(nrow(social_high_asian)/nrow(dat_replication_social_high)*100,1)
social_high_americanIndian_perc <- round(nrow(social_high_americanIndian)/nrow(dat_replication_social_high)*100,1) 
social_high_multi_perc <- round(nrow(social_high_multi)/nrow(dat_replication_social_high)*100,1) 
social_high_other_perc <- round(nrow(social_high_other)/nrow(dat_replication_social_high)*100,1) 

# Percentages
social_low_white_perc # 78.5%
social_low_black_perc # 8.6%
social_low_asian_perc # 6.0%
social_low_americanIndian_perc # 0.4%
social_low_multi_perc # 4.3%
social_low_other_perc # 2.1%
social_high_white_perc # 63%
social_high_black_perc # 23.3%
social_high_asian_perc # 5.5%
social_high_americanIndian_perc # 0%
social_high_multi_perc # 6.8%
social_high_other_perc # 1.4%

# Counts
nrow(social_low_white) # 183
nrow(social_low_black) # 20
nrow(social_low_asian) # 14
nrow(social_low_americanIndian) # 1
nrow(social_low_multi) # 10
nrow(social_low_other) # 5
nrow(social_high_white) # 46
nrow(social_high_black) # 17
nrow(social_high_asian) # 4
nrow(social_high_americanIndian) # 0
nrow(social_high_multi) # 5
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
Table_Line_4 = c(nrow(social_low_americanIndian),nrow(social_high_americanIndian))
Table_Line_5 = c(nrow(social_low_multi),nrow(social_high_multi))
Table_Line_6 = c(nrow(social_low_other),nrow(social_high_other))


Race = rbind(Table_Line_1,Table_Line_2,Table_Line_3,Table_Line_4,Table_Line_5,Table_Line_6)
rownames(Race) = c("White","Black","Asian","American Indian","Multiracial","Other")
colnames(Race) = c("low","high")

chisq.test(Race, correct = FALSE)

