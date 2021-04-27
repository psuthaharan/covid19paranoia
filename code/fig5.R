### Analysis: Predicting paranoia from state features
###
### Description: Perform multiple regression analysis with 
###              backward stepwise regression to predict paranoia
###              paranoia from state features
###
### Figure: 5
### Written by: Praveen Suthaharan


#Empties Global Environment cache
rm(list = ls())

#Set working directory to current file location
setwd("C:/Pandemic_2020/revisions/data")

#Importing packages
library(tidyverse)
#install.packages("data.table")
library(data.table)
library(ggpubr)
library(plotrix)
library(dplyr)
library(openintro)
library(sjPlot)


#Read in data
ctl.mask <- read.csv("Figure5_CTL.csv", 
                     stringsAsFactors = FALSE)

panel <- read.csv("Figure5_regression.csv", stringsAsFactors = FALSE)

dynata <- read.csv('dynata.csv')

cases_deaths <- read.csv("Figure5_us_cases_deaths.csv")
cases_deaths$state <- state2abbr(cases_deaths$state)

# dynata
dynata.df <- data.frame(state = dynata$state,
                        mask.response = dynata$mask,
                        value = dynata$respondents)

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


# df.ctlmask <- left_join(ctl.mask,dynata.fiveAlways)
# 
# df.ctlmask.stats <- df.ctlmask %>%
#   group_by(group) %>%
#   summarise(mean.maskpercep = mean(rel.freq,na.rm = TRUE),
#             sem.maskpercep = std.error(rel.freq, na.rm = TRUE))



# ggplot(data = df.ctlmask.stats, aes(x=factor(group), y=mean.maskpercep)) + 
#   geom_bar(stat = "identity") +
#   geom_errorbar(aes(ymin = mean.maskpercep - sem.maskpercep, ymax = mean.maskpercep + sem.maskpercep), 
#                 width = 0.2, size=2) +
#   geom_text(aes(label = signif(mean.maskpercep, digits = 3)),color="white", size=8, nudge_y = -0.2) +
#   scale_x_discrete(labels = c("tight-mandate",
#                               "tight-recommended",
#                               "loose-mandate",
#                               "loose-recommended")) + labs(x = "CLT-mask",y="mask-wearing belief")

panel$period2 <- factor(panel$period2, levels = c("pre","post"))
# panel$mwr <- factor(panel$mwr, levels = c("req","rec"))
# 
# 
# ggplot(panel, aes(ctl, paranoia)) + geom_smooth(method = "lm") + geom_point() + stat_cor() +
#   facet_grid(mwr ~ period2)


df <- left_join(panel,dynata.fiveAlways)

df.post <- df[which(df$period2 == "post"),]

dat <- read.csv("pandemicPRL.csv")
pandemic_post <- dat[which(dat$dataset == "pandemic" & dat$period == "postlockdown"),]
pandemic_post$paranoia_score <- rowMeans(pandemic_post[, grepl("rgpts_", names(pandemic_post))], na.rm = TRUE)
df.post$paranoia_score <- pandemic_post$paranoia_score
df.post <- left_join(df.post,cases_deaths)



# ggplot(df.post, aes(clt, rel.freq)) + geom_smooth(method = "lm") + geom_point() + stat_cor() +
#   facet_grid(mwr ~ period2) + geom_text(label=rownames(df.post$state))




# t.test(df.post$rel.freq ~ df.post$mwr,
#        mu=0,
#        alt="two.sided",
#        conf=0.95,
#        var.eq=F,
#        paired=F)
# 
# t.test(df.post$clt ~ df.post$mwr,
#        mu=0,
#        alt="two.sided",
#        conf=0.95,
#        var.eq=F,
#        paired=F)


regression <- data.frame(id=1:nrow(df.post),
                         date = df.post$date,
                         state = df.post$state,
                         paranoia = df.post$paranoia_score,
                         wsr = df.post$wsr,
                         #mu3 = df.post$mu3,
                         #anxiety = df.post$BAI.score,
                         #depression = df.post$BDI.score,
                         #unemployment = df.post$UE.June,
                         policy=df.post$mwr,
                         ctl=df.post$ctl,
                         mask=df.post$rel.freq,
                         cases = df.post$cases#,
                         #deaths = df.post$deaths,
                         #protest = df.post$protest
                         )

# all:paranoia
lm_all <- lm(paranoia ~ cases*policy*ctl*mask, data = regression)
summary(lm_all)
backward.all = step(lm(lm_all, data = regression), method = "backward")
summary(backward.all)

# Plot
plot_model(backward.all, type = "pred", terms = c("mask","ctl [27.37,48.43,75.45]","policy"))

p <- plot_model(backward.all, type = "pred", terms = c("mask","ctl [27.37,48.43,75.45]","policy"))

p + theme(panel.background = element_rect(fill = "white"),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          axis.line = element_line( colour = "black"),
          legend.position = "none",
          #legend.box = "vertical",
          axis.text.x=element_blank(),
          axis.text.y = element_blank(),
          aspect.ratio = 1) + labs(title = "", y = "", x="")

p 




