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
library(plotrix)
library(ggpattern)
library(car)
library(lsr)

# read in data
dat <- read.csv("pandemicPRL.csv", 
                stringsAsFactors = FALSE)


# subset pandemic, lockdown and postlockdown data
pandemic_lockdown_postlockdown <- dat[which(dat$dataset == "pandemic" & (dat$period == "lockdown"| 
                                                                           dat$period == "postlockdown")),]


# calculate contamination fear score
pandemic_lockdown_postlockdown$contam_fear_score <- rowMeans(pandemic_lockdown_postlockdown[, grepl("docs_", names(pandemic_lockdown_postlockdown))][,1:5], na.rm = TRUE)



pandemic_lockdown_postlockdown_subset <- data.frame(period = pandemic_lockdown_postlockdown$period,
                                                proactivity = pandemic_lockdown_postlockdown$state_proactivity,
                                                mask_mandate = pandemic_lockdown_postlockdown$state_mask_mandate,
                                                contam_fear = pandemic_lockdown_postlockdown$contam_fear_score)


pandemic_lockdown_postlockdown_subset$period <- factor(pandemic_lockdown_postlockdown_subset$period, levels = c("lockdown", "postlockdown"))


pandemic_lockdown_postlockdown_subset$proactivity <- ifelse(pandemic_lockdown_postlockdown_subset$proactivity <= median(pandemic_lockdown_postlockdown_subset$proactivity,na.rm = TRUE), "less proactive","more proactive")
pandemic_lockdown_postlockdown_subset$mask_mandate <- ifelse(pandemic_lockdown_postlockdown_subset$mask_mandate == 0, "recommended","required")
pandemic_lockdown_postlockdown_subset$mask_mandate <- factor(pandemic_lockdown_postlockdown_subset$mask_mandate, levels = c("recommended","required"))


# pre-lockdown follows lockdown proactivity levels, post-lockdown proactivity based on mask-mandate status
proactivity_lockdown <- as.data.frame(pandemic_lockdown_postlockdown_subset[which(pandemic_lockdown_postlockdown_subset$period == "lockdown"),]$proactivity)
colnames(proactivity_lockdown) <- "proactivity"
proactivity_postlockdown <- as.data.frame(pandemic_lockdown_postlockdown_subset[which(pandemic_lockdown_postlockdown_subset$period == "postlockdown"),]$mask_mandate)
proactivity_postlockdown <- ifelse(proactivity_postlockdown == "recommended","less proactive","more proactive")
colnames(proactivity_postlockdown) <- "proactivity"

proactivity <- rbind(proactivity_lockdown, proactivity_postlockdown)


pandemic_lockdown_postlockdown_df <- data.frame(period = pandemic_lockdown_postlockdown_subset$period,
                                                proactivity,
                                                contam_fear = pandemic_lockdown_postlockdown_subset$contam_fear)


stats_contam_fear <- pandemic_lockdown_postlockdown_df %>%
  dplyr::group_by(period, proactivity) %>%
  dplyr::summarise(contam_fear_mean = mean(contam_fear, na.rm = TRUE),
                   contam_fear_sem = std.error(contam_fear, na.rm = TRUE))


p1 <- ggplot(pandemic_lockdown_postlockdown_df, aes(x = period,
                                                    y = contam_fear)) +
  geom_boxplot_pattern(aes(fill=proactivity),pattern=c("stripe","none",
                                                       "stripe","none"),pattern_colour = "black",
                       #width = .7,
                       lwd = 1.2#,
                       #position = position_dodge(width=0.8)
  ) +
  #scale_colour_manual(values = c("#f2a097","#ed7669","#e84c3b"))+ #5c1a33
  scale_fill_manual(values = c("#675E24","#45731E"))+ #5c1a33
  scale_x_discrete(labels=c("lockdown" = "Lockdown",
                            "postlockdown" = "Post-Lockdown")) +
  labs(title="", x="Pandemic period", y="Contamination Fear") +  guides(#pattern = guide_legend(override.aes = list(fill = "white")),
    fill = guide_legend(override.aes = list(pattern=c("stripe","none")))
  )


p1 + theme_Publication() +  theme(axis.title.y = element_blank(),
                                       axis.title.x = element_blank(),
                                       axis.text = element_blank(), 
                                       axis.line = element_line(colour="black", size = 1.5),
                                       axis.ticks = element_line(colour="black", size = 1.5),
                                       legend.text = element_blank(),
                                       legend.position = "none")


p1 <- p1 + theme_Publication() + theme(legend.direction = "horizontal",
      legend.key.size= unit(2, "cm"),
      legend.spacing.x = unit(4.0, 'cm'),
      legend.text = element_blank())

# Using the cowplot package
legend <- cowplot::get_legend(p1)

grid.newpage()
grid.draw(legend)




aov2_contam_fear <- aov(contam_fear ~ proactivity * period, data = pandemic_lockdown_postlockdown_df)
summary(aov2_contam_fear)
Anova(aov2_contam_fear, type="III")

etaSquared(aov2_contam_fear, type = 3, anova = TRUE)

# We observe a main effect of period [F(1,399) = 15.50, p<0.001, eta_sq_part = 0.037] and 
# a significant interaction between the effects of proactivity and period on 
# contamination fear [F(1,399) = 6.36, p=0.012, eta_sq_part = 0.016]

df_postlockdown <- pandemic_lockdown_postlockdown_df[which(pandemic_lockdown_postlockdown_df$period == "postlockdown"),]
t.test(df_postlockdown$contam_fear ~ df_postlockdown$proactivity,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

cohensD(contam_fear ~ proactivity, data = df_postlockdown)

# We observe a significant mean difference 
# of paranoia [t(101)=-2.89, p=0.005, Cohen's d = 0.471, 95% CI=(-0.655,-0.121)] between
# less proactive and more proactive states 
# in reopening




# difference in contamination fear between lockdown and post-lockdown (reopening)
t.test(pandemic_lockdown_postlockdown_df$contam_fear ~ pandemic_lockdown_postlockdown_df$period,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)
