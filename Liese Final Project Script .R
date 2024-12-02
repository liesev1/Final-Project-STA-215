## Project:  STA 215, Fall 2024, Final Project
# Located:   STA215
# File Name: template
# Date:      2024_11_25
# Who:       Zachary D. Kline

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
setwd("~/Desktop/STA215")
dataset <- read_csv("dataset.csv")
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
dataset_nomissing_nomissing <- na.omit(dataset[, c("wealth", "voting_record", "age", "party", "gun_policy")])

table(dataset_nomissing_nomissing$party)
table(dataset_nomissing_nomissing$gun_policy)

summary(dataset_nomissing_nomissing$wealth)
sd(dataset_nomissing_nomissing$wealth)

summary(dataset_nomissing_nomissing$age)
sd(dataset_nomissing_nomissing$age)


##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
table(dataset_nomissing_nomissing$prev_job)
mean(dataset_nomissing_nomissing$wealth)

dataset_nomissing$lnwealth <- log(dataset_nomissing$wealth + 1)
boxplot(lnwealth ~ prev_job, data = dataset_nomissing)


anova <- aov(lnwealth ~ prev_job, data = dataset_nomissing)
summary(anova)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
dataset_nomissing_nomissing <- na.omit(dataset_nomissing[, c("voting_record", "age")])



linear_plot <- plot(dataset_nomissing_nomissing$age, dataset_nomissing_nomissing$voting_record)
print(linear_plot)
meany <- mean(dataset_nomissing_nomissing$voting_record, na.rm = TRUE)
meanx <- mean(dataset_nomissing_nomissing$age, na.rm = TRUE)
abline(v = meanx, col = "black")
abline(h = meany, col = "black")
linear_relationship <- lm(voting_record ~ age, data = dataset_nomissing_nomissing)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset_nomissing_nomissing$age, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset_nomissing$party, dataset_nomissing$gun_policy)

chisq.test(table(dataset_nomissing$party, dataset_nomissing$gun_policy))


