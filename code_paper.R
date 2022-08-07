install.packages("rmdformats")
install.packages("readxl")
install.packages("tidyverse")
install.packages("summarytools")
install.packages("epiDisplay")
intall.packages("nortest")
intall.packages("diptest")
install.packages("LaplacesDemon")
install.packages("car")
install.packages("plyr")
install.packages("ggplot2")
install.packages("effectsize")
install.packages("outliers")
install.packages("rstatix")


library(rmdformats)
library(readxl)
library(tidyverse)
library(summarytools)
library(epiDisplay)
library(nortest)
library(diptest)
library(LaplacesDemon)
library(car)
library(plyr)
library(ggplot2)
library(effectsize)
library(outliers)
library(rstatix)

OurData <- read_excel("C:/Users/annag/Desktop/homework/II semester/replication paper/R/Replication Data.xlsx")
OurData <-rename(OurData, c("Participant ID" = "ID" ))
OurData <-rename(OurData, c("# of siblings" ="Siblings"))
OurData <-rename(OurData, c("# of Stickers Shared"= "Stickers_Shared" ))
OurData <-rename(OurData, c("Control Q1: How many tries do you have?" = "C1"))
OurData <-rename(OurData, c("Control Q2: Where to put the stickers" = "C2"))
OurData <-rename(OurData, c("Did they guess the aim of the study?"= "Study_Aim"))
OurData <-rename(OurData, c("Employment status" = "Employment"))
OurData <-rename(OurData, c("Education (enrolled)" = "Education" ))

## DATA CLEANING:

# Remove pilot person
OurData1 <-subset(OurData, ID != "PILOT")

# Remove people who failed
OurData1 <-subset(OurData1, C1 != "Failed" & C2 != "Failed")

# Remove people who are older than 30
OurData2 <- subset(OurData1, Age <= 30)

TimeP <- subset(OurData2, Condition == "Time Pressure")
NoTimeP <- subset(OurData2, Condition == "NO Pressure")

Female_Data <- subset(OurData2, Gender == "Female")
Male_Data <- subset(OurData2, Gender != "Female")

# DEMOGRAPHICS

##!!Sample collected, without exclusion criteria!!

descr(OurData1, headings = FALSE, stats = "common", transpose = TRUE)

freq(OurData1$Gender,report.nas = FALSE,total = TRUE, cumul = FALSE, headings = TRUE)


## Sample after data cleaning

descr(OurData2, headings = FALSE, transpose = TRUE)


##CONDITIONS: 
stby(data = OurData2, INDICES = OurData2$Condition,FUN = descr,
     transpose = TRUE)


##GENDER
freq(OurData2$Gender,report.nas = FALSE,total = TRUE, cumul = FALSE, headings = TRUE)

ctable(x = OurData2$Condition, y = OurData2$Gender, prop = "t")

stby(data = OurData2, INDICES = OurData2$Gender,FUN = descr,
     transpose = TRUE)

Male_Data %>% filter(Condition == "Time Pressure") %>% 
  summarise(mean(Stickers_Shared), sd(Stickers_Shared), median(Stickers_Shared),IQR(Stickers_Shared))

Male_Data %>% filter(Condition == "NO Pressure") %>% 
  summarise(mean(Stickers_Shared), sd(Stickers_Shared), median(Stickers_Shared), IQR(Stickers_Shared))

Female_Data %>% filter(Condition == "Time Pressure") %>% 
  summarise(mean(Stickers_Shared), sd(Stickers_Shared), median(Stickers_Shared), IQR(Stickers_Shared))

Female_Data %>% filter(Condition == "NO Pressure") %>% 
  summarise(mean(Stickers_Shared), sd(Stickers_Shared), median(Stickers_Shared), IQR(Stickers_Shared))



##EDUCATION
freq(OurData2$Education,report.nas = FALSE,total = TRUE, cumul = FALSE, headings = TRUE)

ctable(x = OurData2$Condition, y = OurData2$Education,prop = "t")

##EMPLOYMENT STATUS
freq(OurData2$Employment,report.nas = FALSE,total = TRUE, cumul = FALSE, headings = TRUE)

ctable(x = OurData2$Condition, y = OurData2$Employment,prop = "t")

##STUDY AIM
freq(OurData2$Study_Aim,report.nas = FALSE,total = TRUE, cumul = FALSE, headings = TRUE)

ctable(x = OurData2$Condition, y = OurData2$Study_Aim,prop = "t")

## OUTLIERS 
dixon.test(OurData2$Stickers_Shared)

dixon.test(OurData2$Stickers_Shared, opposite = TRUE)

#no significant outliers

##ASSUMPTIONS for testing

##1. NORMALITY:

#scaterplot: 
ggplot(OurData2)+
  geom_bar(aes(Stickers_Shared))

ggplot(NoTimeP)+
  geom_bar(aes(Stickers_Shared))


ggplot(TimeP)+
  geom_bar(aes(Stickers_Shared)) 

#Normality test: 

lillie.test(NoTimeP$Stickers_Shared)

#the p-value is significant so we cannot assume normality

lillie.test(TimeP$Stickers_Shared)

#the p-value is significant so we cannot assume normality

lillie.test(OurData2$Stickers_Shared)

#the p-value is significant so we cannot assume normality


#2. MODALITY: 

#violin plots
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

ggplot(OurData2, aes(x = Condition, y = Stickers_Shared, group = Condition))+
  geom_violin(trim = FALSE) +
  stat_summary(fun.data= data_summary, color = "blue") +
  geom_jitter(color = "darkred", size = 0.5)+
  ylab("Stickers shared")

ggplot(OurData2, aes(x = Gender, y = Stickers_Shared, group = Gender))+
  geom_violin(trim = FALSE) +
  stat_summary(fun.data= data_summary, color = "blue") +
  geom_jitter(color = "darkred", size = 0.5)+
  ylab("Stickers shared")


#Modality test: 

dip.test(TimeP$Stickers_Shared)

dip.test(NoTimeP$Stickers_Shared)

dip.test(OurData2$Stickers_Shared)

dip.test(Male_Data$Stickers_Shared)

dip.test(Female_Data$Stickers_Shared)

## we can assume unimodality for the distributions in the two conditions, but 
#not in the overall distribution and in the distributionsby gender

##3. HOMOGENEITY OF VARIANCE
leveneTest(OurData2$Stickers_Shared, OurData2$Condition)

#We can assume homogeneity

### --> Not all assumptions for t-test are met

## Main hypothesis:

# 1. directed hypothesis --> H1: Adults in the Time Pressure Condition share more 
# stickers than those in the No Time Pressure Condition

wilcox_test(Stickers_Shared ~ Condition, data = OurData2, alternative = "less")

wilcox.test(Stickers_Shared ~ Condition, data = OurData2, alternative = "less", paired = FALSE)


#p-value is not significant


# 2. not directed hypothesis --> H1: There is a difference in shared stickers between
# conditions

wilcox_test(Stickers_Shared ~ Condition, data = OurData2)

#p-value is not significant

#EFFECT SIZES: 

rank_biserial(Stickers_Shared ~ Condition, data = OurData2, alternative = "less")

rank_biserial(Stickers_Shared ~ Condition, data = OurData2)


## violin plot (median):
data_summary <- function(x) {
  m <- median(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

ggplot(OurData2, aes(x = Condition, y = Stickers_Shared, group = Condition))+
  geom_violin(trim = FALSE) +
  stat_summary(fun.data= data_summary, color = "blue") +
  geom_jitter(color = "darkred", size = 0.5)+
  ylab("Stickers shared")

#Secondary hypotheses: 
# H2: Growing up with siblings increased sharing behaviour 

Siblings <- subset(OurData2, Siblings != "0")

NoSiblings <- subset(OurData2, Siblings == "0")

lillie.test(Siblings$Stickers_Shared)

lillie.test(NoSiblings$Stickers_Shared)

OurData_Siblings <- OurData2

OurData_Siblings$Siblings[OurData_Siblings$Siblings > 1] <- 1

replace(OurData_Siblings$Siblings, OurData_Siblings$Siblings > 1,1)

wilcox_test(Stickers_Shared ~ Siblings, data = OurData_Siblings, alternative = "less")

#no significant p-value

rank_biserial(Stickers_Shared ~ Siblings, data = OurData_Siblings, alternative = "less")


#H3: Adults who guessed the true nature of the study were more likely to be prosocial

Study_Aim <- subset(OurData2, Study_Aim == "Yes")

No_Study_Aim <- subset(OurData2, Study_Aim != "No")

lillie.test(Study_Aim$Stickers_Shared)

lillie.test(No_Study_Aim$Stickers_Shared)

is.unimodal(Study_Aim$Stickers_Shared)

is.unimodal(No_Study_Aim$Stickers_Shared)

wilcox_test(Stickers_Shared ~ Study_Aim, data = OurData2, alternative = "less")

# p-value not significant

rank_biserial(Stickers_Shared ~ Study_Aim, data = OurData2, alternative = "less")


# Is there an effect of gender? Do women shared more? 

lillie.test(Female_Data$Stickers_Shared)

lillie.test(Male_Data$Stickers_Shared)

wilcox_test(Stickers_Shared ~ Gender, data = OurData2, alternative = "greater")

# p-value not significant

rank_biserial(Stickers_Shared ~ Gender, data = OurData_Siblings, alternative = "greater")


## is there an interaction between gender and condition?

ggplot(OurData2, aes(x = Condition, y = Stickers_Shared, group = Condition, color = Gender))+
  geom_violin(trim = FALSE) +
  stat_summary(fun.data= data_summary, color = "blue") +
  geom_jitter(size = 0.5)+
  ylab("Stickers shared")

ggplot(OurData2)+
  geom_point(aes(x = Condition, y = Stickers_Shared, color = Gender)) +
  ylab("Stickers shared")

Interaction_1 <- lm(Stickers_Shared ~ Gender + Condition, data = OurData2)
summary(Interaction_1)

Interaction_2 <- lm(Stickers_Shared ~ Gender + Condition + Gender:Condition, data = OurData2)
summary(Interaction_2)
#closest to 0.05

Interaction_3 <-lm(Stickers_Shared ~ Gender * Condition, data = OurData2)
summary(Interaction_3)
#closest to 0.05


## is there an effect of age?

ggplot(OurData2)+
  geom_point(aes(x = Stickers_Shared, y = Age))

lillie.test(OurData2$Stickers_Shared)

lillie.test(OurData2$Age)

cor.test(OurData2$Stickers_Shared, OurData2$Age, method=c("kendall"))

# p-value not significant 


#is there an interaction of age and condition?

ggplot(OurData2)+
  geom_point(aes(x = Stickers_Shared, y = Age, color = Condition))


Interaction_4 <- lm(Stickers_Shared ~ Age + Condition, data = OurData2)
summary(Interaction_4)

Interaction_5 <- lm(Stickers_Shared ~ Age + Condition + Age:Condition, data = OurData2)
summary(Interaction_5)

Interaction_6 <- lm(Stickers_Shared ~ Age * Condition, data = OurData2)
summary(Interaction_6)




