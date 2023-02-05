# Load Libraries
library(psych)
library(tidyverse)
library(ggplot2)
library(report)
library(car)

# Data Cleaning

# Import Data
data <- data.frame(read.csv("SESStudy1_12_10_21.csv", header = TRUE))
data <- data[-1:-2,18:103]


# SDO Transform Function
sdo_transform <- function(data)
{
  df <- dplyr::recode({{data}},
               "Strongly Oppose"=1,
               "Somewhat Oppose"=2,
               "Slightly Oppose"=3,
               "Neutral"=4,
               "Slightly Favor"=5,
               "Somewhat Favor"=6,
               "Strongly Favor"=7)
}

# 7pt Likert Scale Transformation Function
rest_transform <- function(data)
{
  df <- dplyr::recode({{data}},
               "Strongly Disagree"=1,
               "Moderately Disagree"=2,
               "Slightly Disagree"=3,
               "Neutral"=4,
               "Slightly Agree"=5,
               "Moderately Agree"=6,
               "Strongly Agree"=7) 
}

# Transform SDO, WP, WG, MRS and CSER Scales
data <- data %>% mutate(across(.cols = c(SDO_1,SDO_2,SDO_3,SDO_4,
                                         SDO_5,SDO_6,SDO_7,SDO_8,
                                         SDO_9,SDO_10,SDO_11,SDO_12,
                                         SDO_13,SDO_14,SDO_15,SDO_16), sdo_transform))

data <- data %>% 
  mutate(across(.cols = c(WPS_1,WPS_2,WPS_3,WPS_4,WPS_5,
                          WG_1,WG_2,WG_3,WG_4,WG_5,MRS_1,MRS_2,MRS_3,MRS_4,MRS_5,MRS_6,
                          CSER_1,CSER_2,CSER_3,CSER_4,CSER_5,CSER_6,CSER_7,CSER_8,CSER_9,CSER_10,CSER_11,
                          CSER_12,CSER_13,CSER_14,CSER_15,CSER_16), rest_transform))

#Rename Columns & Recode Attention to Binary (1 = Pass, 0 = Fail)
data <- data %>% rename(
  "Att_1" = "Q38",
  "Att_2" = "Q39",
  "Att_3" = "WPS_6",
  "Att_4" = "WG_6",
  "Att_5" = "MRS_7",
  "Att_6" = "Q40",
  "Att_7" = "CSER_17",
  "Att_8" = "SDO_17",
  "Condition" = "FL_29_DO") %>% 
  mutate(Att_1 = if_else(
    Att_1 == "A",1,0),
    Att_2 = if_else(Att_2 == "A",1,0),
    Att_3 = if_else(Att_3 == "Strongly Agree",1,0),
    Att_4 = if_else(Att_4 == "Moderately Disagree",1,0),
    Att_5 = if_else(Att_5 == "Neutral",1,0),
    Att_6 = if_else(Att_6 == "C",1,0),
    Att_7 = if_else(Att_7 == "Slightly Disagree",1,0),
    Att_8 = if_else(Att_8 == "Slightly Favor",1,0))

data$Att_1 <- if_else(data$Att_1 == 0,data$Att_2,data$Att_1)

# Create Att Check Composite
data <- data %>% mutate(Att_Total = Att_1 + Att_3 + Att_4 + Att_5 + Att_6 + Att_7 + Att_8)

# Filter (Race = White)
data2 <- filter(data, data$D3 == "White")
# Filter (Att >= 5)
data2 <- filter(data2, data2$Att_Total >= 5)

# White Privilege Scale (Higher = More Privilege Awareness)
WP <- data.frame(data2$WPS_1, data2$WPS_2, data2$WPS_3, data2$WPS_4, data2$WPS_5)
WP <- as.data.frame(sapply(WP, as.numeric))
WPKey <- c(1,1,1,-1,1)
WP <- data.frame(reverse.code(WPKey,WP, mini = NULL, maxi = NULL))
colnames(WP) <- c("1","2","3","4","5")
# Create Mean
data2$WPMean <- rowMeans(WP, na.rm = FALSE, dims = 1L)

# White Guilt (Higher = More Guilt)
WG <- data.frame(data2$WG_1,data2$WG_2,data2$WG_3,data2$WG_4,data2$WG_5)
WG <- as.data.frame(sapply(WG, as.numeric))
WGKey <- c(1,1,-1,1,1)
WG <- data.frame(reverse.code(WGKey,WG, mini = NULL, maxi = NULL))
colnames(WG) <- c("1","2","3","4","5")
# Create WG Mean
data2$WGMean <- rowMeans(WG, na.rm = FALSE, dims = 1L)


# Social Dominance Orientation
SDO <- data.frame(data2$SDO_1,data2$SDO_2,data2$SDO_3,data2$SDO_4,data2$SDO_5,data2$SDO_6,data2$SDO_7,data2$SDO_8,data2$SDO_9,data2$SDO_10,data2$SDO_11,data2$SDO_12,data2$SDO_13,data2$SDO_14,data2$SDO_15,data2$SDO_16)
SDO <- as.data.frame(sapply(SDO, as.numeric))
SDOKey <- c(1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1)
SDO <- data.frame(reverse.code(SDOKey,SDO,mini = NULL,maxi = NULL))
colnames(SDO) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")
# Create SDO Mean
data2$SDOMean <- rowMeans(SDO, na.rm = FALSE, dims = 1L)



# Collective Self Esteem
CSE <- data.frame(data2$CSER_1,data2$CSER_2,data2$CSER_3,data2$CSER_4,data2$CSER_5,data2$CSER_6,data2$CSER_7,data2$CSER_8,data2$CSER_9,data2$CSER_10,data2$CSER_11,data2$CSER_12,data2$CSER_13,data2$CSER_14,data2$CSER_15,data2$CSER_16)
CSE <- as.data.frame(sapply(CSE, as.numeric))
CSEKey <- c(-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1)
CSE <- data.frame(reverse.code(CSEKey,CSE, mini = NULL, maxi = NULL))
colnames(CSE) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")
# Create CES (Race) Mean
data2$CSERMean <- rowMeans(CSE, na.rm = FALSE, dims = 1L)



# Modern Racism (Higher = More Racist Attitudes)
MRS <- data.frame(data2$MRS_1,data2$MRS_2,data2$MRS_3,data2$MRS_4,data2$MRS_5,data2$MRS_6)
MRS <- as.data.frame(sapply(MRS, as.numeric))
MRSKey <- c(1,-1,1,1,1,1)
MRS <- data.frame(reverse.code(MRSKey,MRS, mini = NULL, maxi = NULL))
colnames(MRS) <- c("1","2","3","4","5","6")

# Create MRS Mean
data2$MRSMean <- rowMeans(MRS, na.rm = FALSE, dims = 1L)
describe(data2$MRSMean)

# Create Rank Variable
data2 <- data2 %>% mutate(across(.cols = c(Q29_1,Q26_1),.fns = as.numeric))

#Create Condition
data2$Condition <- as.factor(data2$Condition)

#Replace Missing LowSES with 0
data2$Q29_1 <- replace_na(data2$Q29_1,0)

#Replace Mising HighSES with 0
data2$Q26_1 <- replace_na(data2$Q26_1,0)

#Create Single Rank Variable
data2$rank <- data2$Q26_1 + data2$Q29_1


#Relevant Data
keydata <- data.frame(data2$D1, data2$D3, data2$D4, data2$D5_1, data2$D6, data2$rank, data2$Condition, data2$WPMean, data2$WGMean, data2$SDOMean, data2$MRSMean, data2$CSERMean,
                      WP$`1`,WP$`2`,WP$`3`,WP$`4`,WP$`5`,
                      WG$`1`,WG$`2`,WG$`3`,WG$`4`,WG$`5`,
                      SDO$`1`,SDO$`2`,SDO$`3`,SDO$`4`,SDO$`5`,SDO$`6`,SDO$`7`,SDO$`8`,SDO$`9`,SDO$`10`,SDO$`11`,SDO$`12`,SDO$`13`,SDO$`14`,SDO$`15`,SDO$`16`,
                      MRS$`1`,MRS$`2`,MRS$`3`,MRS$`4`,MRS$`5`,MRS$`6`,
                      CSE$`1`,CSE$`2`,CSE$`3`,CSE$`4`,CSE$`5`,CSE$`6`,CSE$`7`,CSE$`8`,CSE$`9`,CSE$`10`,CSE$`11`,CSE$`12`,CSE$`13`,CSE$`14`,CSE$`15`,CSE$`16`)

colnames(keydata) <- c("Age","Race","Religion","Politics","Household_Income","Rank","Condition","WP_Mean","WGuilt_Mean","SDO_Mean","MR_Mean", "CSER_Mean",
                       "WP1","WP2","WP3","WP4","WP5",
                       "WG1","WG2","WG3","WG4","WG5",
                       "SDO1","SDO2","SDO3","SDO4","SDO5","SDO6","SDO7","SDO8","SDO9","SDO10","SDO11","SDO12","SDO13","SDO14","SDO15","SDO16",
                       "MRS1","MRS2","MRS3","MRS4","MRS5","MRS6",
                       "CSE1","CSE2","CSE3","CSE4","CSE5","CSE6","CSE7","CSE8","CSE9","CSE10","CSE11","CSE12","CSE13","CSE14","CSE15","CSE16")

keydata$Race <- as.factor(keydata$Race)
keydata$Religion <- as.factor(keydata$Religion)
keydata$Household_Income <- as.factor(keydata$Household_Income)
keydata$Condition <- as.factor(keydata$Condition)

describe(keydata)

# Export Raw

library(haven)
haven::write_sav(data,"SES_Study1_Dec_2021_Raw.sav")
haven::write_sav(keydata,"SES_Study1_Dec_2021_Clean.sav")

# Export Cleaned

summary(aov(WP_Mean ~ SDO_Mean*Condition + WGuilt_Mean, data = keydata))
