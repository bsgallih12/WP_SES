# Load Libraries 
library(tidyverse)
library(readxl)
library(psych)

# Read Data
data <- data.frame(read.csv("WP_SES_Study2_Raw.csv"))
data <- data[3:618,18:104]
# Rename Condiition
data <- data %>% rename("Condition" = "FL_29_DO")
# Rename Demographics
data <- data %>% rename("Age" = "D1",
                        "Gender" = "D2",
                        "Gender_Alt" = "D2_7_TEXT",
                        "Race" = "D3",
                        "Race_Alt" = "D3_8_TEXT",
                        "Religion" = "D4",
                        "Religion_Alt" = "D4_13_TEXT",
                        "Politics" = "D5_1",
                        "SES" = "D6",
                        "Education" = "D7")

# Transform and Clean

# SDO Transform Function
sdo_transform <- function(data)
{
  df <- recode({{data}},
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
  df <- recode({{data}},
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
                                         SDO_13,SDO_14,SDO_15,SDO_16,SDO_17), sdo_transform))

data <- data %>% 
  mutate(across(.cols = c(WPS_1,WPS_2,WPS_3,WPS_4,WPS_5,WPS_6,
                          WG_1,WG_2,WG_3,WG_4,WG_5,WG_6,MRS_1,MRS_2,MRS_3,MRS_4,MRS_5,MRS_6,MRS_7,
                          CSER_1,CSER_2,CSER_3,CSER_4,CSER_5,CSER_6,CSER_7,CSER_8,CSER_9,CSER_10,CSER_11,
                          CSER_12,CSER_13,CSER_14,CSER_15,CSER_16,CSER_17), rest_transform))

# Filter For White Only Participants
data <- data %>% filter(Race == "White")

# Rename Att Check Variables
data <- data %>% rename("Att_1" = "SDO_17",
                        "Att_2" = "Q38",
                        "Att_3" = "Q39",
                        "Att_4" = "WPS_6",
                        "Att_5" = "WG_6",
                        "Att_6" = "MRS_7",
                        "Att_7" = "Q40",
                        "Att_8" = "CSER_17"
)

# Transform Att Check Variables
data <- data %>% mutate(
  Att_1 = if_else(Att_1 == 5,1,0),
  Att_2 = if_else(Att_2 == "A",1,0),
  Att_3 = if_else(Att_3 == "A",1,0),
  Att_4 = if_else(Att_4 == 7,1,0),
  Att_5 = if_else(Att_5 == 2,1,0),
  Att_6 = if_else(Att_6 == 4,1,0),
  Att_7 = if_else(Att_7 == "C",1,0),
  Att_8 = if_else(Att_8 == 3,1,0))

data$Att_2 <- if_else(data$Att_2 == 0,data$Att_3,data$Att_2)

# Create Att Check Composite
data <- data %>% mutate(Att_Total = Att_1 + Att_2 + Att_4 + Att_5 + Att_6 + Att_7 + Att_8)

# Filter Data For Att Check >= 6
data <- data %>% filter(Att_Total >= 5)


# SDO Recode
sdo <- data %>% select(SDO_1,SDO_2,SDO_3,SDO_4,SDO_5,SDO_6,SDO_7,SDO_8,SDO_9,
                       SDO_10,SDO_11,SDO_12,SDO_13,SDO_14,SDO_15,SDO_16)
sdokey <- c(1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1)
sdo_R <- psych::reverse.code(sdokey,sdo)

data$sdo_mean <- rowMeans(sdo_R, na.rm = TRUE, dims = 1L)


# WPS Recode 
wps <- data %>% select(WPS_1,WPS_2,WPS_3,WPS_4,WPS_5)
wpskey <- c(1,1,1,-1,1)
wps_R <- psych::reverse.code(wpskey,wps)
data$wps_mean <- rowMeans(wps_R, na.rm = TRUE, dims = 1L)

# WG Recode 
wg <- data %>% select(WG_1,WG_2,WG_3,WG_4,WG_5)
wgkey <- c(1,1,-1,1,1)
wg_R <- psych::reverse.code(wgkey,wg)
data$wg_mean <- rowMeans(wg_R, na.rm = TRUE, dims = 1L)

# MRS Recode 

mrs <- data %>% select(MRS_1,MRS_2,MRS_3,MRS_4,MRS_5,MRS_6)
mrskey <- c(1,-1,1,1,1,1)
mrs_R <- psych::reverse.code(mrskey,mrs)
data$mrs_mean <- rowMeans(mrs_R, na.rm = TRUE, dims = 1L)

# CSER Overall Recode

cse_total <- data %>% select(CSER_1,CSER_2,CSER_3,CSER_4,CSER_5,CSER_6,CSER_7,CSER_8,
                             CSER_9,CSER_10,CSER_11,CSER_12,CSER_13,CSER_14,CSER_15,CSER_16)
cse_total_key <- c(-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1)
cse_total_R <- psych::reverse.code(cse_total_key,cse_total)
data$cse_total <- rowMeans(cse_total_R, na.rm = TRUE, dims = 1L)

# Create CSE Subscales

# Private CSER Recode
cse_private <- cse_total %>% select(CSER_6, CSER_9, CSER_12,CSER_15)
privatekey <- c(-1,1,-1,1)
cse_private_R <- psych::reverse.code(privatekey,cse_private)
data$cse_private <- rowMeans(cse_private_R, na.rm = TRUE, dims = 1L)

# Public CSER Recode
cse_public <- cse_total %>% select(CSER_7,CSER_10,CSER_13,CSER_16)
publickey <- c(1,-1,1,-1)
cse_public_R <- psych::reverse.code(publickey,cse_public)
data$cse_public <- rowMeans(cse_public_R, na.rm = TRUE, dims = 1L)

# Membership CSER Recode
cse_membership <- cse_total %>% select(CSER_5,CSER_8,CSER_11,CSER_14)
membershipkey <- c(1,-1,1,-1)
cse_membership_R <- psych::reverse.code(membershipkey,cse_membership)
data$cse_membership <- rowMeans(cse_membership_R, na.rm = TRUE, dims = 1L)

# Identity CSER Recode
cse_identity <- cse_total %>% select(CSER_1,CSER_2,CSER_3,CSER_4)
identitykey <- c(-1,1,-1,1)
cse_identity_R <- psych::reverse.code(identitykey,cse_identity)
data$cse_identity <- rowMeans(cse_identity_R, na.rm = TRUE, dims = 1L)

# Create Relevant Variables Dataframe
keyvariables <- data %>% select(
  "Age","Gender","Gender_Alt","Race","Race_Alt","Religion","Religion_Alt",
  "Politics","SES","Education","Q37","Q36","Q35","Q26_1","Q32","Q33",
  "Q34","Q29_1","RGP_1","RGP_2","RGP_3","RGP_4","RGP_5","RGP_6","RGP_7",
  "RGP_8","RGP_9","Condition","sdo_mean","wps_mean","wg_mean","mrs_mean",
  "cse_total","cse_private","cse_public","cse_membership","cse_identity"  
)

# Merge Gender Alt and Religion_Alt into Gender and Religion Column
# Remove Religion_Alt, Gender_Alt and Race_Alt from data set
keyvariables <- keyvariables %>% 
  mutate(Gender = ifelse(Gender == "Other", Gender_Alt, Gender)) %>% 
  mutate(Religion = ifelse(Religion == "Other (Please Specify)",Religion_Alt,Religion)) %>% 
  select(-Religion_Alt,-Race_Alt,-Gender_Alt)

# Convert Gender, Religion, Race and Education to Factor 
keyvariables <- keyvariables %>% 
  mutate(across(.cols = c(Gender,Religion,Race,Education),.fns = as.factor))

# Recode Political Affiliation and SES
keyvariables <- keyvariables %>% 
  mutate(Politics = recode(Politics,
                           "Extreme Liberal" = 1,
                           "Strong Liberal" = 2,
                           "Moderate Liberal" = 3,
                           "Slight Liberal" = 4,
                           "Neutral" = 5,
                           "Slight Conservative" = 6,
                           "Moderate Conservative" = 7,
                           "Strong Conservative" = 8,
                           "Extreme Conservative" = 9)) %>%
  mutate(SES = as.factor(SES)) %>% mutate(SES = recode(SES,
                            "$10,000 to $19,999" = 2,
                            "$100,000 to $149,999" = 11,
                            "$150,000 or more" = 12,
                             "$20,000 to $29,999" = 3,
                             "$30,000 to $39,999" = 4,
                            "$40,000 to $49,999" = 5,
                             "$50,000 to $59,999" = 6,
                             "$60,000 to $69,999" = 7,
                              "$70,000 to $79,999" = 8,
                             "$80,000 to $89,999" = 9,
                             "$90,000 to $99,999" = 10,
                             "Less than $10,000" = 1)) %>% 
  mutate(SES = as.numeric(SES))

# Merge Free Responses and Remove Left Over Columns

keyvariables <- keyvariables %>% 
  mutate(Q37 = ifelse(Q37 == "",Q32,Q37)) %>% 
  mutate(Q36 = ifelse(Q36 == "",Q33,Q36)) %>% 
  mutate(Q35 = ifelse(Q35 == "",Q34,Q35)) %>% 
  select(-Q32,-Q33,-Q34)

# Rename Free Response Variables and Merge Rank Variables and Remove Left Over Columns
keyvariables <- keyvariables %>% 
  rename(
  "Response_1" = 'Q37',
  "Response_2" = 'Q36',
  "Response_3" = 'Q35') %>% 
  mutate(Q26_1 = ifelse(Q26_1 == "",Q29_1,Q26_1)) %>% 
  rename(
    "Rank" = 'Q26_1'
  ) %>% select(-Q29_1)

# Export to SPSS/JASP File
library(haven)
haven::write_sav(keyvariables,"Kim_SES_Study2_Cleaned.sav")
write.csv(keyvariables,"SES_Study2_Cleaned.csv")