source("SES_2_Clean.R")
library(HayesPROCESS)
library(car)

data <- keyvariables

# Interaction Between Condition and SDO on WP (Control for WG; ANOVA)
aov1 <- aov(wps_mean ~ Condition * sdo_mean + wg_mean, data = data)
summary(aov1)
Anova(aov1, type = "III")


# SDO Associated With More WP Denial in Low SES Condition (Correlation)

LowCondition <- data %>% filter(Condition == "LowSESManipulation")
corr.test(LowCondition$sdo_mean,LowCondition$wps_mean)

# SDO Not Associated With More WP Denial in High SES Condition (Correlation)

HighCondition <- data %>% filter(Condition == "HighSESManipulation")
corr.test(HighCondition$sdo_mean,HighCondition$wps_mean)

