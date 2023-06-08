
# Creatinine
summary(Final_Biomarker_NHANES_20172018$lbdscrsi)
Final_Biomarker_NHANES_20172018$lbdscrsi_Wts<-0.0095
Final_Biomarker_NHANES_20172018$lbdscrsi_Terms<-Final_Biomarker_NHANES_20172018$lbdscrsi*Final_Biomarker_NHANES_20172018$lbdscrsi_Wts

# Glucose
summary(Final_Biomarker_NHANES_20172018$lbdsglsi)
Final_Biomarker_NHANES_20172018$lbdsglsi_Wts<-0.1953
Final_Biomarker_NHANES_20172018$lbdsglsi_Terms<-Final_Biomarker_NHANES_20172018$lbdsglsi*Final_Biomarker_NHANES_20172018$lbdsglsi_Wts

# CRP
summary(Final_Biomarker_NHANES_20172018$lbxhscrp)
Final_Biomarker_NHANES_20172018$lbxcrp_Conversion<-Final_Biomarker_NHANES_20172018$lbxhscrp*0.1
Final_Biomarker_NHANES_20172018$lbxcrp_LN_Conversion<-log(Final_Biomarker_NHANES_20172018$lbxcrp_Conversion)
Final_Biomarker_NHANES_20172018$lbxcrp_Wts<-0.0954
Final_Biomarker_NHANES_20172018$lbxcrp_Terms<-Final_Biomarker_NHANES_20172018$lbxcrp_LN_Conversion*Final_Biomarker_NHANES_20172018$lbxcrp_Wts

# Lymphocyte
summary(Final_Biomarker_NHANES_20172018$lbxlypct)
Final_Biomarker_NHANES_20172018$lbxlypct_Wts<--0.012
Final_Biomarker_NHANES_20172018$lbxlypct_Terms<-Final_Biomarker_NHANES_20172018$lbxlypct*Final_Biomarker_NHANES_20172018$lbxlypct_Wts

# Mean Cell Volume
summary(Final_Biomarker_NHANES_20172018$lbxmcvsi)
Final_Biomarker_NHANES_20172018$lbxmcvsi_Wts<-0.0268
Final_Biomarker_NHANES_20172018$lbxmcvsi_Terms<-Final_Biomarker_NHANES_20172018$lbxmcvsi*Final_Biomarker_NHANES_20172018$lbxmcvsi_Wts

# Red Cell Dist Width
summary(Final_Biomarker_NHANES_20172018$lbxrdw)
Final_Biomarker_NHANES_20172018$lbxrdw_Wts<-0.3306
Final_Biomarker_NHANES_20172018$lbxrdw_Terms<-Final_Biomarker_NHANES_20172018$lbxrdw*Final_Biomarker_NHANES_20172018$lbxrdw_Wts

# Alkaline Phosphatase
summary(Final_Biomarker_NHANES_20172018$lbxsapsi)
Final_Biomarker_NHANES_20172018$lbxsapsi_Wts<-0.0019
Final_Biomarker_NHANES_20172018$lbxsapsi_Terms<-Final_Biomarker_NHANES_20172018$lbxrdw*Final_Biomarker_NHANES_20172018$lbxsapsi_Wts

# White Blood Cells
summary(Final_Biomarker_NHANES_20172018$lbxwbcsi)
Final_Biomarker_NHANES_20172018$lbxwbcsi_Wts<-0.0554
Final_Biomarker_NHANES_20172018$lbxwbcsi_Terms<-Final_Biomarker_NHANES_20172018$lbxwbcsi*Final_Biomarker_NHANES_20172018$lbxwbcsi_Wts

################################################################################
# Survey Weights #
################################################################################
summary(Final_NHANES_19992020$sdmvpsu)
# Here we use "svydesign" to assign the weights. We will use this new design
# variable "nhanesDesign" when running our analyses.
library(survey)
Final_NHANES_19992020_nonmissing<-Final_NHANES_19992020[!is.na(Final_NHANES_19992020$sdmvpsu)]

Final_NHANES_19992020_nonmissing$lbdsapsi23<-ifelse(is.na(Final_NHANES_19992020_nonmissing$lbxsapsi),
                                                    paste(Final_NHANES_19992020_nonmissing$lb2sapsi),
                                                    paste(Final_NHANES_19992020_nonmissing$lbxsapsi))
Final_NHANES_19992020_nonmissing$lbdsapsi23<-as.numeric(Final_NHANES_19992020_nonmissing$lbdsapsi23)
summary(Final_NHANES_19992020_nonmissing$lbdsapsi23)

Final_NHANES_19992020_nonmissing$lbdsapsi233<-ifelse(is.na(Final_NHANES_19992020_nonmissing$lbdsapsi23),
                                                     paste(Final_NHANES_19992020_nonmissing$lbdsapsi),
                                                     paste(Final_NHANES_19992020_nonmissing$lbdsapsi23))
Final_NHANES_19992020_nonmissing$lbdsapsi233<-as.numeric(Final_NHANES_19992020_nonmissing$lbdsapsi233)
summary(Final_NHANES_19992020_nonmissing$lbdsapsi233)

Final_NHANES_19992020_nonmissing$lbxcrp2<-ifelse(is.na(Final_NHANES_19992020_nonmissing$lbxcrp),
                                                 paste(Final_NHANES_19992020_nonmissing$lbxhscrp),
                                                 paste(Final_NHANES_19992020_nonmissing$lbxcrp))
Final_NHANES_19992020_nonmissing$lbxcrp<-as.numeric(Final_NHANES_19992020_nonmissing$lbxcrp2)


nhanesDesign <- svydesign(id      = ~sdmvpsu,
                          strata  = ~sdmvstra,
                          weights = ~wtint2yr,
                          nest    = TRUE,
                          data    = Final_NHANES_19992020_nonmissing)

# Frequency table for Age Deciles

svytable(~age_deciles, nhanesDesign)
age_deciles_weighted<-c(320782414, 277403126, 291228458 ,256168770, 173890449, 133242298 , 19527628)

# 19-30	320782414	21.79%
# 31-40	277403126	18.84%
# 41-50	291228458	19.78%
# 51-60	256168770	17.40%
# 61-70	173890449	11.81%
# 71-80	133242298	9.05%
# 81-90	19527628	1.33%

# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Albumin_agegroups <- svyby(~lbdsalsi, ~factor(age_deciles), design = nhanesDesign, svymean)
Albumin_agegroups_sd <- svyby(~lbdsalsi, ~factor(age_deciles), design = nhanesDesign, svyvar)
Albumin_agegroups_range <- svyby(~lbdsalsi, ~age_deciles, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_agegroups_summarysd<-data.frame(Albumin_agegroups_sd)
Albumin_agegroups_summarysd$sd <- sqrt(Albumin_agegroups_summarysd$lbdsalsi)
Albumin_agegroups_summarysd

# View the results
Albumin_agegroups
Albumin_agegroups_summarysd
Albumin_agegroups_range



# Weighted Frequency table for Age Groups 

svytable(~AgeGroups, nhanesDesign)

# 19-39	320782414	38.6%
# 40-59	277403126	37.68%
# 60+	291228458	23.7%

# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Albumin_agegroups2 <- svyby(~lbdsalsi, ~factor(AgeGroups), design = nhanesDesign, svymean)
Albumin_agegroups2_sd <- svyby(~lbdsalsi, ~factor(AgeGroups), design = nhanesDesign, svyvar)
Albumin_agegroups2_range <- svyby(~lbdsalsi, ~AgeGroups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_agegroups_summary2 <- data.frame(Albumin_agegroups2)
Albumin_agegroups_summarysd2<-data.frame(Albumin_agegroups2_sd)
Albumin_agegroups_summarysd2$sd <- sqrt(Albumin_agegroups_summarysd2$lbdsalsi)


# View the results
print(Albumin_agegroups_summary2)
Albumin_agegroups_summarysd2
Albumin_agegroups2_range



# Weighted Frequency table for Race
svytable(~RACE_RECODE, nhanesDesign)

#Mexican American 	122268531	8.30%
#Non-Hispanic Black 	158420633	10.76%
#Non-Hispanic White 	1012045779	68.74%
#Other Hispanic 	80102460	5.44%
#Other Race - Including Multi-Racial 	99405741	6.75%

# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Albumin_race <- svyby(~lbdsalsi, ~factor(RACE_RECODE), design = nhanesDesign, svymean)
Albumin_race_sd <- svyby(~lbdsalsi, ~factor(RACE_RECODE), design = nhanesDesign, svyvar)
Albumin_race_range <- svyby(~lbdsalsi, ~RACE_RECODE, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_race_summary <- data.frame(Albumin_race)
Albumin_race_summarysd<-data.frame(Albumin_race_sd)
Albumin_race_summarysd$sd <- sqrt(Albumin_race_summarysd$lbdsalsi)


# View the results
print(Albumin_race_summary)
Albumin_race_summarysd
Albumin_race_range


# Weighted Frequency table for Sex

svytable(~riagendr, nhanesDesign)
# Male 711157980 48.3%
# Female 761085164 51.7%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Albumin_sex <- svyby(~lbdsalsi, ~factor(riagendr), design = nhanesDesign, svymean)
Albumin_sex_sd <- svyby(~lbdsalsi, ~factor(riagendr), design = nhanesDesign, svyvar)
Albumin_sex_range <- svyby(~lbdsalsi, ~riagendr, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_sex_summary <- data.frame(Albumin_sex)
Albumin_sex_summarysd<-data.frame(Albumin_sex_sd)
Albumin_sex_summarysd$sd <- sqrt(Albumin_sex_summarysd$lbdsalsi)


# View the results
print(Albumin_sex_summary)
Albumin_sex_summarysd
Albumin_sex_range


# Weighted Frequency table for BMI groups
svytable(~BMI_Groups, nhanesDesign)
#<20	73972563	5.13%
#20-25	384449356	26.68%
#25-30	476812880	33.08%
#30-35	287313865	19.94%
#35-40	128785256	8.94%
#40 or higher	89820361	6.23%

# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Albumin_BMI_Groups <- svyby(~lbdsalsi, ~factor(BMI_Groups), design = nhanesDesign, svymean)
Albumin_BMI_Groups_sd <- svyby(~lbdsalsi, ~factor(BMI_Groups), design = nhanesDesign, svyvar)
Albumin_BMI_Groups_range <- svyby(~lbdsalsi, ~BMI_Groups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_BMI_Groups_summary <- data.frame(Albumin_BMI_Groups)
Albumin_BMI_Groups_summarysd<-data.frame(Albumin_BMI_Groups_sd)
Albumin_BMI_Groups_summarysd$sd <- sqrt(Albumin_BMI_Groups_summarysd$lbdsalsi)

# View the results
print(Albumin_BMI_Groups_summary)
Albumin_BMI_Groups_summarysd
Albumin_BMI_Groups_range



# Weighted Frequency table for Education
svytable(~dmdeduc2, nhanesDesign)
# Less than 9th grade (N=4651)	87898201.3	6.066%
# 9-11th grade (Includes 12th grade with no diploma) (N=5402)	159491596.8	11.006%
# High school graduate/GED or equivalent (N=8537)	358733876	24.756%
# Some college or AA degree (N=10183)	444474864	30.673%
# College graduate or above (N=7559)	396998727.5	27.396%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Albumin_Education <- svyby(~lbdsalsi, ~factor(dmdeduc2), design = nhanesDesign, svymean)
Albumin_Education_sd <- svyby(~lbdsalsi, ~factor(dmdeduc2), design = nhanesDesign, svyvar)
Albumin_Education_range <- svyby(~lbdsalsi, ~dmdeduc2, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_Education_summary <- data.frame(Albumin_Education)
Albumin_Education_summarysd<-data.frame(Albumin_Education_sd)
Albumin_Education_summarysd$sd <- sqrt(Albumin_Education_summarysd$lbdsalsi)


# View the results
print(Albumin_Education_summary)
Albumin_Education_summarysd
Albumin_Education_range



# Weighted Frequency table for Income
table_weighted_income<-svytable(~familyincomeannual, nhanesDesign)
tabledataframe<-data.frame(table_weighted_income)
total_freq <- sum(tabledataframe$Freq)
tabledataframe$Percentage <- (tabledataframe$Freq/total_freq)*100
tabledataframe$Percentage <- round(tabledataframe$Percentage, 2)

#   familyincomeannual      Freq Percentage

#             $ <5000  28398917       2.14%
#         10000-15000  92076718       6.92%
#          15000-2000  83075022       6.25%
#         20000-25000 108780678       8.18%
#         25000-35000 136452422      10.26%
#         35000-45000 145722842      10.96%
#         45000-55000 131348748       9.87%
#          5000-10000  48284605       3.63%
#        55000-65000 101404308       7.62%
#        65000-75000  88542639       6.66%
#       75000-100000  70246203       5.28%
#             >100000 295801899      22.24%
Albumin_familyincomeannual <- svyby(~lbdsalsi, ~factor(FamilyIncomeRecode), design = nhanesDesign, svymean)
Albumin_familyincomeannual_sd <- svyby(~lbdsalsi, ~factor(FamilyIncomeRecode), design = nhanesDesign, svyvar)
Albumin_familyincomeannual_range <- svyby(~lbdsalsi, ~FamilyIncomeRecode, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_familyincomeannual_summary <- data.frame(Albumin_familyincomeannual)
Albumin_familyincomeannual_summarysd<-data.frame(Albumin_familyincomeannual_sd)
Albumin_familyincomeannual_summarysd$sd <- sqrt(Albumin_familyincomeannual_summarysd$lbdsalsi)


# View the results
print(Albumin_familyincomeannual_summary)
Albumin_familyincomeannual_summarysd
Albumin_familyincomeannual_range



# Weighted Frequency table for Alcohol
table_weighted_alcohol<-svytable(~Alcohol_Any_Categories, nhanesDesign)
tabledataframe_alcohol<-data.frame(table_weighted_alcohol)
total_freq <- sum(tabledataframe_alcohol$Freq)
tabledataframe_alcohol$Percentage <- (tabledataframe_alcohol$Freq/total_freq)*100
tabledataframe_alcohol$Percentage <- round(tabledataframe_alcohol$Percentage, 2) 
tabledataframe_alcohol
# Comparison of a numeric variable by a grouping variable
# Alcohol_Any_Categories      Freq Percentage
#          Not a Drinker 249130415      25.33
#         Heavy Drinker 133341354      13.56
#        Regular Drinker 601083385      61.11
Albumin_Alcohol <- svyby(~lbdsalsi, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svymean)
Albumin_Alcohol_sd <- svyby(~lbdsalsi, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svyvar)
Albumin_Alcohol_range <- svyby(~lbdsalsi, ~Alcohol_Any_Categories, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_Alcohol_summary <- data.frame(Albumin_Alcohol)
Albumin_Alcohol_summarysd<-data.frame(Albumin_Alcohol_sd)
Albumin_Alcohol_summarysd$sd <- sqrt(Albumin_Alcohol_summarysd$lbdsalsi)


# View the results
print(Albumin_Alcohol_summary)
Albumin_Alcohol_summarysd
Albumin_Alcohol_range



# Weighted Frequency table for Smoking
table_weighted_SmokingStatus<-svytable(~SmokingStatus, nhanesDesign)
tabledataframe_SmokingStatus<-data.frame(table_weighted_SmokingStatus)
total_freq <- sum(tabledataframe_SmokingStatus$Freq)
tabledataframe_SmokingStatus$Percentage <- (tabledataframe_SmokingStatus$Freq/total_freq)*100
tabledataframe_SmokingStatus$Percentage <- round(tabledataframe_SmokingStatus$Percentage, 2) 
tabledataframe_SmokingStatus
#    SmokingStatus  Freq Percentage
# Current Smoker 316496128      21.76%
#  Former Smoker 362187774      24.90%
#   Never Smoker 775650383      53.3%
Albumin_SmokingStatus <- svyby(~lbdsalsi, ~factor(SmokingStatus), design = nhanesDesign, svymean)
Albumin_SmokingStatus_sd <- svyby(~lbdsalsi, ~factor(SmokingStatus), design = nhanesDesign, svyvar)
Albumin_SmokingStatus_range <- svyby(~lbdsalsi, ~SmokingStatus, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_SmokingStatus_summary <- data.frame(Albumin_SmokingStatus)
Albumin_SmokingStatus_summarysd<-data.frame(Albumin_SmokingStatus_sd)
Albumin_SmokingStatus_summarysd$sd <- sqrt(Albumin_SmokingStatus_summarysd$lbdsalsi)

# View the results
print(Albumin_SmokingStatus_summary)
Albumin_SmokingStatus_summarysd
Albumin_SmokingStatus_range


# Weighted Frequency table for Diabetes
table_weighted_Diabetes<-svytable(~Diabetes, nhanesDesign)
tabledataframe_Diabetes<-data.frame(table_weighted_Diabetes)
total_freq <- sum(tabledataframe_Diabetes$Freq)
tabledataframe_Diabetes$Percentage <- (tabledataframe_Diabetes$Freq/total_freq)*100
tabledataframe_Diabetes$Percentage <- round(tabledataframe_Diabetes$Percentage, 2) 
tabledataframe_Diabetes

#Diabetes       Freq Percentage
#                No 1322298535      91.39%
#               Yes  124521799       8.61%
Albumin_Diabetes <- svyby(~lbdsalsi, ~factor(Diabetes), design = nhanesDesign, svymean)
Albumin_Diabetes_sd <- svyby(~lbdsalsi, ~factor(Diabetes), design = nhanesDesign, svyvar)
Albumin_Diabetes_range <- svyby(~lbdsalsi, ~Diabetes, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation int

Albumin_Diabetes_summary <- data.frame(Albumin_Diabetes)
Albumin_Diabetes_summarysd<-data.frame(Albumin_Diabetes_sd)
Albumin_Diabetes_summarysd$sd <- sqrt(Albumin_Diabetes_summarysd$lbdsalsi)

# View the results
print(Albumin_Diabetes_summary)
Albumin_Diabetes_summarysd
Albumin_Diabetes_range



# Weighted Frequency table for Hypertension
table_weighted_Hypertension<-svytable(~Hypertension, nhanesDesign)
tabledataframe_Hypertension<-data.frame(table_weighted_Hypertension)
total_freq <- sum(tabledataframe_Hypertension$Freq)
tabledataframe_Hypertension$Percentage <- (tabledataframe_Hypertension$Freq/total_freq)*100
tabledataframe_Hypertension$Percentage <- round(tabledataframe_Hypertension$Percentage, 2) 
tabledataframe_Hypertension
#Hypertension       Freq Percentage
#1                      0 1125223737      84.25
#2                      1  210355103      15.75

Albumin_Hypertension <- svyby(~lbdsalsi, ~factor(Hypertension), design = nhanesDesign, svymean)
Albumin_Hypertension_sd <- svyby(~lbdsalsi, ~factor(Hypertension), design = nhanesDesign, svyvar)
Albumin_Hypertension_range <- svyby(~lbdsalsi, ~Hypertension, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_Hypertension_summary <- data.frame(Albumin_Hypertension)
Albumin_Hypertension_summarysd<-data.frame(Albumin_Hypertension_sd)
Albumin_Hypertension_summarysd$sd <- sqrt(Albumin_Hypertension_summarysd$lbdsalsi)

# View the results
print(Albumin_Hypertension_summary)
Albumin_Hypertension_summarysd
Albumin_Hypertension_range



# Weighted Frequency table for Dyslipidemia
table_weighted_Dyslipidemia<-svytable(~Dyslipidemia, nhanesDesign)
tabledataframe_Dyslipidemia<-data.frame(table_weighted_Dyslipidemia)
total_freq <- sum(tabledataframe_Dyslipidemia$Freq)
tabledataframe_Dyslipidemia$Percentage <- (tabledataframe_Dyslipidemia$Freq/total_freq)*100
tabledataframe_Dyslipidemia$Percentage <- round(tabledataframe_Dyslipidemia$Percentage, 2) 
tabledataframe_Dyslipidemia
#Dyslipidemia       Freq Percentage
#1            0 1213253354      82.45
#2            1  258237153      17.55

Albumin_Dyslipidemia <- svyby(~lbdsalsi, ~factor(Dyslipidemia), design = nhanesDesign, svymean)
Albumin_Dyslipidemia_sd <- svyby(~lbdsalsi, ~factor(Dyslipidemia), design = nhanesDesign, svyvar)
Albumin_Dyslipidemia_range <- svyby(~lbdsalsi, ~Dyslipidemia, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_Dyslipidemia_summary <- data.frame(Albumin_Dyslipidemia)
Albumin_Dyslipidemia_summarysd<-data.frame(Albumin_Dyslipidemia_sd)
Albumin_Dyslipidemia_summarysd$sd <- sqrt(Albumin_Dyslipidemia_summarysd$lbdsalsi)

#Print
Albumin_Dyslipidemia_summary
Albumin_Dyslipidemia_summarysd
Albumin_Dyslipidemia_range



# Weighted Frequency table for Cardiac Disease
table_weighted_HeartDisease<-svytable(~HeartDisease, nhanesDesign)
tabledataframe_HeartDisease<-data.frame(table_weighted_HeartDisease)
total_freq <- sum(tabledataframe_HeartDisease$Freq)
tabledataframe_HeartDisease$Percentage <- (tabledataframe_HeartDisease$Freq/total_freq)*100
tabledataframe_HeartDisease$Percentage <- round(tabledataframe_HeartDisease$Percentage, 2) 
tabledataframe_HeartDisease
#HeartDisease       Freq Percentage
#1            0 1352032083      93.3
#2            1  97031973      6.7

Albumin_HeartDisease <- svyby(~lbdsalsi, ~factor(HeartDisease), design = nhanesDesign, svymean)
Albumin_HeartDisease_sd <- svyby(~lbdsalsi, ~factor(HeartDisease), design = nhanesDesign, svyvar)
Albumin_HeartDisease_range <- svyby(~lbdsalsi, ~HeartDisease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_HeartDisease_summary <- data.frame(Albumin_HeartDisease)
Albumin_HeartDisease_summarysd<-data.frame(Albumin_HeartDisease_sd)
Albumin_HeartDisease_summarysd$sd <- sqrt(Albumin_HeartDisease_summarysd$lbdsalsi)

#Print
Albumin_HeartDisease_summary
Albumin_HeartDisease_summarysd
Albumin_HeartDisease_range


# Weighted Frequency table for Cancer
table_weighted_Cancer<-svytable(~mcq220, nhanesDesign)
tabledataframe_Cancer<-data.frame(table_weighted_Cancer)
total_freq <- sum(tabledataframe_Cancer$Freq)
tabledataframe_Cancer$Percentage <- (tabledataframe_Cancer$Freq/total_freq)*100
tabledataframe_Cancer$Percentage <- round(tabledataframe_Cancer$Percentage, 2) 
tabledataframe_Cancer
#Cancer       Freq Percentage
#1            0 1314831945      9.15
#2            1  132562783      90.74

Albumin_Cancer <- svyby(~lbdsalsi, ~factor(mcq220), design = nhanesDesign, svymean)
Albumin_Cancer_sd <- svyby(~lbdsalsi, ~factor(mcq220), design = nhanesDesign, svyvar)
Albumin_Cancer_range <- svyby(~lbdsalsi, ~mcq220, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_Cancer_summary <- data.frame(Albumin_Cancer)
Albumin_Cancer_summarysd<-data.frame(Albumin_Cancer_sd)
Albumin_Cancer_summarysd$sd <- sqrt(Albumin_Cancer_summarysd$lbdsalsi)

#Print
Albumin_Cancer_summary
Albumin_Cancer_summarysd
Albumin_Cancer_range



# Weighted Frequency table for Any_Pulmonary_Disease
table_weighted_Any_Pulmonary_Disease<-svytable(~Any_Pulmonary_Disease, nhanesDesign)
tabledataframe_Any_Pulmonary_Disease<-data.frame(table_weighted_Any_Pulmonary_Disease)
total_freq <- sum(tabledataframe_Any_Pulmonary_Disease$Freq)
tabledataframe_Any_Pulmonary_Disease$Percentage <- (tabledataframe_Any_Pulmonary_Disease$Freq/total_freq)*100
tabledataframe_Any_Pulmonary_Disease$Percentage <- round(tabledataframe_Any_Pulmonary_Disease$Percentage, 2) 
tabledataframe_Any_Pulmonary_Disease
#Any_Pulmonary_Disease       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Albumin_Any_Pulmonary_Disease <- svyby(~lbdsalsi, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svymean)
Albumin_Any_Pulmonary_Disease_sd <- svyby(~lbdsalsi, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svyvar)
Albumin_Any_Pulmonary_Disease_range <- svyby(~lbdsalsi, ~Any_Pulmonary_Disease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_Any_Pulmonary_Disease_summary <- data.frame(Albumin_Any_Pulmonary_Disease)
Albumin_Any_Pulmonary_Disease_summarysd<-data.frame(Albumin_Any_Pulmonary_Disease_sd)
Albumin_Any_Pulmonary_Disease_summarysd$sd <- sqrt(Albumin_Any_Pulmonary_Disease_summarysd$lbdsalsi)

#Print
Albumin_Any_Pulmonary_Disease_summary
Albumin_Any_Pulmonary_Disease_summarysd
Albumin_Any_Pulmonary_Disease_range




# Weighted Frequency table for VIGOROUS_ACTIVITY
table_weighted_VIGOROUS_ACTIVITY<-svytable(~VIGOROUS_ACTIVITY, nhanesDesign)
tabledataframe_VIGOROUS_ACTIVITY<-data.frame(table_weighted_VIGOROUS_ACTIVITY)
total_freq <- sum(tabledataframe_VIGOROUS_ACTIVITY$Freq)
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- (tabledataframe_VIGOROUS_ACTIVITY$Freq/total_freq)*100
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- round(tabledataframe_VIGOROUS_ACTIVITY$Percentage, 2) 
tabledataframe_VIGOROUS_ACTIVITY
#VIGOROUS_ACTIVITY       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Albumin_VIGOROUS_ACTIVITY <- svyby(~lbdsalsi, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svymean)
Albumin_VIGOROUS_ACTIVITY_sd <- svyby(~lbdsalsi, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svyvar)
Albumin_VIGOROUS_ACTIVITY_range <- svyby(~lbdsalsi, ~VIGOROUS_ACTIVITY, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Albumin_VIGOROUS_ACTIVITY_summary <- data.frame(Albumin_VIGOROUS_ACTIVITY)
Albumin_VIGOROUS_ACTIVITY_summarysd<-data.frame(Albumin_VIGOROUS_ACTIVITY_sd)
Albumin_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(Albumin_VIGOROUS_ACTIVITY_summarysd$lbdsalsi)

# Print
Albumin_VIGOROUS_ACTIVITY_summary
Albumin_VIGOROUS_ACTIVITY_summarysd
Albumin_VIGOROUS_ACTIVITY_range





### Creatinine
# Calculate the mean and standard deviation of Creatinine by age_deciles
Creatinine_agegroups <- svyby(~lbdscrsi, ~factor(age_deciles), design = nhanesDesign, svymean)
Creatinine_agegroups_sd <- svyby(~lbdscrsi, ~factor(age_deciles), design = nhanesDesign, svyvar)
Creatinine_agegroups_range <- svyby(~lbdscrsi, ~age_deciles, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_agegroups_summary <- data.frame(Creatinine_agegroups)
Creatinine_agegroups_summarysd<-data.frame(Creatinine_agegroups_sd)
Creatinine_agegroups_summarysd$sd <- sqrt(Creatinine_agegroups_summarysd$lbdscrsi)


# View the results
print(Creatinine_agegroups_summary)
Creatinine_agegroups_summarysd
Creatinine_agegroups_range



# Calculate the mean and standard deviation of Creatinine by age_deciles
Creatinine_race <- svyby(~lbdscrsi, ~factor(RACE_RECODE), design = nhanesDesign, svymean)
Creatinine_race_sd <- svyby(~lbdscrsi, ~factor(RACE_RECODE), design = nhanesDesign, svyvar)
Creatinine_race_range <- svyby(~lbdscrsi, ~RACE_RECODE, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_race_summary <- data.frame(Creatinine_race)
Creatinine_race_summarysd<-data.frame(Creatinine_race_sd)
Creatinine_race_summarysd$sd <- sqrt(Creatinine_race_summarysd$lbdscrsi)

#Print
Creatinine_race_summary
Creatinine_race_summarysd
Creatinine_race_range



# Calculate the mean and standard deviation of Creatinine by age_deciles
Creatinine_sex <- svyby(~lbdscrsi, ~factor(riagendr), design = nhanesDesign, svymean)
Creatinine_sex_sd <- svyby(~lbdscrsi, ~factor(riagendr), design = nhanesDesign, svyvar)
Creatinine_sex_range <- svyby(~lbdscrsi, ~riagendr, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_sex_summary <- data.frame(Creatinine_sex)
Creatinine_sex_summarysd<-data.frame(Creatinine_sex_sd)
Creatinine_sex_summarysd$sd <- sqrt(Creatinine_sex_summarysd$lbdscrsi)

#Print
Creatinine_sex_summary
Creatinine_sex_summarysd
Creatinine_sex_range



# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Creatinine_BMI_Groups <- svyby(~lbdscrsi, ~factor(BMI_Groups), design = nhanesDesign, svymean)
Creatinine_BMI_Groups_sd <- svyby(~lbdscrsi, ~factor(BMI_Groups), design = nhanesDesign, svyvar)
Creatinine_BMI_Groups_range <- svyby(~lbdscrsi, ~BMI_Groups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_BMI_Groups_summary <- data.frame(Creatinine_BMI_Groups)
Creatinine_BMI_Groups_summarysd<-data.frame(Creatinine_BMI_Groups_sd)
Creatinine_BMI_Groups_summarysd$sd <- sqrt(Creatinine_BMI_Groups_summarysd$lbdscrsi)

#Print
Creatinine_BMI_Groups_summary
Creatinine_BMI_Groups_summarysd
Creatinine_BMI_Groups_range


# Weighted Frequency table for Education
svytable(~dmdeduc2, nhanesDesign)
# Less than 9th grade (N=4651)	87898201.3	6.066%
# 9-11th grade (Includes 12th grade with no diploma) (N=5402)	159491596.8	11.006%
# High school graduate/GED or equivalent (N=8537)	358733876	24.756%
# Some college or AA degree (N=10183)	444474864	30.673%
# College graduate or above (N=7559)	396998727.5	27.396%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Creatinine_Education <- svyby(~lbdscrsi, ~factor(dmdeduc2), design = nhanesDesign, svymean)
Creatinine_Education_sd <- svyby(~lbdscrsi, ~factor(dmdeduc2), design = nhanesDesign, svyvar)
Creatinine_Education_range <- svyby(~lbdscrsi, ~dmdeduc2, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_Education_summary <- data.frame(Creatinine_Education)
Creatinine_Education_summarysd<-data.frame(Creatinine_Education_sd)
Creatinine_Education_summarysd$sd <- sqrt(Creatinine_Education_summarysd$lbdscrsi)

# View the results
print(Creatinine_Education_summary)
Creatinine_Education_summarysd
Creatinine_Education_range


# Weighted Frequency table for Income
table_weighted_income<-svytable(~familyincomeannual, nhanesDesign)
tabledataframe<-data.frame(table_weighted_income)
total_freq <- sum(tabledataframe$Freq)
tabledataframe$Percentage <- (tabledataframe$Freq/total_freq)*100
tabledataframe$Percentage <- round(tabledataframe$Percentage, 2)

#   familyincomeannual      Freq Percentage

#             $ <5000  28398917       2.14%
#         10000-15000  92076718       6.92%
#          15000-2000  83075022       6.25%
#         20000-25000 108780678       8.18%
#         25000-35000 136452422      10.26%
#         35000-45000 145722842      10.96%
#         45000-55000 131348748       9.87%
#          5000-10000  48284605       3.63%
#        55000-65000 101404308       7.62%
#        65000-75000  88542639       6.66%
#       75000-100000  70246203       5.28%
#             >100000 295801899      22.24%
Creatinine_familyincomeannual <- svyby(~lbdscrsi, ~factor(FamilyIncomeRecode), design = nhanesDesign, svymean)
Creatinine_familyincomeannual_sd <- svyby(~lbdscrsi, ~factor(FamilyIncomeRecode), design = nhanesDesign, svyvar)
Creatinine_familyincomeannual_range <- svyby(~lbdscrsi, ~FamilyIncomeRecode, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_familyincomeannual_summary <- data.frame(Creatinine_familyincomeannual)
Creatinine_familyincomeannual_summarysd<-data.frame(Creatinine_familyincomeannual_sd)
Creatinine_familyincomeannual_summarysd$sd <- sqrt(Creatinine_familyincomeannual_summarysd$lbdscrsi)

# View the results
print(Creatinine_familyincomeannual_summary)
Creatinine_familyincomeannual_summarysd
Creatinine_familyincomeannual_range


# Weighted Frequency table for Alcohol
table_weighted_alcohol<-svytable(~Alcohol_Any_Categories, nhanesDesign)
tabledataframe_alcohol<-data.frame(table_weighted_alcohol)
total_freq <- sum(tabledataframe_alcohol$Freq)
tabledataframe_alcohol$Percentage <- (tabledataframe_alcohol$Freq/total_freq)*100
tabledataframe_alcohol$Percentage <- round(tabledataframe_alcohol$Percentage, 2) 
tabledataframe_alcohol
# Comparison of a numeric variable by a grouping variable
# Alcohol_Any_Categories      Freq Percentage
#          Not a Drinker 249130415      25.33
#         Heavy Drinker 133341354      13.56
#        Regular Drinker 601083385      61.11
Creatinine_Alcohol <- svyby(~lbdscrsi, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svymean)
Creatinine_Alcohol_sd <- svyby(~lbdscrsi, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svyvar)
Creatinine_Alcohol_range <- svyby(~lbdscrsi, ~Alcohol_Any_Categories, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_Alcohol_summary <- data.frame(Creatinine_Alcohol)
Creatinine_Alcohol_summarysd<-data.frame(Creatinine_Alcohol_sd)
Creatinine_Alcohol_summarysd$sd <- sqrt(Creatinine_Alcohol_summarysd$lbdscrsi)

# View the results
print(Creatinine_Alcohol_summary)
Creatinine_Alcohol_summarysd
Creatinine_Alcohol_range



# Weighted Frequency table for Smoking
table_weighted_SmokingStatus<-svytable(~SmokingStatus, nhanesDesign)
tabledataframe_SmokingStatus<-data.frame(table_weighted_SmokingStatus)
total_freq <- sum(tabledataframe_SmokingStatus$Freq)
tabledataframe_SmokingStatus$Percentage <- (tabledataframe_SmokingStatus$Freq/total_freq)*100
tabledataframe_SmokingStatus$Percentage <- round(tabledataframe_SmokingStatus$Percentage, 2) 
tabledataframe_SmokingStatus
#    SmokingStatus  Freq Percentage
# Current Smoker 316496128      21.76%
#  Former Smoker 362187774      24.90%
#   Never Smoker 775650383      53.3%
Creatinine_SmokingStatus <- svyby(~lbdscrsi, ~factor(SmokingStatus), design = nhanesDesign, svymean)
Creatinine_SmokingStatus_sd <- svyby(~lbdscrsi, ~factor(SmokingStatus), design = nhanesDesign, svyvar)
Creatinine_SmokingStatus_range <- svyby(~lbdscrsi, ~SmokingStatus, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_SmokingStatus_summary <- data.frame(Creatinine_SmokingStatus)
Creatinine_SmokingStatus_summarysd<-data.frame(Creatinine_SmokingStatus_sd)
Creatinine_SmokingStatus_summarysd$sd <- sqrt(Creatinine_SmokingStatus_summarysd$lbdscrsi)


# View the results
print(Creatinine_SmokingStatus_summary)


# Weighted Frequency table for Diabetes
table_weighted_Diabetes<-svytable(~Diabetes, nhanesDesign)
tabledataframe_Diabetes<-data.frame(table_weighted_Diabetes)
total_freq <- sum(tabledataframe_Diabetes$Freq)
tabledataframe_Diabetes$Percentage <- (tabledataframe_Diabetes$Freq/total_freq)*100
tabledataframe_Diabetes$Percentage <- round(tabledataframe_Diabetes$Percentage, 2) 
tabledataframe_Diabetes

#Diabetes       Freq Percentage
#                No 1322298535      91.39%
#               Yes  124521799       8.61%
Creatinine_Diabetes <- svyby(~lbdscrsi, ~factor(Diabetes), design = nhanesDesign, svymean)
Creatinine_Diabetes_sd <- svyby(~lbdscrsi, ~factor(Diabetes), design = nhanesDesign, svyvar)
Creatinine_Diabetes_range <- svyby(~lbdscrsi, ~Diabetes, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))


Creatinine_Diabetes_summary <- data.frame(Creatinine_Diabetes)
Creatinine_Diabetes_summarysd<-data.frame(Creatinine_Diabetes_sd)
Creatinine_Diabetes_summarysd$sd <- sqrt(Creatinine_Diabetes_summarysd$lbdscrsi)

# View the results
print(Creatinine_Diabetes_summary)
Creatinine_Diabetes_summarysd
Creatinine_Diabetes_range


# Weighted Frequency table for Hypertension
table_weighted_Hypertension<-svytable(~Hypertension, nhanesDesign)
tabledataframe_Hypertension<-data.frame(table_weighted_Hypertension)
total_freq <- sum(tabledataframe_Hypertension$Freq)
tabledataframe_Hypertension$Percentage <- (tabledataframe_Hypertension$Freq/total_freq)*100
tabledataframe_Hypertension$Percentage <- round(tabledataframe_Hypertension$Percentage, 2) 
tabledataframe_Hypertension
#Hypertension       Freq Percentage
#1                      0 1125223737      84.25
#2                      1  210355103      15.75

Creatinine_Hypertension <- svyby(~lbdscrsi, ~factor(Hypertension), design = nhanesDesign, svymean)
Creatinine_Hypertension_sd <- svyby(~lbdscrsi, ~factor(Hypertension), design = nhanesDesign, svyvar)
Creatinine_Hypertension_range <- svyby(~lbdscrsi, ~Hypertension, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_Hypertension_summary <- data.frame(Creatinine_Hypertension)
Creatinine_Hypertension_summarysd<-data.frame(Creatinine_Hypertension_sd)
Creatinine_Hypertension_summarysd$sd <- sqrt(Creatinine_Hypertension_summarysd$lbdscrsi)

# View the results
print(Creatinine_Hypertension_summary)
Creatinine_Hypertension_summarysd
Creatinine_Hypertension_range


# Weighted Frequency table for Dyslipidemia
table_weighted_Dyslipidemia<-svytable(~Dyslipidemia, nhanesDesign)
tabledataframe_Dyslipidemia<-data.frame(table_weighted_Dyslipidemia)
total_freq <- sum(tabledataframe_Dyslipidemia$Freq)
tabledataframe_Dyslipidemia$Percentage <- (tabledataframe_Dyslipidemia$Freq/total_freq)*100
tabledataframe_Dyslipidemia$Percentage <- round(tabledataframe_Dyslipidemia$Percentage, 2) 
tabledataframe_Dyslipidemia
#Dyslipidemia       Freq Percentage
#1            0 1213253354      82.45
#2            1  258237153      17.55

Creatinine_Dyslipidemia <- svyby(~lbdscrsi, ~factor(Dyslipidemia), design = nhanesDesign, svymean)
Creatinine_Dyslipidemia_sd <- svyby(~lbdscrsi, ~factor(Dyslipidemia), design = nhanesDesign, svyvar)
Creatinine_Dyslipidemia_range <- svyby(~lbdscrsi, ~Dyslipidemia, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_Dyslipidemia_summary <- data.frame(Creatinine_Dyslipidemia)
Creatinine_Dyslipidemia_summarysd<-data.frame(Creatinine_Dyslipidemia_sd)
Creatinine_Dyslipidemia_summarysd$sd <- sqrt(Creatinine_Dyslipidemia_summarysd$lbdscrsi)

#Print
Creatinine_Dyslipidemia_summary
Creatinine_Dyslipidemia_summarysd
Creatinine_Dyslipidemia_range


# Weighted Frequency table for Cardiac Disease
table_weighted_HeartDisease<-svytable(~HeartDisease, nhanesDesign)
tabledataframe_HeartDisease<-data.frame(table_weighted_HeartDisease)
total_freq <- sum(tabledataframe_HeartDisease$Freq)
tabledataframe_HeartDisease$Percentage <- (tabledataframe_HeartDisease$Freq/total_freq)*100
tabledataframe_HeartDisease$Percentage <- round(tabledataframe_HeartDisease$Percentage, 2) 
tabledataframe_HeartDisease
#HeartDisease       Freq Percentage
#1            0 1352032083      93.3
#2            1  97031973      6.7

Creatinine_HeartDisease <- svyby(~lbdscrsi, ~factor(HeartDisease), design = nhanesDesign, svymean)
Creatinine_HeartDisease_sd <- svyby(~lbdscrsi, ~factor(HeartDisease), design = nhanesDesign, svyvar)
Creatinine_HeartDisease_range <- svyby(~lbdscrsi, ~HeartDisease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_HeartDisease_summary <- data.frame(Creatinine_HeartDisease)
Creatinine_HeartDisease_summarysd<-data.frame(Creatinine_HeartDisease_sd)
Creatinine_HeartDisease_summarysd$sd <- sqrt(Creatinine_HeartDisease_summarysd$lbdscrsi)

#Print
Creatinine_HeartDisease_summary
Creatinine_HeartDisease_summarysd
Creatinine_HeartDisease_range



# Weighted Frequency table for Cancer
table_weighted_Cancer<-svytable(~mcq220, nhanesDesign)
tabledataframe_Cancer<-data.frame(table_weighted_Cancer)
total_freq <- sum(tabledataframe_Cancer$Freq)
tabledataframe_Cancer$Percentage <- (tabledataframe_Cancer$Freq/total_freq)*100
tabledataframe_Cancer$Percentage <- round(tabledataframe_Cancer$Percentage, 2) 
tabledataframe_Cancer
#Cancer       Freq Percentage
#1            0 1314831945      9.15
#2            1  132562783      90.74

Creatinine_Cancer <- svyby(~lbdscrsi, ~factor(mcq220), design = nhanesDesign, svymean)
Creatinine_Cancer_sd <- svyby(~lbdscrsi, ~factor(mcq220), design = nhanesDesign, svyvar)
Creatinine_Cancer_range <- svyby(~lbdscrsi, ~mcq220, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_Cancer_summary <- data.frame(Creatinine_Cancer)
Creatinine_Cancer_summarysd<-data.frame(Creatinine_Cancer_sd)
Creatinine_Cancer_summarysd$sd <- sqrt(Creatinine_Cancer_summarysd$lbdscrsi)

#Print
Creatinine_Cancer_summary
Creatinine_Cancer_summarysd
Creatinine_Cancer_range


# Weighted Frequency table for Any_Pulmonary_Disease
table_weighted_Any_Pulmonary_Disease<-svytable(~Any_Pulmonary_Disease, nhanesDesign)
tabledataframe_Any_Pulmonary_Disease<-data.frame(table_weighted_Any_Pulmonary_Disease)
total_freq <- sum(tabledataframe_Any_Pulmonary_Disease$Freq)
tabledataframe_Any_Pulmonary_Disease$Percentage <- (tabledataframe_Any_Pulmonary_Disease$Freq/total_freq)*100
tabledataframe_Any_Pulmonary_Disease$Percentage <- round(tabledataframe_Any_Pulmonary_Disease$Percentage, 2) 
tabledataframe_Any_Pulmonary_Disease
#Any_Pulmonary_Disease       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Creatinine_Any_Pulmonary_Disease <- svyby(~lbdscrsi, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svymean)
Creatinine_Any_Pulmonary_Disease_sd <- svyby(~lbdscrsi, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svyvar)
Creatinine_Any_Pulmonary_Disease_range <- svyby(~lbdscrsi, ~Any_Pulmonary_Disease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_Any_Pulmonary_Disease_summary <- data.frame(Creatinine_Any_Pulmonary_Disease)
Creatinine_Any_Pulmonary_Disease_summarysd<-data.frame(Creatinine_Any_Pulmonary_Disease_sd)
Creatinine_Any_Pulmonary_Disease_summarysd$sd <- sqrt(Creatinine_Any_Pulmonary_Disease_summarysd$lbdscrsi)

# Print
Creatinine_Any_Pulmonary_Disease_summary
Creatinine_Any_Pulmonary_Disease_summarysd
Creatinine_Any_Pulmonary_Disease_range



# Weighted Frequency table for VIGOROUS_ACTIVITY
table_weighted_VIGOROUS_ACTIVITY<-svytable(~VIGOROUS_ACTIVITY, nhanesDesign)
tabledataframe_VIGOROUS_ACTIVITY<-data.frame(table_weighted_VIGOROUS_ACTIVITY)
total_freq <- sum(tabledataframe_VIGOROUS_ACTIVITY$Freq)
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- (tabledataframe_VIGOROUS_ACTIVITY$Freq/total_freq)*100
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- round(tabledataframe_VIGOROUS_ACTIVITY$Percentage, 2) 
tabledataframe_VIGOROUS_ACTIVITY
#VIGOROUS_ACTIVITY       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Creatinine_VIGOROUS_ACTIVITY <- svyby(~lbdscrsi, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svymean)
Creatinine_VIGOROUS_ACTIVITY_sd <- svyby(~lbdscrsi, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svyvar)
Creatinine_VIGOROUS_ACTIVITY_range <- svyby(~lbdscrsi, ~VIGOROUS_ACTIVITY, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Creatinine_VIGOROUS_ACTIVITY_summary <- data.frame(Creatinine_VIGOROUS_ACTIVITY)
Creatinine_VIGOROUS_ACTIVITY_summarysd<-data.frame(Creatinine_VIGOROUS_ACTIVITY_sd)
Creatinine_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(Creatinine_VIGOROUS_ACTIVITY_summarysd$lbdscrsi)

# Print
Creatinine_VIGOROUS_ACTIVITY_summary
Creatinine_VIGOROUS_ACTIVITY_summarysd
Creatinine_VIGOROUS_ACTIVITY_range


### Glucose
# Calculate the mean and standard deviation of Glucose by age_deciles
Glucose_agegroups <- svyby(~lbdsglsi, ~factor(age_deciles), design = nhanesDesign, svymean)
Glucose_agegroups_sd <- svyby(~lbdsglsi, ~factor(age_deciles), design = nhanesDesign, svyvar)
Glucose_agegroups_range <- svyby(~lbdsglsi, ~age_deciles, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_agegroups_summary <- data.frame(Glucose_agegroups)
Glucose_agegroups_summarysd<-data.frame(Glucose_agegroups_sd)
Glucose_agegroups_summarysd$sd <- sqrt(Glucose_agegroups_summarysd$lbdsglsi)


# View the results
print(Glucose_agegroups_summary)
print(Glucose_agegroups_summarysd)
print(Glucose_agegroups_range)

# Calculate the mean and standard deviation of Glucose by age_deciles
Glucose_race <- svyby(~lbdsglsi, ~factor(RACE_RECODE), design = nhanesDesign, svymean)
Glucose_race_sd <- svyby(~lbdsglsi, ~factor(RACE_RECODE), design = nhanesDesign, svyvar)
Glucose_race_range <- svyby(~lbdsglsi, ~RACE_RECODE, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_race_summary <- data.frame(Glucose_race)
Glucose_race_summarysd<-data.frame(Glucose_race_sd)
Glucose_race_summarysd$sd <- sqrt(Glucose_race_summarysd$lbdsglsi)

# Print
Glucose_race_summary
Glucose_race_summarysd
Glucose_race_range


# Calculate the mean and standard deviation of Glucose by age_deciles
Glucose_sex <- svyby(~lbdsglsi, ~factor(riagendr), design = nhanesDesign, svymean)
Glucose_sex_sd <- svyby(~lbdsglsi, ~factor(riagendr), design = nhanesDesign, svyvar)
Glucose_sex_range <- svyby(~lbdsglsi, ~riagendr, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_sex_summary <- data.frame(Glucose_sex)
Glucose_sex_summarysd<-data.frame(Glucose_sex_sd)
Glucose_sex_summarysd$sd <- sqrt(Glucose_sex_summarysd$lbdsglsi)

#Print
Glucose_sex_summary
Glucose_sex_summarysd
Glucose_sex_range


# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Glucose_BMI_Groups <- svyby(~lbdsglsi, ~factor(BMI_Groups), design = nhanesDesign, svymean)
Glucose_BMI_Groups_sd <- svyby(~lbdsglsi, ~factor(BMI_Groups), design = nhanesDesign, svyvar)
Glucose_BMI_Groups_range <- svyby(~lbdsglsi, ~BMI_Groups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_BMI_Groups_summary <- data.frame(Glucose_BMI_Groups)
Glucose_BMI_Groups_summarysd<-data.frame(Glucose_BMI_Groups_sd)
Glucose_BMI_Groups_summarysd$sd <- sqrt(Glucose_BMI_Groups_summarysd$lbdsglsi)

# Print
Glucose_BMI_Groups_summary
Glucose_BMI_Groups_summarysd
Glucose_BMI_Groups_range




# Weighted Frequency table for Education
svytable(~dmdeduc2, nhanesDesign)
# Less than 9th grade (N=4651)	87898201.3	6.066%
# 9-11th grade (Includes 12th grade with no diploma) (N=5402)	159491596.8	11.006%
# High school graduate/GED or equivalent (N=8537)	358733876	24.756%
# Some college or AA degree (N=10183)	444474864	30.673%
# College graduate or above (N=7559)	396998727.5	27.396%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Glucose_Education <- svyby(~lbdsglsi, ~factor(dmdeduc2), design = nhanesDesign, svymean)
Glucose_Education_sd <- svyby(~lbdsglsi, ~factor(dmdeduc2), design = nhanesDesign, svyvar)
Glucose_Education_range <- svyby(~lbdsglsi, ~dmdeduc2, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_Education_summary <- data.frame(Glucose_Education)
Glucose_Education_summarysd<-data.frame(Glucose_Education_sd)
Glucose_Education_summarysd$sd <- sqrt(Glucose_Education_summarysd$lbdsglsi)

# View the results
print(Glucose_Education_summary)
Glucose_Education_summarysd
Glucose_Education_range



# Weighted Frequency table for Income
table_weighted_income<-svytable(~familyincomeannual, nhanesDesign)
tabledataframe<-data.frame(table_weighted_income)
total_freq <- sum(tabledataframe$Freq)
tabledataframe$Percentage <- (tabledataframe$Freq/total_freq)*100
tabledataframe$Percentage <- round(tabledataframe$Percentage, 2)

#   familyincomeannual      Freq Percentage

#             $ <5000  28398917       2.14%
#         10000-15000  92076718       6.92%
#          15000-2000  83075022       6.25%
#         20000-25000 108780678       8.18%
#         25000-35000 136452422      10.26%
#         35000-45000 145722842      10.96%
#         45000-55000 131348748       9.87%
#          5000-10000  48284605       3.63%
#        55000-65000 101404308       7.62%
#        65000-75000  88542639       6.66%
#       75000-100000  70246203       5.28%
#             >100000 295801899      22.24%
Glucose_familyincomeannual <- svyby(~lbdsglsi, ~factor(FamilyIncomeRecode), design = nhanesDesign, svymean)
Glucose_familyincomeannual_sd <- svyby(~lbdsglsi, ~factor(FamilyIncomeRecode), design = nhanesDesign, svyvar)
Glucose_familyincomeannual_range <- svyby(~lbdsglsi, ~FamilyIncomeRecode, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_familyincomeannual_summary <- data.frame(Glucose_familyincomeannual)
Glucose_familyincomeannual_summarysd<-data.frame(Glucose_familyincomeannual_sd)
Glucose_familyincomeannual_summarysd$sd <- sqrt(Glucose_familyincomeannual_summarysd$lbdsglsi)

# View the results
print(Glucose_familyincomeannual_summary)
Glucose_familyincomeannual_summarysd
Glucose_familyincomeannual_range


# Weighted Frequency table for Alcohol
table_weighted_alcohol<-svytable(~Alcohol_Any_Categories, nhanesDesign)
tabledataframe_alcohol<-data.frame(table_weighted_alcohol)
total_freq <- sum(tabledataframe_alcohol$Freq)
tabledataframe_alcohol$Percentage <- (tabledataframe_alcohol$Freq/total_freq)*100
tabledataframe_alcohol$Percentage <- round(tabledataframe_alcohol$Percentage, 2) 
tabledataframe_alcohol
# Comparison of a numeric variable by a grouping variable
# Alcohol_Any_Categories      Freq Percentage
#          Not a Drinker 249130415      25.33
#         Heavy Drinker 133341354      13.56
#        Regular Drinker 601083385      61.11

Glucose_Alcohol <- svyby(~lbdsglsi, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svymean)
Glucose_Alcohol_sd <- svyby(~lbdsglsi, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svyvar)
Glucose_Alcohol_range <- svyby(~lbdsglsi, ~Alcohol_Any_Categories, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_Alcohol_summary <- data.frame(Glucose_Alcohol)
Glucose_Alcohol_summarysd<-data.frame(Glucose_Alcohol_sd)
Glucose_Alcohol_summarysd$sd <- sqrt(Glucose_Alcohol_summarysd$lbdsglsi)

# View the results
print(Glucose_Alcohol_summary)
Glucose_Alcohol_summarysd
Glucose_Alcohol_range


# Weighted Frequency table for Smoking
table_weighted_SmokingStatus<-svytable(~SmokingStatus, nhanesDesign)
tabledataframe_SmokingStatus<-data.frame(table_weighted_SmokingStatus)
total_freq <- sum(tabledataframe_SmokingStatus$Freq)
tabledataframe_SmokingStatus$Percentage <- (tabledataframe_SmokingStatus$Freq/total_freq)*100
tabledataframe_SmokingStatus$Percentage <- round(tabledataframe_SmokingStatus$Percentage, 2) 
tabledataframe_SmokingStatus
#    SmokingStatus  Freq Percentage
# Current Smoker 316496128      21.76%
#  Former Smoker 362187774      24.90%
#   Never Smoker 775650383      53.3%
Glucose_SmokingStatus <- svyby(~lbdsglsi, ~factor(SmokingStatus), design = nhanesDesign, svymean)
Glucose_SmokingStatus_sd <- svyby(~lbdsglsi, ~factor(SmokingStatus), design = nhanesDesign, svyvar)
Glucose_SmokingStatus_range <- svyby(~lbdsglsi, ~SmokingStatus, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_SmokingStatus_summary <- data.frame(Glucose_SmokingStatus)
Glucose_SmokingStatus_summarysd<-data.frame(Glucose_SmokingStatus_sd)
Glucose_SmokingStatus_summarysd$sd <- sqrt(Glucose_SmokingStatus_summarysd$lbdsglsi)


# View the results
print(Glucose_SmokingStatus_summary)
Glucose_SmokingStatus_summarysd
Glucose_SmokingStatus_range



# Weighted Frequency table for Diabetes
table_weighted_Diabetes<-svytable(~Diabetes, nhanesDesign)
tabledataframe_Diabetes<-data.frame(table_weighted_Diabetes)
total_freq <- sum(tabledataframe_Diabetes$Freq)
tabledataframe_Diabetes$Percentage <- (tabledataframe_Diabetes$Freq/total_freq)*100
tabledataframe_Diabetes$Percentage <- round(tabledataframe_Diabetes$Percentage, 2) 
tabledataframe_Diabetes

#Diabetes       Freq Percentage
#                No 1322298535      91.39%
#               Yes  124521799       8.61%
Glucose_Diabetes <- svyby(~lbdsglsi, ~factor(Diabetes), design = nhanesDesign, svymean)
Glucose_Diabetes_sd <- svyby(~lbdsglsi, ~factor(Diabetes), design = nhanesDesign, svyvar)
Glucose_Diabetes_range <- svyby(~lbdsglsi, ~Diabetes, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation int

Glucose_Diabetes_summary <- data.frame(Glucose_Diabetes)
Glucose_Diabetes_summarysd<-data.frame(Glucose_Diabetes_sd)
Glucose_Diabetes_summarysd$sd <- sqrt(Glucose_Diabetes_summarysd$lbdsglsi)

# View the results
print(Glucose_Diabetes_summary)
Glucose_Diabetes_summarysd
Glucose_Diabetes_range



# Weighted Frequency table for Hypertension
table_weighted_Hypertension<-svytable(~Hypertension, nhanesDesign)
tabledataframe_Hypertension<-data.frame(table_weighted_Hypertension)
total_freq <- sum(tabledataframe_Hypertension$Freq)
tabledataframe_Hypertension$Percentage <- (tabledataframe_Hypertension$Freq/total_freq)*100
tabledataframe_Hypertension$Percentage <- round(tabledataframe_Hypertension$Percentage, 2) 
tabledataframe_Hypertension
#Hypertension       Freq Percentage
#1                      0 1125223737      84.25
#2                      1  210355103      15.75

Glucose_Hypertension <- svyby(~lbdsglsi, ~factor(Hypertension), design = nhanesDesign, svymean)
Glucose_Hypertension_sd <- svyby(~lbdsglsi, ~factor(Hypertension), design = nhanesDesign, svyvar)
Glucose_Hypertension_range <- svyby(~lbdsglsi, ~Hypertension, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_Hypertension_summary <- data.frame(Glucose_Hypertension)
Glucose_Hypertension_summarysd<-data.frame(Glucose_Hypertension_sd)
Glucose_Hypertension_summarysd$sd <- sqrt(Glucose_Hypertension_summarysd$lbdsglsi)

# View the results
print(Glucose_Hypertension_summary)
Glucose_Hypertension_summarysd
Glucose_Hypertension_range


# Weighted Frequency table for Dyslipidemia
table_weighted_Dyslipidemia<-svytable(~Dyslipidemia, nhanesDesign)
tabledataframe_Dyslipidemia<-data.frame(table_weighted_Dyslipidemia)
total_freq <- sum(tabledataframe_Dyslipidemia$Freq)
tabledataframe_Dyslipidemia$Percentage <- (tabledataframe_Dyslipidemia$Freq/total_freq)*100
tabledataframe_Dyslipidemia$Percentage <- round(tabledataframe_Dyslipidemia$Percentage, 2) 
tabledataframe_Dyslipidemia
#Dyslipidemia       Freq Percentage
#1            0 1213253354      82.45
#2            1  258237153      17.55

Glucose_Dyslipidemia <- svyby(~lbdsglsi, ~factor(Dyslipidemia), design = nhanesDesign, svymean)
Glucose_Dyslipidemia_sd <- svyby(~lbdsglsi, ~factor(Dyslipidemia), design = nhanesDesign, svyvar)
Glucose_Dyslipidemia_range <- svyby(~lbdsglsi, ~Dyslipidemia, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_Dyslipidemia_summary <- data.frame(Glucose_Dyslipidemia)
Glucose_Dyslipidemia_summarysd<-data.frame(Glucose_Dyslipidemia_sd)
Glucose_Dyslipidemia_summarysd$sd <- sqrt(Glucose_Dyslipidemia_summarysd$lbdsglsi)

# Print
Glucose_Dyslipidemia_summary
Glucose_Dyslipidemia_summarysd
Glucose_Dyslipidemia_range



# Weighted Frequency table for Cardiac Disease
table_weighted_HeartDisease<-svytable(~HeartDisease, nhanesDesign)
tabledataframe_HeartDisease<-data.frame(table_weighted_HeartDisease)
total_freq <- sum(tabledataframe_HeartDisease$Freq)
tabledataframe_HeartDisease$Percentage <- (tabledataframe_HeartDisease$Freq/total_freq)*100
tabledataframe_HeartDisease$Percentage <- round(tabledataframe_HeartDisease$Percentage, 2) 
tabledataframe_HeartDisease
#HeartDisease       Freq Percentage
#1            0 1352032083      93.3
#2            1  97031973      6.7

Glucose_HeartDisease <- svyby(~lbdsglsi, ~factor(HeartDisease), design = nhanesDesign, svymean)
Glucose_HeartDisease_sd <- svyby(~lbdsglsi, ~factor(HeartDisease), design = nhanesDesign, svyvar)
Glucose_HeartDisease_range <- svyby(~lbdsglsi, ~HeartDisease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_HeartDisease_summary <- data.frame(Glucose_HeartDisease)
Glucose_HeartDisease_summarysd<-data.frame(Glucose_HeartDisease_sd)
Glucose_HeartDisease_summarysd$sd <- sqrt(Glucose_HeartDisease_summarysd$lbdsglsi)

#Print
Glucose_HeartDisease_summary
Glucose_HeartDisease_summarysd
Glucose_HeartDisease_range



# Weighted Frequency table for Cancer
table_weighted_Cancer<-svytable(~mcq220, nhanesDesign)
tabledataframe_Cancer<-data.frame(table_weighted_Cancer)
total_freq <- sum(tabledataframe_Cancer$Freq)
tabledataframe_Cancer$Percentage <- (tabledataframe_Cancer$Freq/total_freq)*100
tabledataframe_Cancer$Percentage <- round(tabledataframe_Cancer$Percentage, 2) 
tabledataframe_Cancer
#Cancer       Freq Percentage
#1            0 1314831945      9.15
#2            1  132562783      90.74

Glucose_Cancer <- svyby(~lbdsglsi, ~factor(mcq220), design = nhanesDesign, svymean)
Glucose_Cancer_sd <- svyby(~lbdsglsi, ~factor(mcq220), design = nhanesDesign, svyvar)
Glucose_Cancer_range <- svyby(~lbdsglsi, ~mcq220, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_Cancer_summary <- data.frame(Glucose_Cancer)
Glucose_Cancer_summarysd<-data.frame(Glucose_Cancer_sd)
Glucose_Cancer_summarysd$sd <- sqrt(Glucose_Cancer_summarysd$lbdsglsi)

# Print
Glucose_Cancer_summary
Glucose_Cancer_summarysd
Glucose_Cancer_range



# Weighted Frequency table for Any_Pulmonary_Disease
table_weighted_Any_Pulmonary_Disease<-svytable(~Any_Pulmonary_Disease, nhanesDesign)
tabledataframe_Any_Pulmonary_Disease<-data.frame(table_weighted_Any_Pulmonary_Disease)
total_freq <- sum(tabledataframe_Any_Pulmonary_Disease$Freq)
tabledataframe_Any_Pulmonary_Disease$Percentage <- (tabledataframe_Any_Pulmonary_Disease$Freq/total_freq)*100
tabledataframe_Any_Pulmonary_Disease$Percentage <- round(tabledataframe_Any_Pulmonary_Disease$Percentage, 2) 
tabledataframe_Any_Pulmonary_Disease
#Any_Pulmonary_Disease       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Glucose_Any_Pulmonary_Disease <- svyby(~lbdsglsi, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svymean)
Glucose_Any_Pulmonary_Disease_sd <- svyby(~lbdsglsi, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svyvar)
Glucose_Any_Pulmonary_Disease_range <- svyby(~lbdsglsi, ~Any_Pulmonary_Disease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_Any_Pulmonary_Disease_summary <- data.frame(Glucose_Any_Pulmonary_Disease)
Glucose_Any_Pulmonary_Disease_summarysd<-data.frame(Glucose_Any_Pulmonary_Disease_sd)
Glucose_Any_Pulmonary_Disease_summarysd$sd <- sqrt(Glucose_Any_Pulmonary_Disease_summarysd$lbdsglsi)

#Print
Glucose_Any_Pulmonary_Disease_summary
Glucose_Any_Pulmonary_Disease_summarysd
Glucose_Any_Pulmonary_Disease_range



# Weighted Frequency table for VIGOROUS_ACTIVITY
table_weighted_VIGOROUS_ACTIVITY<-svytable(~VIGOROUS_ACTIVITY, nhanesDesign)
tabledataframe_VIGOROUS_ACTIVITY<-data.frame(table_weighted_VIGOROUS_ACTIVITY)
total_freq <- sum(tabledataframe_VIGOROUS_ACTIVITY$Freq)
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- (tabledataframe_VIGOROUS_ACTIVITY$Freq/total_freq)*100
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- round(tabledataframe_VIGOROUS_ACTIVITY$Percentage, 2) 
tabledataframe_VIGOROUS_ACTIVITY
#VIGOROUS_ACTIVITY       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Glucose_VIGOROUS_ACTIVITY <- svyby(~lbdsglsi, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svymean)
Glucose_VIGOROUS_ACTIVITY_sd <- svyby(~lbdsglsi, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svyvar)
Glucose_VIGOROUS_ACTIVITY_range <- svyby(~lbdsglsi, ~VIGOROUS_ACTIVITY, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Glucose_VIGOROUS_ACTIVITY_summary <- data.frame(Glucose_VIGOROUS_ACTIVITY)
Glucose_VIGOROUS_ACTIVITY_summarysd<-data.frame(Glucose_VIGOROUS_ACTIVITY_sd)
Glucose_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(Glucose_VIGOROUS_ACTIVITY_summarysd$lbdsglsi)

#Print
Glucose_VIGOROUS_ACTIVITY_summary
Glucose_VIGOROUS_ACTIVITY_summarysd
Glucose_VIGOROUS_ACTIVITY_range




# CRP

### CRP
# Calculate the mean and standard deviation of CRP by age_deciles
CRP_agegroups <- svyby(~lbxcrp, ~factor(age_deciles), design = nhanesDesign, svymean)
CRP_agegroups_sd <- svyby(~lbxcrp, ~factor(age_deciles), design = nhanesDesign, svyvar)
CRP_agegroups_range <- svyby(~lbxcrp, ~age_deciles, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_agegroups_summary <- data.frame(CRP_agegroups)
CRP_agegroups_summarysd<-data.frame(CRP_agegroups_sd)
CRP_agegroups_summarysd$sd <- sqrt(CRP_agegroups_summarysd$lbxcrp)


# View the results
print(CRP_agegroups_summary)
print(CRP_agegroups_summarysd)
print(CRP_agegroups_range)


# Calculate the mean and standard deviation of CRP by age_deciles
CRP_race <- svyby(~lbxcrp, ~factor(RACE_RECODE), design = nhanesDesign, svymean)
CRP_race_sd <- svyby(~lbxcrp, ~factor(RACE_RECODE), design = nhanesDesign, svyvar)
CRP_race_range <- svyby(~lbxcrp, ~RACE_RECODE, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_race_summary <- data.frame(CRP_race)
CRP_race_summarysd<-data.frame(CRP_race_sd)
CRP_race_summarysd$sd <- sqrt(CRP_race_summarysd$lbxcrp)

# Print
CRP_race_summary
CRP_race_summarysd
CRP_race_range


# Calculate the mean and standard deviation of CRP by age_deciles
CRP_sex <- svyby(~lbxcrp, ~factor(riagendr), design = nhanesDesign, svymean)
CRP_sex_sd <- svyby(~lbxcrp, ~factor(riagendr), design = nhanesDesign, svyvar)
CRP_sex_range <- svyby(~lbxcrp, ~riagendr, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_sex_summary <- data.frame(CRP_sex)
CRP_sex_summarysd<-data.frame(CRP_sex_sd)
CRP_sex_summarysd$sd <- sqrt(CRP_sex_summarysd$lbxcrp)

#Print
CRP_sex_summary
CRP_sex_summarysd
CRP_sex_range


# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
CRP_BMI_Groups <- svyby(~lbxcrp, ~factor(BMI_Groups), design = nhanesDesign, svymean)
CRP_BMI_Groups_sd <- svyby(~lbxcrp, ~factor(BMI_Groups), design = nhanesDesign, svyvar)
CRP_BMI_Groups_range <- svyby(~lbxcrp, ~BMI_Groups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_BMI_Groups_summary <- data.frame(CRP_BMI_Groups)
CRP_BMI_Groups_summarysd<-data.frame(CRP_BMI_Groups_sd)
CRP_BMI_Groups_summarysd$sd <- sqrt(CRP_BMI_Groups_summarysd$lbxcrp)

# Print
CRP_BMI_Groups_summary
CRP_BMI_Groups_summarysd
CRP_BMI_Groups_range



# Weighted Frequency table for Education
svytable(~dmdeduc2, nhanesDesign)
# Less than 9th grade (N=4651)	87898201.3	6.066%
# 9-11th grade (Includes 12th grade with no diploma) (N=5402)	159491596.8	11.006%
# High school graduate/GED or equivalent (N=8537)	358733876	24.756%
# Some college or AA degree (N=10183)	444474864	30.673%
# College graduate or above (N=7559)	396998727.5	27.396%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
CRP_Education <- svyby(~lbxcrp, ~factor(dmdeduc2), design = nhanesDesign, svymean)
CRP_Education_sd <- svyby(~lbxcrp, ~factor(dmdeduc2), design = nhanesDesign, svyvar)
CRP_Education_range <- svyby(~lbxcrp, ~dmdeduc2, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_Education_summary <- data.frame(CRP_Education)
CRP_Education_summarysd<-data.frame(CRP_Education_sd)
CRP_Education_summarysd$sd <- sqrt(CRP_Education_summarysd$lbxcrp)

# View the results
print(CRP_Education_summary)
CRP_Education_summarysd
CRP_Education_range



# Weighted Frequency table for Income
table_weighted_income<-svytable(~FamilyIncomeRecode, nhanesDesign)
tabledataframe<-data.frame(table_weighted_income)
total_freq <- sum(tabledataframe$Freq)
tabledataframe$Percentage <- (tabledataframe$Freq/total_freq)*100
tabledataframe$Percentage <- round(tabledataframe$Percentage, 2)

#   familyincomeannual      Freq Percentage

#             $ <5000  28398917       2.14%
#         10000-15000  92076718       6.92%
#          15000-2000  83075022       6.25%
#         20000-25000 108780678       8.18%
#         25000-35000 136452422      10.26%
#         35000-45000 145722842      10.96%
#         45000-55000 131348748       9.87%
#          5000-10000  48284605       3.63%
#        55000-65000 101404308       7.62%
#        65000-75000  88542639       6.66%
#       75000-100000  70246203       5.28%
#             >100000 295801899      22.24%
CRP_familyincomeannual <- svyby(~lbxcrp, ~factor(FamilyIncomeRecode), design = nhanesDesign, svymean)
CRP_familyincomeannual_sd <- svyby(~lbxcrp, ~factor(FamilyIncomeRecode), design = nhanesDesign, svyvar)
CRP_familyincomeannual_range <- svyby(~lbxcrp, ~FamilyIncomeRecode, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_familyincomeannual_summary <- data.frame(CRP_familyincomeannual)
CRP_familyincomeannual_summarysd<-data.frame(CRP_familyincomeannual_sd)
CRP_familyincomeannual_summarysd$sd <- sqrt(CRP_familyincomeannual_summarysd$lbxcrp)

# View the results
print(CRP_familyincomeannual_summary)
CRP_familyincomeannual_summarysd
CRP_familyincomeannual_range


# Weighted Frequency table for Alcohol
table_weighted_alcohol<-svytable(~Alcohol_Any_Categories, nhanesDesign)
tabledataframe_alcohol<-data.frame(table_weighted_alcohol)
total_freq <- sum(tabledataframe_alcohol$Freq)
tabledataframe_alcohol$Percentage <- (tabledataframe_alcohol$Freq/total_freq)*100
tabledataframe_alcohol$Percentage <- round(tabledataframe_alcohol$Percentage, 2) 
tabledataframe_alcohol
# Comparison of a numeric variable by a grouping variable
# Alcohol_Any_Categories      Freq Percentage
#          Not a Drinker 249130415      25.33
#         Heavy Drinker 133341354      13.56
#        Regular Drinker 601083385      61.11

CRP_Alcohol <- svyby(~lbxcrp, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svymean)
CRP_Alcohol_sd <- svyby(~lbxcrp, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svyvar)
CRP_Alcohol_range <- svyby(~lbxcrp, ~Alcohol_Any_Categories, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_Alcohol_summary <- data.frame(CRP_Alcohol)
CRP_Alcohol_summarysd<-data.frame(CRP_Alcohol_sd)
CRP_Alcohol_summarysd$sd <- sqrt(CRP_Alcohol_summarysd$lbxcrp)

# View the results
print(CRP_Alcohol_summary)
CRP_Alcohol_summarysd
CRP_Alcohol_range


# Weighted Frequency table for Smoking
table_weighted_SmokingStatus<-svytable(~SmokingStatus, nhanesDesign)
tabledataframe_SmokingStatus<-data.frame(table_weighted_SmokingStatus)
total_freq <- sum(tabledataframe_SmokingStatus$Freq)
tabledataframe_SmokingStatus$Percentage <- (tabledataframe_SmokingStatus$Freq/total_freq)*100
tabledataframe_SmokingStatus$Percentage <- round(tabledataframe_SmokingStatus$Percentage, 2) 
tabledataframe_SmokingStatus
#    SmokingStatus  Freq Percentage
# Current Smoker 316496128      21.76%
#  Former Smoker 362187774      24.90%
#   Never Smoker 775650383      53.3%
CRP_SmokingStatus <- svyby(~lbxcrp, ~factor(SmokingStatus), design = nhanesDesign, svymean)
CRP_SmokingStatus_sd <- svyby(~lbxcrp, ~factor(SmokingStatus), design = nhanesDesign, svyvar)
CRP_SmokingStatus_range <- svyby(~lbxcrp, ~SmokingStatus, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_SmokingStatus_summary <- data.frame(CRP_SmokingStatus)
CRP_SmokingStatus_summarysd<-data.frame(CRP_SmokingStatus_sd)
CRP_SmokingStatus_summarysd$sd <- sqrt(CRP_SmokingStatus_summarysd$lbxcrp)


# View the results
print(CRP_SmokingStatus_summary)
CRP_SmokingStatus_summarysd
CRP_SmokingStatus_range



# Weighted Frequency table for Diabetes
table_weighted_Diabetes<-svytable(~Diabetes, nhanesDesign)
tabledataframe_Diabetes<-data.frame(table_weighted_Diabetes)
total_freq <- sum(tabledataframe_Diabetes$Freq)
tabledataframe_Diabetes$Percentage <- (tabledataframe_Diabetes$Freq/total_freq)*100
tabledataframe_Diabetes$Percentage <- round(tabledataframe_Diabetes$Percentage, 2) 
tabledataframe_Diabetes

#Diabetes       Freq Percentage
#                No 1322298535      91.39%
#               Yes  124521799       8.61%
CRP_Diabetes <- svyby(~lbxcrp, ~factor(Diabetes), design = nhanesDesign, svymean)
CRP_Diabetes_sd <- svyby(~lbxcrp, ~factor(Diabetes), design = nhanesDesign, svyvar)
CRP_Diabetes_range <- svyby(~lbxcrp, ~Diabetes, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation int

CRP_Diabetes_summary <- data.frame(CRP_Diabetes)
CRP_Diabetes_summarysd<-data.frame(CRP_Diabetes_sd)
CRP_Diabetes_summarysd$sd <- sqrt(CRP_Diabetes_summarysd$lbxcrp)

# View the results
print(CRP_Diabetes_summary)
CRP_Diabetes_summarysd
CRP_Diabetes_range



# Weighted Frequency table for Hypertension
table_weighted_Hypertension<-svytable(~Hypertension, nhanesDesign)
tabledataframe_Hypertension<-data.frame(table_weighted_Hypertension)
total_freq <- sum(tabledataframe_Hypertension$Freq)
tabledataframe_Hypertension$Percentage <- (tabledataframe_Hypertension$Freq/total_freq)*100
tabledataframe_Hypertension$Percentage <- round(tabledataframe_Hypertension$Percentage, 2) 
tabledataframe_Hypertension
#Hypertension       Freq Percentage
#1                      0 1125223737      84.25
#2                      1  210355103      15.75

CRP_Hypertension <- svyby(~lbxcrp, ~factor(Hypertension), design = nhanesDesign, svymean)
CRP_Hypertension_sd <- svyby(~lbxcrp, ~factor(Hypertension), design = nhanesDesign, svyvar)
CRP_Hypertension_range <- svyby(~lbxcrp, ~Hypertension, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_Hypertension_summary <- data.frame(CRP_Hypertension)
CRP_Hypertension_summarysd<-data.frame(CRP_Hypertension_sd)
CRP_Hypertension_summarysd$sd <- sqrt(CRP_Hypertension_summarysd$lbxcrp)

# View the results
print(CRP_Hypertension_summary)
CRP_Hypertension_summarysd
CRP_Hypertension_range


# Weighted Frequency table for Dyslipidemia
table_weighted_Dyslipidemia<-svytable(~Dyslipidemia, nhanesDesign)
tabledataframe_Dyslipidemia<-data.frame(table_weighted_Dyslipidemia)
total_freq <- sum(tabledataframe_Dyslipidemia$Freq)
tabledataframe_Dyslipidemia$Percentage <- (tabledataframe_Dyslipidemia$Freq/total_freq)*100
tabledataframe_Dyslipidemia$Percentage <- round(tabledataframe_Dyslipidemia$Percentage, 2) 
tabledataframe_Dyslipidemia
#Dyslipidemia       Freq Percentage
#1            0 1213253354      82.45
#2            1  258237153      17.55

CRP_Dyslipidemia <- svyby(~lbxcrp, ~factor(Dyslipidemia), design = nhanesDesign, svymean)
CRP_Dyslipidemia_sd <- svyby(~lbxcrp, ~factor(Dyslipidemia), design = nhanesDesign, svyvar)
CRP_Dyslipidemia_range <- svyby(~lbxcrp, ~Dyslipidemia, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_Dyslipidemia_summary <- data.frame(CRP_Dyslipidemia)
CRP_Dyslipidemia_summarysd<-data.frame(CRP_Dyslipidemia_sd)
CRP_Dyslipidemia_summarysd$sd <- sqrt(CRP_Dyslipidemia_summarysd$lbxcrp)

# Print
CRP_Dyslipidemia_summary
CRP_Dyslipidemia_summarysd
CRP_Dyslipidemia_range



# Weighted Frequency table for Cardiac Disease
table_weighted_HeartDisease<-svytable(~HeartDisease, nhanesDesign)
tabledataframe_HeartDisease<-data.frame(table_weighted_HeartDisease)
total_freq <- sum(tabledataframe_HeartDisease$Freq)
tabledataframe_HeartDisease$Percentage <- (tabledataframe_HeartDisease$Freq/total_freq)*100
tabledataframe_HeartDisease$Percentage <- round(tabledataframe_HeartDisease$Percentage, 2) 
tabledataframe_HeartDisease
#HeartDisease       Freq Percentage
#1            0 1352032083      93.3
#2            1  97031973      6.7

CRP_HeartDisease <- svyby(~lbxcrp, ~factor(HeartDisease), design = nhanesDesign, svymean)
CRP_HeartDisease_sd <- svyby(~lbxcrp, ~factor(HeartDisease), design = nhanesDesign, svyvar)
CRP_HeartDisease_range <- svyby(~lbxcrp, ~HeartDisease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_HeartDisease_summary <- data.frame(CRP_HeartDisease)
CRP_HeartDisease_summarysd<-data.frame(CRP_HeartDisease_sd)
CRP_HeartDisease_summarysd$sd <- sqrt(CRP_HeartDisease_summarysd$lbxcrp)

#Print
CRP_HeartDisease_summary
CRP_HeartDisease_summarysd
CRP_HeartDisease_range



# Weighted Frequency table for Cancer
table_weighted_Cancer<-svytable(~mcq220, nhanesDesign)
tabledataframe_Cancer<-data.frame(table_weighted_Cancer)
total_freq <- sum(tabledataframe_Cancer$Freq)
tabledataframe_Cancer$Percentage <- (tabledataframe_Cancer$Freq/total_freq)*100
tabledataframe_Cancer$Percentage <- round(tabledataframe_Cancer$Percentage, 2) 
tabledataframe_Cancer
#Cancer       Freq Percentage
#1            0 1314831945      9.15
#2            1  132562783      90.74

CRP_Cancer <- svyby(~lbxcrp, ~factor(mcq220), design = nhanesDesign, svymean)
CRP_Cancer_sd <- svyby(~lbxcrp, ~factor(mcq220), design = nhanesDesign, svyvar)
CRP_Cancer_range <- svyby(~lbxcrp, ~mcq220, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_Cancer_summary <- data.frame(CRP_Cancer)
CRP_Cancer_summarysd<-data.frame(CRP_Cancer_sd)
CRP_Cancer_summarysd$sd <- sqrt(CRP_Cancer_summarysd$lbxcrp)

# Print
CRP_Cancer_summary
CRP_Cancer_summarysd
CRP_Cancer_range



# Weighted Frequency table for Any_Pulmonary_Disease
table_weighted_Any_Pulmonary_Disease<-svytable(~Any_Pulmonary_Disease, nhanesDesign)
tabledataframe_Any_Pulmonary_Disease<-data.frame(table_weighted_Any_Pulmonary_Disease)
total_freq <- sum(tabledataframe_Any_Pulmonary_Disease$Freq)
tabledataframe_Any_Pulmonary_Disease$Percentage <- (tabledataframe_Any_Pulmonary_Disease$Freq/total_freq)*100
tabledataframe_Any_Pulmonary_Disease$Percentage <- round(tabledataframe_Any_Pulmonary_Disease$Percentage, 2) 
tabledataframe_Any_Pulmonary_Disease
#Any_Pulmonary_Disease       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

CRP_Any_Pulmonary_Disease <- svyby(~lbxcrp, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svymean)
CRP_Any_Pulmonary_Disease_sd <- svyby(~lbxcrp, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svyvar)
CRP_Any_Pulmonary_Disease_range <- svyby(~lbxcrp, ~Any_Pulmonary_Disease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_Any_Pulmonary_Disease_summary <- data.frame(CRP_Any_Pulmonary_Disease)
CRP_Any_Pulmonary_Disease_summarysd<-data.frame(CRP_Any_Pulmonary_Disease_sd)
CRP_Any_Pulmonary_Disease_summarysd$sd <- sqrt(CRP_Any_Pulmonary_Disease_summarysd$lbxcrp)

#Print
CRP_Any_Pulmonary_Disease_summary
CRP_Any_Pulmonary_Disease_summarysd
CRP_Any_Pulmonary_Disease_range



# Weighted Frequency table for VIGOROUS_ACTIVITY
table_weighted_VIGOROUS_ACTIVITY<-svytable(~VIGOROUS_ACTIVITY, nhanesDesign)
tabledataframe_VIGOROUS_ACTIVITY<-data.frame(table_weighted_VIGOROUS_ACTIVITY)
total_freq <- sum(tabledataframe_VIGOROUS_ACTIVITY$Freq)
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- (tabledataframe_VIGOROUS_ACTIVITY$Freq/total_freq)*100
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- round(tabledataframe_VIGOROUS_ACTIVITY$Percentage, 2) 
tabledataframe_VIGOROUS_ACTIVITY
#VIGOROUS_ACTIVITY       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

CRP_VIGOROUS_ACTIVITY <- svyby(~lbxcrp, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svymean)
CRP_VIGOROUS_ACTIVITY_sd <- svyby(~lbxcrp, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svyvar)
CRP_VIGOROUS_ACTIVITY_range <- svyby(~lbxcrp, ~VIGOROUS_ACTIVITY, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
CRP_VIGOROUS_ACTIVITY_summary <- data.frame(CRP_VIGOROUS_ACTIVITY)
CRP_VIGOROUS_ACTIVITY_summarysd<-data.frame(CRP_VIGOROUS_ACTIVITY_sd)
CRP_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(CRP_VIGOROUS_ACTIVITY_summarysd$lbxcrp)

#Print
CRP_VIGOROUS_ACTIVITY_summary
CRP_VIGOROUS_ACTIVITY_summarysd
CRP_VIGOROUS_ACTIVITY_range





# Lymphocyte

### Lymphocyte
# Calculate the mean and standard deviation of Lymphocyte by age_deciles
Lymphocyte_agegroups <- svyby(~lbxlypct, ~factor(age_deciles), design = nhanesDesign, svymean)
Lymphocyte_agegroups_sd <- svyby(~lbxlypct, ~factor(age_deciles), design = nhanesDesign, svyvar)
Lymphocyte_agegroups_range <- svyby(~lbxlypct, ~age_deciles, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_agegroups_summary <- data.frame(Lymphocyte_agegroups)
Lymphocyte_agegroups_summarysd<-data.frame(Lymphocyte_agegroups_sd)
Lymphocyte_agegroups_summarysd$sd <- sqrt(Lymphocyte_agegroups_summarysd$lbxlypct)


# View the results
print(Lymphocyte_agegroups_summary)
print(Lymphocyte_agegroups_summarysd)
print(Lymphocyte_agegroups_range)


# Calculate the mean and standard deviation of Lymphocyte by age_deciles
Lymphocyte_race <- svyby(~lbxlypct, ~factor(RACE_RECODE), design = nhanesDesign, svymean)
Lymphocyte_race_sd <- svyby(~lbxlypct, ~factor(RACE_RECODE), design = nhanesDesign, svyvar)
Lymphocyte_race_range <- svyby(~lbxlypct, ~RACE_RECODE, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_race_summary <- data.frame(Lymphocyte_race)
Lymphocyte_race_summarysd<-data.frame(Lymphocyte_race_sd)
Lymphocyte_race_summarysd$sd <- sqrt(Lymphocyte_race_summarysd$lbxlypct)

# Print
Lymphocyte_race_summary
Lymphocyte_race_summarysd
Lymphocyte_race_range


# Calculate the mean and standard deviation of Lymphocyte by age_deciles
Lymphocyte_sex <- svyby(~lbxlypct, ~factor(riagendr), design = nhanesDesign, svymean)
Lymphocyte_sex_sd <- svyby(~lbxlypct, ~factor(riagendr), design = nhanesDesign, svyvar)
Lymphocyte_sex_range <- svyby(~lbxlypct, ~riagendr, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_sex_summary <- data.frame(Lymphocyte_sex)
Lymphocyte_sex_summarysd<-data.frame(Lymphocyte_sex_sd)
Lymphocyte_sex_summarysd$sd <- sqrt(Lymphocyte_sex_summarysd$lbxlypct)

#Print
Lymphocyte_sex_summary
Lymphocyte_sex_summarysd
Lymphocyte_sex_range


# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Lymphocyte_BMI_Groups <- svyby(~lbxlypct, ~factor(BMI_Groups), design = nhanesDesign, svymean)
Lymphocyte_BMI_Groups_sd <- svyby(~lbxlypct, ~factor(BMI_Groups), design = nhanesDesign, svyvar)
Lymphocyte_BMI_Groups_range <- svyby(~lbxlypct, ~BMI_Groups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_BMI_Groups_summary <- data.frame(Lymphocyte_BMI_Groups)
Lymphocyte_BMI_Groups_summarysd<-data.frame(Lymphocyte_BMI_Groups_sd)
Lymphocyte_BMI_Groups_summarysd$sd <- sqrt(Lymphocyte_BMI_Groups_summarysd$lbxlypct)

# Print
Lymphocyte_BMI_Groups_summary
Lymphocyte_BMI_Groups_summarysd
Lymphocyte_BMI_Groups_range




# Weighted Frequency table for Education
svytable(~dmdeduc2, nhanesDesign)
# Less than 9th grade (N=4651)	87898201.3	6.066%
# 9-11th grade (Includes 12th grade with no diploma) (N=5402)	159491596.8	11.006%
# High school graduate/GED or equivalent (N=8537)	358733876	24.756%
# Some college or AA degree (N=10183)	444474864	30.673%
# College graduate or above (N=7559)	396998727.5	27.396%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Lymphocyte_Education <- svyby(~lbxlypct, ~factor(dmdeduc2), design = nhanesDesign, svymean)
Lymphocyte_Education_sd <- svyby(~lbxlypct, ~factor(dmdeduc2), design = nhanesDesign, svyvar)
Lymphocyte_Education_range <- svyby(~lbxlypct, ~dmdeduc2, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_Education_summary <- data.frame(Lymphocyte_Education)
Lymphocyte_Education_summarysd<-data.frame(Lymphocyte_Education_sd)
Lymphocyte_Education_summarysd$sd <- sqrt(Lymphocyte_Education_summarysd$lbxlypct)

# View the results
print(Lymphocyte_Education_summary)
Lymphocyte_Education_summarysd
Lymphocyte_Education_range



# Weighted Frequency table for Income
table_weighted_income<-svytable(~familyincomeannual, nhanesDesign)
tabledataframe<-data.frame(table_weighted_income)
total_freq <- sum(tabledataframe$Freq)
tabledataframe$Percentage <- (tabledataframe$Freq/total_freq)*100
tabledataframe$Percentage <- round(tabledataframe$Percentage, 2)

#   familyincomeannual      Freq Percentage

#             $ <5000  28398917       2.14%
#         10000-15000  92076718       6.92%
#          15000-2000  83075022       6.25%
#         20000-25000 108780678       8.18%
#         25000-35000 136452422      10.26%
#         35000-45000 145722842      10.96%
#         45000-55000 131348748       9.87%
#          5000-10000  48284605       3.63%
#        55000-65000 101404308       7.62%
#        65000-75000  88542639       6.66%
#       75000-100000  70246203       5.28%
#             >100000 295801899      22.24%
Lymphocyte_familyincomeannual <- svyby(~lbxlypct, ~factor(FamilyIncomeRecode), design = nhanesDesign, svymean)
Lymphocyte_familyincomeannual_sd <- svyby(~lbxlypct, ~factor(FamilyIncomeRecode), design = nhanesDesign, svyvar)
Lymphocyte_familyincomeannual_range <- svyby(~lbxlypct, ~FamilyIncomeRecode, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_familyincomeannual_summary <- data.frame(Lymphocyte_familyincomeannual)
Lymphocyte_familyincomeannual_summarysd<-data.frame(Lymphocyte_familyincomeannual_sd)
Lymphocyte_familyincomeannual_summarysd$sd <- sqrt(Lymphocyte_familyincomeannual_summarysd$lbxlypct)

# View the results
print(Lymphocyte_familyincomeannual_summary)
Lymphocyte_familyincomeannual_summarysd
Lymphocyte_familyincomeannual_range


# Weighted Frequency table for Alcohol
table_weighted_alcohol<-svytable(~Alcohol_Any_Categories, nhanesDesign)
tabledataframe_alcohol<-data.frame(table_weighted_alcohol)
total_freq <- sum(tabledataframe_alcohol$Freq)
tabledataframe_alcohol$Percentage <- (tabledataframe_alcohol$Freq/total_freq)*100
tabledataframe_alcohol$Percentage <- round(tabledataframe_alcohol$Percentage, 2) 
tabledataframe_alcohol
# Comparison of a numeric variable by a grouping variable
# Alcohol_Any_Categories      Freq Percentage
#          Not a Drinker 249130415      25.33
#         Heavy Drinker 133341354      13.56
#        Regular Drinker 601083385      61.11

Lymphocyte_Alcohol <- svyby(~lbxlypct, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svymean)
Lymphocyte_Alcohol_sd <- svyby(~lbxlypct, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svyvar)
Lymphocyte_Alcohol_range <- svyby(~lbxlypct, ~Alcohol_Any_Categories, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_Alcohol_summary <- data.frame(Lymphocyte_Alcohol)
Lymphocyte_Alcohol_summarysd<-data.frame(Lymphocyte_Alcohol_sd)
Lymphocyte_Alcohol_summarysd$sd <- sqrt(Lymphocyte_Alcohol_summarysd$lbxlypct)

# View the results
print(Lymphocyte_Alcohol_summary)
Lymphocyte_Alcohol_summarysd
Lymphocyte_Alcohol_range


# Weighted Frequency table for Smoking
table_weighted_SmokingStatus<-svytable(~SmokingStatus, nhanesDesign)
tabledataframe_SmokingStatus<-data.frame(table_weighted_SmokingStatus)
total_freq <- sum(tabledataframe_SmokingStatus$Freq)
tabledataframe_SmokingStatus$Percentage <- (tabledataframe_SmokingStatus$Freq/total_freq)*100
tabledataframe_SmokingStatus$Percentage <- round(tabledataframe_SmokingStatus$Percentage, 2) 
tabledataframe_SmokingStatus
#    SmokingStatus  Freq Percentage
# Current Smoker 316496128      21.76%
#  Former Smoker 362187774      24.90%
#   Never Smoker 775650383      53.3%
Lymphocyte_SmokingStatus <- svyby(~lbxlypct, ~factor(SmokingStatus), design = nhanesDesign, svymean)
Lymphocyte_SmokingStatus_sd <- svyby(~lbxlypct, ~factor(SmokingStatus), design = nhanesDesign, svyvar)
Lymphocyte_SmokingStatus_range <- svyby(~lbxlypct, ~SmokingStatus, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_SmokingStatus_summary <- data.frame(Lymphocyte_SmokingStatus)
Lymphocyte_SmokingStatus_summarysd<-data.frame(Lymphocyte_SmokingStatus_sd)
Lymphocyte_SmokingStatus_summarysd$sd <- sqrt(Lymphocyte_SmokingStatus_summarysd$lbxlypct)


# View the results
print(Lymphocyte_SmokingStatus_summary)
Lymphocyte_SmokingStatus_summarysd
Lymphocyte_SmokingStatus_range



# Weighted Frequency table for Diabetes
table_weighted_Diabetes<-svytable(~Diabetes, nhanesDesign)
tabledataframe_Diabetes<-data.frame(table_weighted_Diabetes)
total_freq <- sum(tabledataframe_Diabetes$Freq)
tabledataframe_Diabetes$Percentage <- (tabledataframe_Diabetes$Freq/total_freq)*100
tabledataframe_Diabetes$Percentage <- round(tabledataframe_Diabetes$Percentage, 2) 
tabledataframe_Diabetes

#Diabetes       Freq Percentage
#                No 1322298535      91.39%
#               Yes  124521799       8.61%
Lymphocyte_Diabetes <- svyby(~lbxlypct, ~factor(Diabetes), design = nhanesDesign, svymean)
Lymphocyte_Diabetes_sd <- svyby(~lbxlypct, ~factor(Diabetes), design = nhanesDesign, svyvar)
Lymphocyte_Diabetes_range <- svyby(~lbxlypct, ~Diabetes, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation int

Lymphocyte_Diabetes_summary <- data.frame(Lymphocyte_Diabetes)
Lymphocyte_Diabetes_summarysd<-data.frame(Lymphocyte_Diabetes_sd)
Lymphocyte_Diabetes_summarysd$sd <- sqrt(Lymphocyte_Diabetes_summarysd$lbxlypct)

# View the results
print(Lymphocyte_Diabetes_summary)
Lymphocyte_Diabetes_summarysd
Lymphocyte_Diabetes_range



# Weighted Frequency table for Hypertension
table_weighted_Hypertension<-svytable(~Hypertension, nhanesDesign)
tabledataframe_Hypertension<-data.frame(table_weighted_Hypertension)
total_freq <- sum(tabledataframe_Hypertension$Freq)
tabledataframe_Hypertension$Percentage <- (tabledataframe_Hypertension$Freq/total_freq)*100
tabledataframe_Hypertension$Percentage <- round(tabledataframe_Hypertension$Percentage, 2) 
tabledataframe_Hypertension
#Hypertension       Freq Percentage
#1                      0 1125223737      84.25
#2                      1  210355103      15.75

Lymphocyte_Hypertension <- svyby(~lbxlypct, ~factor(Hypertension), design = nhanesDesign, svymean)
Lymphocyte_Hypertension_sd <- svyby(~lbxlypct, ~factor(Hypertension), design = nhanesDesign, svyvar)
Lymphocyte_Hypertension_range <- svyby(~lbxlypct, ~Hypertension, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_Hypertension_summary <- data.frame(Lymphocyte_Hypertension)
Lymphocyte_Hypertension_summarysd<-data.frame(Lymphocyte_Hypertension_sd)
Lymphocyte_Hypertension_summarysd$sd <- sqrt(Lymphocyte_Hypertension_summarysd$lbxlypct)

# View the results
print(Lymphocyte_Hypertension_summary)
Lymphocyte_Hypertension_summarysd
Lymphocyte_Hypertension_range


# Weighted Frequency table for Dyslipidemia
table_weighted_Dyslipidemia<-svytable(~Dyslipidemia, nhanesDesign)
tabledataframe_Dyslipidemia<-data.frame(table_weighted_Dyslipidemia)
total_freq <- sum(tabledataframe_Dyslipidemia$Freq)
tabledataframe_Dyslipidemia$Percentage <- (tabledataframe_Dyslipidemia$Freq/total_freq)*100
tabledataframe_Dyslipidemia$Percentage <- round(tabledataframe_Dyslipidemia$Percentage, 2) 
tabledataframe_Dyslipidemia
#Dyslipidemia       Freq Percentage
#1            0 1213253354      82.45
#2            1  258237153      17.55

Lymphocyte_Dyslipidemia <- svyby(~lbxlypct, ~factor(Dyslipidemia), design = nhanesDesign, svymean)
Lymphocyte_Dyslipidemia_sd <- svyby(~lbxlypct, ~factor(Dyslipidemia), design = nhanesDesign, svyvar)
Lymphocyte_Dyslipidemia_range <- svyby(~lbxlypct, ~Dyslipidemia, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_Dyslipidemia_summary <- data.frame(Lymphocyte_Dyslipidemia)
Lymphocyte_Dyslipidemia_summarysd<-data.frame(Lymphocyte_Dyslipidemia_sd)
Lymphocyte_Dyslipidemia_summarysd$sd <- sqrt(Lymphocyte_Dyslipidemia_summarysd$lbxlypct)

# Print
Lymphocyte_Dyslipidemia_summary
Lymphocyte_Dyslipidemia_summarysd
Lymphocyte_Dyslipidemia_range



# Weighted Frequency table for Cardiac Disease
table_weighted_HeartDisease<-svytable(~HeartDisease, nhanesDesign)
tabledataframe_HeartDisease<-data.frame(table_weighted_HeartDisease)
total_freq <- sum(tabledataframe_HeartDisease$Freq)
tabledataframe_HeartDisease$Percentage <- (tabledataframe_HeartDisease$Freq/total_freq)*100
tabledataframe_HeartDisease$Percentage <- round(tabledataframe_HeartDisease$Percentage, 2) 
tabledataframe_HeartDisease
#HeartDisease       Freq Percentage
#1            0 1352032083      93.3
#2            1  97031973      6.7

Lymphocyte_HeartDisease <- svyby(~lbxlypct, ~factor(HeartDisease), design = nhanesDesign, svymean)
Lymphocyte_HeartDisease_sd <- svyby(~lbxlypct, ~factor(HeartDisease), design = nhanesDesign, svyvar)
Lymphocyte_HeartDisease_range <- svyby(~lbxlypct, ~HeartDisease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_HeartDisease_summary <- data.frame(Lymphocyte_HeartDisease)
Lymphocyte_HeartDisease_summarysd<-data.frame(Lymphocyte_HeartDisease_sd)
Lymphocyte_HeartDisease_summarysd$sd <- sqrt(Lymphocyte_HeartDisease_summarysd$lbxlypct)

#Print
Lymphocyte_HeartDisease_summary
Lymphocyte_HeartDisease_summarysd
Lymphocyte_HeartDisease_range



# Weighted Frequency table for Cancer
table_weighted_Cancer<-svytable(~mcq220, nhanesDesign)
tabledataframe_Cancer<-data.frame(table_weighted_Cancer)
total_freq <- sum(tabledataframe_Cancer$Freq)
tabledataframe_Cancer$Percentage <- (tabledataframe_Cancer$Freq/total_freq)*100
tabledataframe_Cancer$Percentage <- round(tabledataframe_Cancer$Percentage, 2) 
tabledataframe_Cancer
#Cancer       Freq Percentage
#1            0 1314831945      9.15
#2            1  132562783      90.74

Lymphocyte_Cancer <- svyby(~lbxlypct, ~factor(mcq220), design = nhanesDesign, svymean)
Lymphocyte_Cancer_sd <- svyby(~lbxlypct, ~factor(mcq220), design = nhanesDesign, svyvar)
Lymphocyte_Cancer_range <- svyby(~lbxlypct, ~mcq220, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_Cancer_summary <- data.frame(Lymphocyte_Cancer)
Lymphocyte_Cancer_summarysd<-data.frame(Lymphocyte_Cancer_sd)
Lymphocyte_Cancer_summarysd$sd <- sqrt(Lymphocyte_Cancer_summarysd$lbxlypct)

# Print
Lymphocyte_Cancer_summary
Lymphocyte_Cancer_summarysd
Lymphocyte_Cancer_range



# Weighted Frequency table for Any_Pulmonary_Disease
table_weighted_Any_Pulmonary_Disease<-svytable(~Any_Pulmonary_Disease, nhanesDesign)
tabledataframe_Any_Pulmonary_Disease<-data.frame(table_weighted_Any_Pulmonary_Disease)
total_freq <- sum(tabledataframe_Any_Pulmonary_Disease$Freq)
tabledataframe_Any_Pulmonary_Disease$Percentage <- (tabledataframe_Any_Pulmonary_Disease$Freq/total_freq)*100
tabledataframe_Any_Pulmonary_Disease$Percentage <- round(tabledataframe_Any_Pulmonary_Disease$Percentage, 2) 
tabledataframe_Any_Pulmonary_Disease
#Any_Pulmonary_Disease       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Lymphocyte_Any_Pulmonary_Disease <- svyby(~lbxlypct, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svymean)
Lymphocyte_Any_Pulmonary_Disease_sd <- svyby(~lbxlypct, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svyvar)
Lymphocyte_Any_Pulmonary_Disease_range <- svyby(~lbxlypct, ~Any_Pulmonary_Disease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_Any_Pulmonary_Disease_summary <- data.frame(Lymphocyte_Any_Pulmonary_Disease)
Lymphocyte_Any_Pulmonary_Disease_summarysd<-data.frame(Lymphocyte_Any_Pulmonary_Disease_sd)
Lymphocyte_Any_Pulmonary_Disease_summarysd$sd <- sqrt(Lymphocyte_Any_Pulmonary_Disease_summarysd$lbxlypct)

#Print
Lymphocyte_Any_Pulmonary_Disease_summary
Lymphocyte_Any_Pulmonary_Disease_summarysd
Lymphocyte_Any_Pulmonary_Disease_range



# Weighted Frequency table for VIGOROUS_ACTIVITY
table_weighted_VIGOROUS_ACTIVITY<-svytable(~VIGOROUS_ACTIVITY, nhanesDesign)
tabledataframe_VIGOROUS_ACTIVITY<-data.frame(table_weighted_VIGOROUS_ACTIVITY)
total_freq <- sum(tabledataframe_VIGOROUS_ACTIVITY$Freq)
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- (tabledataframe_VIGOROUS_ACTIVITY$Freq/total_freq)*100
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- round(tabledataframe_VIGOROUS_ACTIVITY$Percentage, 2) 
tabledataframe_VIGOROUS_ACTIVITY
#VIGOROUS_ACTIVITY       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Lymphocyte_VIGOROUS_ACTIVITY <- svyby(~lbxlypct, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svymean)
Lymphocyte_VIGOROUS_ACTIVITY_sd <- svyby(~lbxlypct, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svyvar)
Lymphocyte_VIGOROUS_ACTIVITY_range <- svyby(~lbxlypct, ~VIGOROUS_ACTIVITY, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Lymphocyte_VIGOROUS_ACTIVITY_summary <- data.frame(Lymphocyte_VIGOROUS_ACTIVITY)
Lymphocyte_VIGOROUS_ACTIVITY_summarysd<-data.frame(Lymphocyte_VIGOROUS_ACTIVITY_sd)
Lymphocyte_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(Lymphocyte_VIGOROUS_ACTIVITY_summarysd$lbxlypct)

#Print
Lymphocyte_VIGOROUS_ACTIVITY_summary
Lymphocyte_VIGOROUS_ACTIVITY_summarysd
Lymphocyte_VIGOROUS_ACTIVITY_range





# Mean_Cell_Volume

### Mean_Cell_Volume
# Calculate the mean and standard deviation of Mean_Cell_Volume by age_deciles
Mean_Cell_Volume_agegroups <- svyby(~lbxmcvsi, ~factor(age_deciles), design = nhanesDesign, svymean)
Mean_Cell_Volume_agegroups_sd <- svyby(~lbxmcvsi, ~factor(age_deciles), design = nhanesDesign, svyvar)
Mean_Cell_Volume_agegroups_range <- svyby(~lbxmcvsi, ~age_deciles, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_agegroups_summary <- data.frame(Mean_Cell_Volume_agegroups)
Mean_Cell_Volume_agegroups_summarysd<-data.frame(Mean_Cell_Volume_agegroups_sd)
Mean_Cell_Volume_agegroups_summarysd$sd <- sqrt(Mean_Cell_Volume_agegroups_summarysd$lbxmcvsi)


# View the results
print(Mean_Cell_Volume_agegroups_summary)
print(Mean_Cell_Volume_agegroups_summarysd)
print(Mean_Cell_Volume_agegroups_range)


# Calculate the mean and standard deviation of Mean_Cell_Volume by age_deciles
Mean_Cell_Volume_race <- svyby(~lbxmcvsi, ~factor(RACE_RECODE), design = nhanesDesign, svymean)
Mean_Cell_Volume_race_sd <- svyby(~lbxmcvsi, ~factor(RACE_RECODE), design = nhanesDesign, svyvar)
Mean_Cell_Volume_race_range <- svyby(~lbxmcvsi, ~RACE_RECODE, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_race_summary <- data.frame(Mean_Cell_Volume_race)
Mean_Cell_Volume_race_summarysd<-data.frame(Mean_Cell_Volume_race_sd)
Mean_Cell_Volume_race_summarysd$sd <- sqrt(Mean_Cell_Volume_race_summarysd$lbxmcvsi)

# Print
Mean_Cell_Volume_race_summary
Mean_Cell_Volume_race_summarysd
Mean_Cell_Volume_race_range


# Calculate the mean and standard deviation of Mean_Cell_Volume by age_deciles
Mean_Cell_Volume_sex <- svyby(~lbxmcvsi, ~factor(riagendr), design = nhanesDesign, svymean)
Mean_Cell_Volume_sex_sd <- svyby(~lbxmcvsi, ~factor(riagendr), design = nhanesDesign, svyvar)
Mean_Cell_Volume_sex_range <- svyby(~lbxmcvsi, ~riagendr, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_sex_summary <- data.frame(Mean_Cell_Volume_sex)
Mean_Cell_Volume_sex_summarysd<-data.frame(Mean_Cell_Volume_sex_sd)
Mean_Cell_Volume_sex_summarysd$sd <- sqrt(Mean_Cell_Volume_sex_summarysd$lbxmcvsi)

#Print
Mean_Cell_Volume_sex_summary
Mean_Cell_Volume_sex_summarysd
Mean_Cell_Volume_sex_range


# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Mean_Cell_Volume_BMI_Groups <- svyby(~lbxmcvsi, ~factor(BMI_Groups), design = nhanesDesign, svymean)
Mean_Cell_Volume_BMI_Groups_sd <- svyby(~lbxmcvsi, ~factor(BMI_Groups), design = nhanesDesign, svyvar)
Mean_Cell_Volume_BMI_Groups_range <- svyby(~lbxmcvsi, ~BMI_Groups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_BMI_Groups_summary <- data.frame(Mean_Cell_Volume_BMI_Groups)
Mean_Cell_Volume_BMI_Groups_summarysd<-data.frame(Mean_Cell_Volume_BMI_Groups_sd)
Mean_Cell_Volume_BMI_Groups_summarysd$sd <- sqrt(Mean_Cell_Volume_BMI_Groups_summarysd$lbxmcvsi)

# Print
Mean_Cell_Volume_BMI_Groups_summary
Mean_Cell_Volume_BMI_Groups_summarysd
Mean_Cell_Volume_BMI_Groups_range


# Weighted Frequency table for Education
svytable(~dmdeduc2, nhanesDesign)
# Less than 9th grade (N=4651)	87898201.3	6.066%
# 9-11th grade (Includes 12th grade with no diploma) (N=5402)	159491596.8	11.006%
# High school graduate/GED or equivalent (N=8537)	358733876	24.756%
# Some college or AA degree (N=10183)	444474864	30.673%
# College graduate or above (N=7559)	396998727.5	27.396%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Mean_Cell_Volume_Education <- svyby(~lbxmcvsi, ~factor(dmdeduc2), design = nhanesDesign, svymean)
Mean_Cell_Volume_Education_sd <- svyby(~lbxmcvsi, ~factor(dmdeduc2), design = nhanesDesign, svyvar)
Mean_Cell_Volume_Education_range <- svyby(~lbxmcvsi, ~dmdeduc2, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_Education_summary <- data.frame(Mean_Cell_Volume_Education)
Mean_Cell_Volume_Education_summarysd<-data.frame(Mean_Cell_Volume_Education_sd)
Mean_Cell_Volume_Education_summarysd$sd <- sqrt(Mean_Cell_Volume_Education_summarysd$lbxmcvsi)

# View the results
print(Mean_Cell_Volume_Education_summary)
Mean_Cell_Volume_Education_summarysd
Mean_Cell_Volume_Education_range



# Weighted Frequency table for Income
table_weighted_income<-svytable(~familyincomeannual, nhanesDesign)
tabledataframe<-data.frame(table_weighted_income)
total_freq <- sum(tabledataframe$Freq)
tabledataframe$Percentage <- (tabledataframe$Freq/total_freq)*100
tabledataframe$Percentage <- round(tabledataframe$Percentage, 2)

#   familyincomeannual      Freq Percentage

#             $ <5000  28398917       2.14%
#         10000-15000  92076718       6.92%
#          15000-2000  83075022       6.25%
#         20000-25000 108780678       8.18%
#         25000-35000 136452422      10.26%
#         35000-45000 145722842      10.96%
#         45000-55000 131348748       9.87%
#          5000-10000  48284605       3.63%
#        55000-65000 101404308       7.62%
#        65000-75000  88542639       6.66%
#       75000-100000  70246203       5.28%
#             >100000 295801899      22.24%
Mean_Cell_Volume_familyincomeannual <- svyby(~lbxmcvsi, ~factor(FamilyIncomeRecode), design = nhanesDesign, svymean)
Mean_Cell_Volume_familyincomeannual_sd <- svyby(~lbxmcvsi, ~factor(FamilyIncomeRecode), design = nhanesDesign, svyvar)
Mean_Cell_Volume_familyincomeannual_range <- svyby(~lbxmcvsi, ~FamilyIncomeRecode, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_familyincomeannual_summary <- data.frame(Mean_Cell_Volume_familyincomeannual)
Mean_Cell_Volume_familyincomeannual_summarysd<-data.frame(Mean_Cell_Volume_familyincomeannual_sd)
Mean_Cell_Volume_familyincomeannual_summarysd$sd <- sqrt(Mean_Cell_Volume_familyincomeannual_summarysd$lbxmcvsi)

# View the results
print(Mean_Cell_Volume_familyincomeannual_summary)
Mean_Cell_Volume_familyincomeannual_summarysd
Mean_Cell_Volume_familyincomeannual_range


# Weighted Frequency table for Alcohol
table_weighted_alcohol<-svytable(~Alcohol_Any_Categories, nhanesDesign)
tabledataframe_alcohol<-data.frame(table_weighted_alcohol)
total_freq <- sum(tabledataframe_alcohol$Freq)
tabledataframe_alcohol$Percentage <- (tabledataframe_alcohol$Freq/total_freq)*100
tabledataframe_alcohol$Percentage <- round(tabledataframe_alcohol$Percentage, 2) 
tabledataframe_alcohol
# Comparison of a numeric variable by a grouping variable
# Alcohol_Any_Categories      Freq Percentage
#          Not a Drinker 249130415      25.33
#         Heavy Drinker 133341354      13.56
#        Regular Drinker 601083385      61.11

Mean_Cell_Volume_Alcohol <- svyby(~lbxmcvsi, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svymean)
Mean_Cell_Volume_Alcohol_sd <- svyby(~lbxmcvsi, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svyvar)
Mean_Cell_Volume_Alcohol_range <- svyby(~lbxmcvsi, ~Alcohol_Any_Categories, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_Alcohol_summary <- data.frame(Mean_Cell_Volume_Alcohol)
Mean_Cell_Volume_Alcohol_summarysd<-data.frame(Mean_Cell_Volume_Alcohol_sd)
Mean_Cell_Volume_Alcohol_summarysd$sd <- sqrt(Mean_Cell_Volume_Alcohol_summarysd$lbxmcvsi)

# View the results
print(Mean_Cell_Volume_Alcohol_summary)
Mean_Cell_Volume_Alcohol_summarysd
Mean_Cell_Volume_Alcohol_range


# Weighted Frequency table for Smoking
table_weighted_SmokingStatus<-svytable(~SmokingStatus, nhanesDesign)
tabledataframe_SmokingStatus<-data.frame(table_weighted_SmokingStatus)
total_freq <- sum(tabledataframe_SmokingStatus$Freq)
tabledataframe_SmokingStatus$Percentage <- (tabledataframe_SmokingStatus$Freq/total_freq)*100
tabledataframe_SmokingStatus$Percentage <- round(tabledataframe_SmokingStatus$Percentage, 2) 
tabledataframe_SmokingStatus
#    SmokingStatus  Freq Percentage
# Current Smoker 316496128      21.76%
#  Former Smoker 362187774      24.90%
#   Never Smoker 775650383      53.3%
Mean_Cell_Volume_SmokingStatus <- svyby(~lbxmcvsi, ~factor(SmokingStatus), design = nhanesDesign, svymean)
Mean_Cell_Volume_SmokingStatus_sd <- svyby(~lbxmcvsi, ~factor(SmokingStatus), design = nhanesDesign, svyvar)
Mean_Cell_Volume_SmokingStatus_range <- svyby(~lbxmcvsi, ~SmokingStatus, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_SmokingStatus_summary <- data.frame(Mean_Cell_Volume_SmokingStatus)
Mean_Cell_Volume_SmokingStatus_summarysd<-data.frame(Mean_Cell_Volume_SmokingStatus_sd)
Mean_Cell_Volume_SmokingStatus_summarysd$sd <- sqrt(Mean_Cell_Volume_SmokingStatus_summarysd$lbxmcvsi)


# View the results
print(Mean_Cell_Volume_SmokingStatus_summary)
Mean_Cell_Volume_SmokingStatus_summarysd
Mean_Cell_Volume_SmokingStatus_range



# Weighted Frequency table for Diabetes
table_weighted_Diabetes<-svytable(~Diabetes, nhanesDesign)
tabledataframe_Diabetes<-data.frame(table_weighted_Diabetes)
total_freq <- sum(tabledataframe_Diabetes$Freq)
tabledataframe_Diabetes$Percentage <- (tabledataframe_Diabetes$Freq/total_freq)*100
tabledataframe_Diabetes$Percentage <- round(tabledataframe_Diabetes$Percentage, 2) 
tabledataframe_Diabetes

#Diabetes       Freq Percentage
#                No 1322298535      91.39%
#               Yes  124521799       8.61%
Mean_Cell_Volume_Diabetes <- svyby(~lbxmcvsi, ~factor(Diabetes), design = nhanesDesign, svymean)
Mean_Cell_Volume_Diabetes_sd <- svyby(~lbxmcvsi, ~factor(Diabetes), design = nhanesDesign, svyvar)
Mean_Cell_Volume_Diabetes_range <- svyby(~lbxmcvsi, ~Diabetes, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation int

Mean_Cell_Volume_Diabetes_summary <- data.frame(Mean_Cell_Volume_Diabetes)
Mean_Cell_Volume_Diabetes_summarysd<-data.frame(Mean_Cell_Volume_Diabetes_sd)
Mean_Cell_Volume_Diabetes_summarysd$sd <- sqrt(Mean_Cell_Volume_Diabetes_summarysd$lbxmcvsi)

# View the results
print(Mean_Cell_Volume_Diabetes_summary)
Mean_Cell_Volume_Diabetes_summarysd
Mean_Cell_Volume_Diabetes_range



# Weighted Frequency table for Hypertension
table_weighted_Hypertension<-svytable(~Hypertension, nhanesDesign)
tabledataframe_Hypertension<-data.frame(table_weighted_Hypertension)
total_freq <- sum(tabledataframe_Hypertension$Freq)
tabledataframe_Hypertension$Percentage <- (tabledataframe_Hypertension$Freq/total_freq)*100
tabledataframe_Hypertension$Percentage <- round(tabledataframe_Hypertension$Percentage, 2) 
tabledataframe_Hypertension
#Hypertension       Freq Percentage
#1                      0 1125223737      84.25
#2                      1  210355103      15.75

Mean_Cell_Volume_Hypertension <- svyby(~lbxmcvsi, ~factor(Hypertension), design = nhanesDesign, svymean)
Mean_Cell_Volume_Hypertension_sd <- svyby(~lbxmcvsi, ~factor(Hypertension), design = nhanesDesign, svyvar)
Mean_Cell_Volume_Hypertension_range <- svyby(~lbxmcvsi, ~Hypertension, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_Hypertension_summary <- data.frame(Mean_Cell_Volume_Hypertension)
Mean_Cell_Volume_Hypertension_summarysd<-data.frame(Mean_Cell_Volume_Hypertension_sd)
Mean_Cell_Volume_Hypertension_summarysd$sd <- sqrt(Mean_Cell_Volume_Hypertension_summarysd$lbxmcvsi)

# View the results
print(Mean_Cell_Volume_Hypertension_summary)
Mean_Cell_Volume_Hypertension_summarysd
Mean_Cell_Volume_Hypertension_range


# Weighted Frequency table for Dyslipidemia
table_weighted_Dyslipidemia<-svytable(~Dyslipidemia, nhanesDesign)
tabledataframe_Dyslipidemia<-data.frame(table_weighted_Dyslipidemia)
total_freq <- sum(tabledataframe_Dyslipidemia$Freq)
tabledataframe_Dyslipidemia$Percentage <- (tabledataframe_Dyslipidemia$Freq/total_freq)*100
tabledataframe_Dyslipidemia$Percentage <- round(tabledataframe_Dyslipidemia$Percentage, 2) 
tabledataframe_Dyslipidemia
#Dyslipidemia       Freq Percentage
#1            0 1213253354      82.45
#2            1  258237153      17.55

Mean_Cell_Volume_Dyslipidemia <- svyby(~lbxmcvsi, ~factor(Dyslipidemia), design = nhanesDesign, svymean)
Mean_Cell_Volume_Dyslipidemia_sd <- svyby(~lbxmcvsi, ~factor(Dyslipidemia), design = nhanesDesign, svyvar)
Mean_Cell_Volume_Dyslipidemia_range <- svyby(~lbxmcvsi, ~Dyslipidemia, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_Dyslipidemia_summary <- data.frame(Mean_Cell_Volume_Dyslipidemia)
Mean_Cell_Volume_Dyslipidemia_summarysd<-data.frame(Mean_Cell_Volume_Dyslipidemia_sd)
Mean_Cell_Volume_Dyslipidemia_summarysd$sd <- sqrt(Mean_Cell_Volume_Dyslipidemia_summarysd$lbxmcvsi)

# Print
Mean_Cell_Volume_Dyslipidemia_summary
Mean_Cell_Volume_Dyslipidemia_summarysd
Mean_Cell_Volume_Dyslipidemia_range



# Weighted Frequency table for Cardiac Disease
table_weighted_HeartDisease<-svytable(~HeartDisease, nhanesDesign)
tabledataframe_HeartDisease<-data.frame(table_weighted_HeartDisease)
total_freq <- sum(tabledataframe_HeartDisease$Freq)
tabledataframe_HeartDisease$Percentage <- (tabledataframe_HeartDisease$Freq/total_freq)*100
tabledataframe_HeartDisease$Percentage <- round(tabledataframe_HeartDisease$Percentage, 2) 
tabledataframe_HeartDisease
#HeartDisease       Freq Percentage
#1            0 1352032083      93.3
#2            1  97031973      6.7

Mean_Cell_Volume_HeartDisease <- svyby(~lbxmcvsi, ~factor(HeartDisease), design = nhanesDesign, svymean)
Mean_Cell_Volume_HeartDisease_sd <- svyby(~lbxmcvsi, ~factor(HeartDisease), design = nhanesDesign, svyvar)
Mean_Cell_Volume_HeartDisease_range <- svyby(~lbxmcvsi, ~HeartDisease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_HeartDisease_summary <- data.frame(Mean_Cell_Volume_HeartDisease)
Mean_Cell_Volume_HeartDisease_summarysd<-data.frame(Mean_Cell_Volume_HeartDisease_sd)
Mean_Cell_Volume_HeartDisease_summarysd$sd <- sqrt(Mean_Cell_Volume_HeartDisease_summarysd$lbxmcvsi)

#Print
Mean_Cell_Volume_HeartDisease_summary
Mean_Cell_Volume_HeartDisease_summarysd
Mean_Cell_Volume_HeartDisease_range



# Weighted Frequency table for Cancer
table_weighted_Cancer<-svytable(~mcq220, nhanesDesign)
tabledataframe_Cancer<-data.frame(table_weighted_Cancer)
total_freq <- sum(tabledataframe_Cancer$Freq)
tabledataframe_Cancer$Percentage <- (tabledataframe_Cancer$Freq/total_freq)*100
tabledataframe_Cancer$Percentage <- round(tabledataframe_Cancer$Percentage, 2) 
tabledataframe_Cancer
#Cancer       Freq Percentage
#1            0 1314831945      9.15
#2            1  132562783      90.74

Mean_Cell_Volume_Cancer <- svyby(~lbxmcvsi, ~factor(mcq220), design = nhanesDesign, svymean)
Mean_Cell_Volume_Cancer_sd <- svyby(~lbxmcvsi, ~factor(mcq220), design = nhanesDesign, svyvar)
Mean_Cell_Volume_Cancer_range <- svyby(~lbxmcvsi, ~mcq220, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_Cancer_summary <- data.frame(Mean_Cell_Volume_Cancer)
Mean_Cell_Volume_Cancer_summarysd<-data.frame(Mean_Cell_Volume_Cancer_sd)
Mean_Cell_Volume_Cancer_summarysd$sd <- sqrt(Mean_Cell_Volume_Cancer_summarysd$lbxmcvsi)

# Print
Mean_Cell_Volume_Cancer_summary
Mean_Cell_Volume_Cancer_summarysd
Mean_Cell_Volume_Cancer_range



# Weighted Frequency table for Any_Pulmonary_Disease
table_weighted_Any_Pulmonary_Disease<-svytable(~Any_Pulmonary_Disease, nhanesDesign)
tabledataframe_Any_Pulmonary_Disease<-data.frame(table_weighted_Any_Pulmonary_Disease)
total_freq <- sum(tabledataframe_Any_Pulmonary_Disease$Freq)
tabledataframe_Any_Pulmonary_Disease$Percentage <- (tabledataframe_Any_Pulmonary_Disease$Freq/total_freq)*100
tabledataframe_Any_Pulmonary_Disease$Percentage <- round(tabledataframe_Any_Pulmonary_Disease$Percentage, 2) 
tabledataframe_Any_Pulmonary_Disease
#Any_Pulmonary_Disease       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Mean_Cell_Volume_Any_Pulmonary_Disease <- svyby(~lbxmcvsi, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svymean)
Mean_Cell_Volume_Any_Pulmonary_Disease_sd <- svyby(~lbxmcvsi, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svyvar)
Mean_Cell_Volume_Any_Pulmonary_Disease_range <- svyby(~lbxmcvsi, ~Any_Pulmonary_Disease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_Any_Pulmonary_Disease_summary <- data.frame(Mean_Cell_Volume_Any_Pulmonary_Disease)
Mean_Cell_Volume_Any_Pulmonary_Disease_summarysd<-data.frame(Mean_Cell_Volume_Any_Pulmonary_Disease_sd)
Mean_Cell_Volume_Any_Pulmonary_Disease_summarysd$sd <- sqrt(Mean_Cell_Volume_Any_Pulmonary_Disease_summarysd$lbxmcvsi)

#Print
Mean_Cell_Volume_Any_Pulmonary_Disease_summary
Mean_Cell_Volume_Any_Pulmonary_Disease_summarysd
Mean_Cell_Volume_Any_Pulmonary_Disease_range



# Weighted Frequency table for VIGOROUS_ACTIVITY
table_weighted_VIGOROUS_ACTIVITY<-svytable(~VIGOROUS_ACTIVITY, nhanesDesign)
tabledataframe_VIGOROUS_ACTIVITY<-data.frame(table_weighted_VIGOROUS_ACTIVITY)
total_freq <- sum(tabledataframe_VIGOROUS_ACTIVITY$Freq)
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- (tabledataframe_VIGOROUS_ACTIVITY$Freq/total_freq)*100
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- round(tabledataframe_VIGOROUS_ACTIVITY$Percentage, 2) 
tabledataframe_VIGOROUS_ACTIVITY
#VIGOROUS_ACTIVITY       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Mean_Cell_Volume_VIGOROUS_ACTIVITY <- svyby(~lbxmcvsi, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svymean)
Mean_Cell_Volume_VIGOROUS_ACTIVITY_sd <- svyby(~lbxmcvsi, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svyvar)
Mean_Cell_Volume_VIGOROUS_ACTIVITY_range <- svyby(~lbxmcvsi, ~VIGOROUS_ACTIVITY, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Mean_Cell_Volume_VIGOROUS_ACTIVITY_summary <- data.frame(Mean_Cell_Volume_VIGOROUS_ACTIVITY)
Mean_Cell_Volume_VIGOROUS_ACTIVITY_summarysd<-data.frame(Mean_Cell_Volume_VIGOROUS_ACTIVITY_sd)
Mean_Cell_Volume_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(Mean_Cell_Volume_VIGOROUS_ACTIVITY_summarysd$lbxmcvsi)

#Print
Mean_Cell_Volume_VIGOROUS_ACTIVITY_summary
Mean_Cell_Volume_VIGOROUS_ACTIVITY_summarysd
Mean_Cell_Volume_VIGOROUS_ACTIVITY_range



# Alkaline_Phosphatase

### Alkaline_Phosphatase
# Calculate the mean and standard deviation of Alkaline_Phosphatase by age_deciles
Alkaline_Phosphatase_agegroups <- svyby(~lbdsapsi233, ~factor(age_deciles), design = nhanesDesign, svymean)
Alkaline_Phosphatase_agegroups_sd <- svyby(~lbdsapsi233, ~factor(age_deciles), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_agegroups_range <- svyby(~lbdsapsi233, ~age_deciles, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_agegroups_summary <- data.frame(Alkaline_Phosphatase_agegroups)
Alkaline_Phosphatase_agegroups_summarysd<-data.frame(Alkaline_Phosphatase_agegroups_sd)
Alkaline_Phosphatase_agegroups_summarysd$sd <- sqrt(Alkaline_Phosphatase_agegroups_summarysd$lbdsapsi233)


# View the results
print(Alkaline_Phosphatase_agegroups_summary)
print(Alkaline_Phosphatase_agegroups_summarysd)
print(Alkaline_Phosphatase_agegroups_range)



# Calculate the mean and standard deviation of Alkaline_Phosphatase by age_deciles
Alkaline_Phosphatase_race <- svyby(~lbdsapsi233, ~factor(RACE_RECODE), design = nhanesDesign, svymean)
Alkaline_Phosphatase_race_sd <- svyby(~lbdsapsi233, ~factor(RACE_RECODE), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_race_range <- svyby(~lbdsapsi233, ~RACE_RECODE, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_race_summary <- data.frame(Alkaline_Phosphatase_race)
Alkaline_Phosphatase_race_summarysd<-data.frame(Alkaline_Phosphatase_race_sd)
Alkaline_Phosphatase_race_summarysd$sd <- sqrt(Alkaline_Phosphatase_race_summarysd$lbdsapsi233)

# Print
Alkaline_Phosphatase_race_summary
Alkaline_Phosphatase_race_summarysd
Alkaline_Phosphatase_race_range


# Calculate the mean and standard deviation of Alkaline_Phosphatase by age_deciles
Alkaline_Phosphatase_sex <- svyby(~lbdsapsi233, ~factor(riagendr), design = nhanesDesign, svymean)
Alkaline_Phosphatase_sex_sd <- svyby(~lbdsapsi233, ~factor(riagendr), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_sex_range <- svyby(~lbdsapsi233, ~riagendr, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_sex_summary <- data.frame(Alkaline_Phosphatase_sex)
Alkaline_Phosphatase_sex_summarysd<-data.frame(Alkaline_Phosphatase_sex_sd)
Alkaline_Phosphatase_sex_summarysd$sd <- sqrt(Alkaline_Phosphatase_sex_summarysd$lbdsapsi233)

#Print
Alkaline_Phosphatase_sex_summary
Alkaline_Phosphatase_sex_summarysd
Alkaline_Phosphatase_sex_range


# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Alkaline_Phosphatase_BMI_Groups <- svyby(~lbdsapsi233, ~factor(BMI_Groups), design = nhanesDesign, svymean)
Alkaline_Phosphatase_BMI_Groups_sd <- svyby(~lbdsapsi233, ~factor(BMI_Groups), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_BMI_Groups_range <- svyby(~lbdsapsi233, ~BMI_Groups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_BMI_Groups_summary <- data.frame(Alkaline_Phosphatase_BMI_Groups)
Alkaline_Phosphatase_BMI_Groups_summarysd<-data.frame(Alkaline_Phosphatase_BMI_Groups_sd)
Alkaline_Phosphatase_BMI_Groups_summarysd$sd <- sqrt(Alkaline_Phosphatase_BMI_Groups_summarysd$lbdsapsi233)

# Print
Alkaline_Phosphatase_BMI_Groups_summary
Alkaline_Phosphatase_BMI_Groups_summarysd
Alkaline_Phosphatase_BMI_Groups_range


# Weighted Frequency table for Education
svytable(~dmdeduc2, nhanesDesign)
# Less than 9th grade (N=4651)	87898201.3	6.066%
# 9-11th grade (Includes 12th grade with no diploma) (N=5402)	159491596.8	11.006%
# High school graduate/GED or equivalent (N=8537)	358733876	24.756%
# Some college or AA degree (N=10183)	444474864	30.673%
# College graduate or above (N=7559)	396998727.5	27.396%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Alkaline_Phosphatase_Education <- svyby(~lbdsapsi233, ~factor(dmdeduc2), design = nhanesDesign, svymean)
Alkaline_Phosphatase_Education_sd <- svyby(~lbdsapsi233, ~factor(dmdeduc2), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_Education_range <- svyby(~lbdsapsi233, ~dmdeduc2, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_Education_summary <- data.frame(Alkaline_Phosphatase_Education)
Alkaline_Phosphatase_Education_summarysd<-data.frame(Alkaline_Phosphatase_Education_sd)
Alkaline_Phosphatase_Education_summarysd$sd <- sqrt(Alkaline_Phosphatase_Education_summarysd$lbdsapsi233)

# View the results
print(Alkaline_Phosphatase_Education_summary)
Alkaline_Phosphatase_Education_summarysd
Alkaline_Phosphatase_Education_range


# Weighted Frequency table for Income
table_weighted_income<-svytable(~familyincomeannual, nhanesDesign)
tabledataframe<-data.frame(table_weighted_income)
total_freq <- sum(tabledataframe$Freq)
tabledataframe$Percentage <- (tabledataframe$Freq/total_freq)*100
tabledataframe$Percentage <- round(tabledataframe$Percentage, 2)

#   familyincomeannual      Freq Percentage

#             $ <5000  28398917       2.14%
#         10000-15000  92076718       6.92%
#          15000-2000  83075022       6.25%
#         20000-25000 108780678       8.18%
#         25000-35000 136452422      10.26%
#         35000-45000 145722842      10.96%
#         45000-55000 131348748       9.87%
#          5000-10000  48284605       3.63%
#        55000-65000 101404308       7.62%
#        65000-75000  88542639       6.66%
#       75000-100000  70246203       5.28%
#             >100000 295801899      22.24%
Alkaline_Phosphatase_familyincomeannual <- svyby(~lbdsapsi233, ~factor(FamilyIncomeRecode), design = nhanesDesign, svymean)
Alkaline_Phosphatase_familyincomeannual_sd <- svyby(~lbdsapsi233, ~factor(FamilyIncomeRecode), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_familyincomeannual_range <- svyby(~lbdsapsi233, ~FamilyIncomeRecode, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_familyincomeannual_summary <- data.frame(Alkaline_Phosphatase_familyincomeannual)
Alkaline_Phosphatase_familyincomeannual_summarysd<-data.frame(Alkaline_Phosphatase_familyincomeannual_sd)
Alkaline_Phosphatase_familyincomeannual_summarysd$sd <- sqrt(Alkaline_Phosphatase_familyincomeannual_summarysd$lbdsapsi233)

# View the results
print(Alkaline_Phosphatase_familyincomeannual_summary)
Alkaline_Phosphatase_familyincomeannual_summarysd
Alkaline_Phosphatase_familyincomeannual_range


# Weighted Frequency table for Alcohol
table_weighted_alcohol<-svytable(~Alcohol_Any_Categories, nhanesDesign)
tabledataframe_alcohol<-data.frame(table_weighted_alcohol)
total_freq <- sum(tabledataframe_alcohol$Freq)
tabledataframe_alcohol$Percentage <- (tabledataframe_alcohol$Freq/total_freq)*100
tabledataframe_alcohol$Percentage <- round(tabledataframe_alcohol$Percentage, 2) 
tabledataframe_alcohol
# Comparison of a numeric variable by a grouping variable
# Alcohol_Any_Categories      Freq Percentage
#          Not a Drinker 249130415      25.33
#         Heavy Drinker 133341354      13.56
#        Regular Drinker 601083385      61.11

Alkaline_Phosphatase_Alcohol <- svyby(~lbdsapsi233, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svymean)
Alkaline_Phosphatase_Alcohol_sd <- svyby(~lbdsapsi233, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_Alcohol_range <- svyby(~lbdsapsi233, ~Alcohol_Any_Categories, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_Alcohol_summary <- data.frame(Alkaline_Phosphatase_Alcohol)
Alkaline_Phosphatase_Alcohol_summarysd<-data.frame(Alkaline_Phosphatase_Alcohol_sd)
Alkaline_Phosphatase_Alcohol_summarysd$sd <- sqrt(Alkaline_Phosphatase_Alcohol_summarysd$lbdsapsi233)

# View the results
print(Alkaline_Phosphatase_Alcohol_summary)
Alkaline_Phosphatase_Alcohol_summarysd
Alkaline_Phosphatase_Alcohol_range


# Weighted Frequency table for Smoking
table_weighted_SmokingStatus<-svytable(~SmokingStatus, nhanesDesign)
tabledataframe_SmokingStatus<-data.frame(table_weighted_SmokingStatus)
total_freq <- sum(tabledataframe_SmokingStatus$Freq)
tabledataframe_SmokingStatus$Percentage <- (tabledataframe_SmokingStatus$Freq/total_freq)*100
tabledataframe_SmokingStatus$Percentage <- round(tabledataframe_SmokingStatus$Percentage, 2) 
tabledataframe_SmokingStatus
#    SmokingStatus  Freq Percentage
# Current Smoker 316496128      21.76%
#  Former Smoker 362187774      24.90%
#   Never Smoker 775650383      53.3%
Alkaline_Phosphatase_SmokingStatus <- svyby(~lbdsapsi233, ~factor(SmokingStatus), design = nhanesDesign, svymean)
Alkaline_Phosphatase_SmokingStatus_sd <- svyby(~lbdsapsi233, ~factor(SmokingStatus), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_SmokingStatus_range <- svyby(~lbdsapsi233, ~SmokingStatus, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_SmokingStatus_summary <- data.frame(Alkaline_Phosphatase_SmokingStatus)
Alkaline_Phosphatase_SmokingStatus_summarysd<-data.frame(Alkaline_Phosphatase_SmokingStatus_sd)
Alkaline_Phosphatase_SmokingStatus_summarysd$sd <- sqrt(Alkaline_Phosphatase_SmokingStatus_summarysd$lbdsapsi233)


# View the results
print(Alkaline_Phosphatase_SmokingStatus_summary)
Alkaline_Phosphatase_SmokingStatus_summarysd
Alkaline_Phosphatase_SmokingStatus_range



# Weighted Frequency table for Diabetes
table_weighted_Diabetes<-svytable(~Diabetes, nhanesDesign)
tabledataframe_Diabetes<-data.frame(table_weighted_Diabetes)
total_freq <- sum(tabledataframe_Diabetes$Freq)
tabledataframe_Diabetes$Percentage <- (tabledataframe_Diabetes$Freq/total_freq)*100
tabledataframe_Diabetes$Percentage <- round(tabledataframe_Diabetes$Percentage, 2) 
tabledataframe_Diabetes

#Diabetes       Freq Percentage
#                No 1322298535      91.39%
#               Yes  124521799       8.61%
Alkaline_Phosphatase_Diabetes <- svyby(~lbdsapsi233, ~factor(Diabetes), design = nhanesDesign, svymean)
Alkaline_Phosphatase_Diabetes_sd <- svyby(~lbdsapsi233, ~factor(Diabetes), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_Diabetes_range <- svyby(~lbdsapsi233, ~Diabetes, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation int

Alkaline_Phosphatase_Diabetes_summary <- data.frame(Alkaline_Phosphatase_Diabetes)
Alkaline_Phosphatase_Diabetes_summarysd<-data.frame(Alkaline_Phosphatase_Diabetes_sd)
Alkaline_Phosphatase_Diabetes_summarysd$sd <- sqrt(Alkaline_Phosphatase_Diabetes_summarysd$lbdsapsi233)

# View the results
print(Alkaline_Phosphatase_Diabetes_summary)
Alkaline_Phosphatase_Diabetes_summarysd
Alkaline_Phosphatase_Diabetes_range



# Weighted Frequency table for Hypertension
table_weighted_Hypertension<-svytable(~Hypertension, nhanesDesign)
tabledataframe_Hypertension<-data.frame(table_weighted_Hypertension)
total_freq <- sum(tabledataframe_Hypertension$Freq)
tabledataframe_Hypertension$Percentage <- (tabledataframe_Hypertension$Freq/total_freq)*100
tabledataframe_Hypertension$Percentage <- round(tabledataframe_Hypertension$Percentage, 2) 
tabledataframe_Hypertension
#Hypertension       Freq Percentage
#1                      0 1125223737      84.25
#2                      1  210355103      15.75

Alkaline_Phosphatase_Hypertension <- svyby(~lbdsapsi233, ~factor(Hypertension), design = nhanesDesign, svymean)
Alkaline_Phosphatase_Hypertension_sd <- svyby(~lbdsapsi233, ~factor(Hypertension), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_Hypertension_range <- svyby(~lbdsapsi233, ~Hypertension, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_Hypertension_summary <- data.frame(Alkaline_Phosphatase_Hypertension)
Alkaline_Phosphatase_Hypertension_summarysd<-data.frame(Alkaline_Phosphatase_Hypertension_sd)
Alkaline_Phosphatase_Hypertension_summarysd$sd <- sqrt(Alkaline_Phosphatase_Hypertension_summarysd$lbdsapsi233)

# View the results
print(Alkaline_Phosphatase_Hypertension_summary)
Alkaline_Phosphatase_Hypertension_summarysd
Alkaline_Phosphatase_Hypertension_range


# Weighted Frequency table for Dyslipidemia
table_weighted_Dyslipidemia<-svytable(~Dyslipidemia, nhanesDesign)
tabledataframe_Dyslipidemia<-data.frame(table_weighted_Dyslipidemia)
total_freq <- sum(tabledataframe_Dyslipidemia$Freq)
tabledataframe_Dyslipidemia$Percentage <- (tabledataframe_Dyslipidemia$Freq/total_freq)*100
tabledataframe_Dyslipidemia$Percentage <- round(tabledataframe_Dyslipidemia$Percentage, 2) 
tabledataframe_Dyslipidemia
#Dyslipidemia       Freq Percentage
#1            0 1213253354      82.45
#2            1  258237153      17.55

Alkaline_Phosphatase_Dyslipidemia <- svyby(~lbdsapsi233, ~factor(Dyslipidemia), design = nhanesDesign, svymean)
Alkaline_Phosphatase_Dyslipidemia_sd <- svyby(~lbdsapsi233, ~factor(Dyslipidemia), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_Dyslipidemia_range <- svyby(~lbdsapsi233, ~Dyslipidemia, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_Dyslipidemia_summary <- data.frame(Alkaline_Phosphatase_Dyslipidemia)
Alkaline_Phosphatase_Dyslipidemia_summarysd<-data.frame(Alkaline_Phosphatase_Dyslipidemia_sd)
Alkaline_Phosphatase_Dyslipidemia_summarysd$sd <- sqrt(Alkaline_Phosphatase_Dyslipidemia_summarysd$lbdsapsi233)

# Print
Alkaline_Phosphatase_Dyslipidemia_summary
Alkaline_Phosphatase_Dyslipidemia_summarysd
Alkaline_Phosphatase_Dyslipidemia_range



# Weighted Frequency table for Cardiac Disease
table_weighted_HeartDisease<-svytable(~HeartDisease, nhanesDesign)
tabledataframe_HeartDisease<-data.frame(table_weighted_HeartDisease)
total_freq <- sum(tabledataframe_HeartDisease$Freq)
tabledataframe_HeartDisease$Percentage <- (tabledataframe_HeartDisease$Freq/total_freq)*100
tabledataframe_HeartDisease$Percentage <- round(tabledataframe_HeartDisease$Percentage, 2) 
tabledataframe_HeartDisease
#HeartDisease       Freq Percentage
#1            0 1352032083      93.3
#2            1  97031973      6.7

Alkaline_Phosphatase_HeartDisease <- svyby(~lbdsapsi233, ~factor(HeartDisease), design = nhanesDesign, svymean)
Alkaline_Phosphatase_HeartDisease_sd <- svyby(~lbdsapsi233, ~factor(HeartDisease), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_HeartDisease_range <- svyby(~lbdsapsi233, ~HeartDisease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_HeartDisease_summary <- data.frame(Alkaline_Phosphatase_HeartDisease)
Alkaline_Phosphatase_HeartDisease_summarysd<-data.frame(Alkaline_Phosphatase_HeartDisease_sd)
Alkaline_Phosphatase_HeartDisease_summarysd$sd <- sqrt(Alkaline_Phosphatase_HeartDisease_summarysd$lbdsapsi233)

#Print
Alkaline_Phosphatase_HeartDisease_summary
Alkaline_Phosphatase_HeartDisease_summarysd
Alkaline_Phosphatase_HeartDisease_range



# Weighted Frequency table for Cancer
table_weighted_Cancer<-svytable(~mcq220, nhanesDesign)
tabledataframe_Cancer<-data.frame(table_weighted_Cancer)
total_freq <- sum(tabledataframe_Cancer$Freq)
tabledataframe_Cancer$Percentage <- (tabledataframe_Cancer$Freq/total_freq)*100
tabledataframe_Cancer$Percentage <- round(tabledataframe_Cancer$Percentage, 2) 
tabledataframe_Cancer
#Cancer       Freq Percentage
#1            0 1314831945      9.15
#2            1  132562783      90.74

Alkaline_Phosphatase_Cancer <- svyby(~lbdsapsi233, ~factor(mcq220), design = nhanesDesign, svymean)
Alkaline_Phosphatase_Cancer_sd <- svyby(~lbdsapsi233, ~factor(mcq220), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_Cancer_range <- svyby(~lbdsapsi233, ~mcq220, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_Cancer_summary <- data.frame(Alkaline_Phosphatase_Cancer)
Alkaline_Phosphatase_Cancer_summarysd<-data.frame(Alkaline_Phosphatase_Cancer_sd)
Alkaline_Phosphatase_Cancer_summarysd$sd <- sqrt(Alkaline_Phosphatase_Cancer_summarysd$lbdsapsi233)

# Print
Alkaline_Phosphatase_Cancer_summary
Alkaline_Phosphatase_Cancer_summarysd
Alkaline_Phosphatase_Cancer_range



# Weighted Frequency table for Any_Pulmonary_Disease
table_weighted_Any_Pulmonary_Disease<-svytable(~Any_Pulmonary_Disease, nhanesDesign)
tabledataframe_Any_Pulmonary_Disease<-data.frame(table_weighted_Any_Pulmonary_Disease)
total_freq <- sum(tabledataframe_Any_Pulmonary_Disease$Freq)
tabledataframe_Any_Pulmonary_Disease$Percentage <- (tabledataframe_Any_Pulmonary_Disease$Freq/total_freq)*100
tabledataframe_Any_Pulmonary_Disease$Percentage <- round(tabledataframe_Any_Pulmonary_Disease$Percentage, 2) 
tabledataframe_Any_Pulmonary_Disease
#Any_Pulmonary_Disease       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Alkaline_Phosphatase_Any_Pulmonary_Disease <- svyby(~lbdsapsi233, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svymean)
Alkaline_Phosphatase_Any_Pulmonary_Disease_sd <- svyby(~lbdsapsi233, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_Any_Pulmonary_Disease_range <- svyby(~lbdsapsi233, ~Any_Pulmonary_Disease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_Any_Pulmonary_Disease_summary <- data.frame(Alkaline_Phosphatase_Any_Pulmonary_Disease)
Alkaline_Phosphatase_Any_Pulmonary_Disease_summarysd<-data.frame(Alkaline_Phosphatase_Any_Pulmonary_Disease_sd)
Alkaline_Phosphatase_Any_Pulmonary_Disease_summarysd$sd <- sqrt(Alkaline_Phosphatase_Any_Pulmonary_Disease_summarysd$lbdsapsi233)

#Print
Alkaline_Phosphatase_Any_Pulmonary_Disease_summary
Alkaline_Phosphatase_Any_Pulmonary_Disease_summarysd
Alkaline_Phosphatase_Any_Pulmonary_Disease_range



# Weighted Frequency table for VIGOROUS_ACTIVITY
table_weighted_VIGOROUS_ACTIVITY<-svytable(~VIGOROUS_ACTIVITY, nhanesDesign)
tabledataframe_VIGOROUS_ACTIVITY<-data.frame(table_weighted_VIGOROUS_ACTIVITY)
total_freq <- sum(tabledataframe_VIGOROUS_ACTIVITY$Freq)
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- (tabledataframe_VIGOROUS_ACTIVITY$Freq/total_freq)*100
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- round(tabledataframe_VIGOROUS_ACTIVITY$Percentage, 2) 
tabledataframe_VIGOROUS_ACTIVITY
#VIGOROUS_ACTIVITY       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Alkaline_Phosphatase_VIGOROUS_ACTIVITY <- svyby(~lbdsapsi233, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svymean)
Alkaline_Phosphatase_VIGOROUS_ACTIVITY_sd <- svyby(~lbdsapsi233, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svyvar)
Alkaline_Phosphatase_VIGOROUS_ACTIVITY_range <- svyby(~lbdsapsi233, ~VIGOROUS_ACTIVITY, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Alkaline_Phosphatase_VIGOROUS_ACTIVITY_summary <- data.frame(Alkaline_Phosphatase_VIGOROUS_ACTIVITY)
Alkaline_Phosphatase_VIGOROUS_ACTIVITY_summarysd<-data.frame(Alkaline_Phosphatase_VIGOROUS_ACTIVITY_sd)
Alkaline_Phosphatase_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(Alkaline_Phosphatase_VIGOROUS_ACTIVITY_summarysd$lbdsapsi233)

#Print
Alkaline_Phosphatase_VIGOROUS_ACTIVITY_summary
Alkaline_Phosphatase_VIGOROUS_ACTIVITY_summarysd
Alkaline_Phosphatase_VIGOROUS_ACTIVITY_range





# Rec_Cell_Dist_Width

### Rec_Cell_Dist_Width
# Calculate the mean and standard deviation of Rec_Cell_Dist_Width by age_deciles
Rec_Cell_Dist_Width_agegroups <- svyby(~lbxrdw, ~factor(age_deciles), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_agegroups_sd <- svyby(~lbxrdw, ~factor(age_deciles), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_agegroups_range <- svyby(~lbxrdw, ~age_deciles, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_agegroups_summary <- data.frame(Rec_Cell_Dist_Width_agegroups)
Rec_Cell_Dist_Width_agegroups_summarysd<-data.frame(Rec_Cell_Dist_Width_agegroups_sd)
Rec_Cell_Dist_Width_agegroups_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_agegroups_summarysd$lbxrdw)


# View the results
print(Rec_Cell_Dist_Width_agegroups_summary)
print(Rec_Cell_Dist_Width_agegroups_summarysd)
print(Rec_Cell_Dist_Width_agegroups_range)


# Calculate the mean and standard deviation of Rec_Cell_Dist_Width by age_deciles
Rec_Cell_Dist_Width_race <- svyby(~lbxrdw, ~factor(RACE_RECODE), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_race_sd <- svyby(~lbxrdw, ~factor(RACE_RECODE), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_race_range <- svyby(~lbxrdw, ~RACE_RECODE, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_race_summary <- data.frame(Rec_Cell_Dist_Width_race)
Rec_Cell_Dist_Width_race_summarysd<-data.frame(Rec_Cell_Dist_Width_race_sd)
Rec_Cell_Dist_Width_race_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_race_summarysd$lbxrdw)

# Print
Rec_Cell_Dist_Width_race_summary
Rec_Cell_Dist_Width_race_summarysd
Rec_Cell_Dist_Width_race_range


# Calculate the mean and standard deviation of Rec_Cell_Dist_Width by age_deciles
Rec_Cell_Dist_Width_sex <- svyby(~lbxrdw, ~factor(riagendr), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_sex_sd <- svyby(~lbxrdw, ~factor(riagendr), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_sex_range <- svyby(~lbxrdw, ~riagendr, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_sex_summary <- data.frame(Rec_Cell_Dist_Width_sex)
Rec_Cell_Dist_Width_sex_summarysd<-data.frame(Rec_Cell_Dist_Width_sex_sd)
Rec_Cell_Dist_Width_sex_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_sex_summarysd$lbxrdw)

#Print
Rec_Cell_Dist_Width_sex_summary
Rec_Cell_Dist_Width_sex_summarysd
Rec_Cell_Dist_Width_sex_range


# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Rec_Cell_Dist_Width_BMI_Groups <- svyby(~lbxrdw, ~factor(BMI_Groups), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_BMI_Groups_sd <- svyby(~lbxrdw, ~factor(BMI_Groups), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_BMI_Groups_range <- svyby(~lbxrdw, ~BMI_Groups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_BMI_Groups_summary <- data.frame(Rec_Cell_Dist_Width_BMI_Groups)
Rec_Cell_Dist_Width_BMI_Groups_summarysd<-data.frame(Rec_Cell_Dist_Width_BMI_Groups_sd)
Rec_Cell_Dist_Width_BMI_Groups_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_BMI_Groups_summarysd$lbxrdw)

# Print
Rec_Cell_Dist_Width_BMI_Groups_summary
Rec_Cell_Dist_Width_BMI_Groups_summarysd
Rec_Cell_Dist_Width_BMI_Groups_range



# Weighted Frequency table for Education
svytable(~dmdeduc2, nhanesDesign)
# Less than 9th grade (N=4651)	87898201.3	6.066%
# 9-11th grade (Includes 12th grade with no diploma) (N=5402)	159491596.8	11.006%
# High school graduate/GED or equivalent (N=8537)	358733876	24.756%
# Some college or AA degree (N=10183)	444474864	30.673%
# College graduate or above (N=7559)	396998727.5	27.396%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Rec_Cell_Dist_Width_Education <- svyby(~lbxrdw, ~factor(dmdeduc2), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_Education_sd <- svyby(~lbxrdw, ~factor(dmdeduc2), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_Education_range <- svyby(~lbxrdw, ~dmdeduc2, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_Education_summary <- data.frame(Rec_Cell_Dist_Width_Education)
Rec_Cell_Dist_Width_Education_summarysd<-data.frame(Rec_Cell_Dist_Width_Education_sd)
Rec_Cell_Dist_Width_Education_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_Education_summarysd$lbxrdw)

# View the results
print(Rec_Cell_Dist_Width_Education_summary)
Rec_Cell_Dist_Width_Education_summarysd
Rec_Cell_Dist_Width_Education_range



# Weighted Frequency table for Income
table_weighted_income<-svytable(~familyincomeannual, nhanesDesign)
tabledataframe<-data.frame(table_weighted_income)
total_freq <- sum(tabledataframe$Freq)
tabledataframe$Percentage <- (tabledataframe$Freq/total_freq)*100
tabledataframe$Percentage <- round(tabledataframe$Percentage, 2)

#   familyincomeannual      Freq Percentage

#             $ <5000  28398917       2.14%
#         10000-15000  92076718       6.92%
#          15000-2000  83075022       6.25%
#         20000-25000 108780678       8.18%
#         25000-35000 136452422      10.26%
#         35000-45000 145722842      10.96%
#         45000-55000 131348748       9.87%
#          5000-10000  48284605       3.63%
#        55000-65000 101404308       7.62%
#        65000-75000  88542639       6.66%
#       75000-100000  70246203       5.28%
#             >100000 295801899      22.24%
Rec_Cell_Dist_Width_familyincomeannual <- svyby(~lbxrdw, ~factor(FamilyIncomeRecode), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_familyincomeannual_sd <- svyby(~lbxrdw, ~factor(FamilyIncomeRecode), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_familyincomeannual_range <- svyby(~lbxrdw, ~FamilyIncomeRecode, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_familyincomeannual_summary <- data.frame(Rec_Cell_Dist_Width_familyincomeannual)
Rec_Cell_Dist_Width_familyincomeannual_summarysd<-data.frame(Rec_Cell_Dist_Width_familyincomeannual_sd)
Rec_Cell_Dist_Width_familyincomeannual_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_familyincomeannual_summarysd$lbxrdw)

# View the results
print(Rec_Cell_Dist_Width_familyincomeannual_summary)
Rec_Cell_Dist_Width_familyincomeannual_summarysd
Rec_Cell_Dist_Width_familyincomeannual_range


# Weighted Frequency table for Alcohol
table_weighted_alcohol<-svytable(~Alcohol_Any_Categories, nhanesDesign)
tabledataframe_alcohol<-data.frame(table_weighted_alcohol)
total_freq <- sum(tabledataframe_alcohol$Freq)
tabledataframe_alcohol$Percentage <- (tabledataframe_alcohol$Freq/total_freq)*100
tabledataframe_alcohol$Percentage <- round(tabledataframe_alcohol$Percentage, 2) 
tabledataframe_alcohol
# Comparison of a numeric variable by a grouping variable
# Alcohol_Any_Categories      Freq Percentage
#          Not a Drinker 249130415      25.33
#         Heavy Drinker 133341354      13.56
#        Regular Drinker 601083385      61.11

Rec_Cell_Dist_Width_Alcohol <- svyby(~lbxrdw, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_Alcohol_sd <- svyby(~lbxrdw, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_Alcohol_range <- svyby(~lbxrdw, ~Alcohol_Any_Categories, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_Alcohol_summary <- data.frame(Rec_Cell_Dist_Width_Alcohol)
Rec_Cell_Dist_Width_Alcohol_summarysd<-data.frame(Rec_Cell_Dist_Width_Alcohol_sd)
Rec_Cell_Dist_Width_Alcohol_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_Alcohol_summarysd$lbxrdw)

# View the results
print(Rec_Cell_Dist_Width_Alcohol_summary)
Rec_Cell_Dist_Width_Alcohol_summarysd
Rec_Cell_Dist_Width_Alcohol_range


# Weighted Frequency table for Smoking
table_weighted_SmokingStatus<-svytable(~SmokingStatus, nhanesDesign)
tabledataframe_SmokingStatus<-data.frame(table_weighted_SmokingStatus)
total_freq <- sum(tabledataframe_SmokingStatus$Freq)
tabledataframe_SmokingStatus$Percentage <- (tabledataframe_SmokingStatus$Freq/total_freq)*100
tabledataframe_SmokingStatus$Percentage <- round(tabledataframe_SmokingStatus$Percentage, 2) 
tabledataframe_SmokingStatus
#    SmokingStatus  Freq Percentage
# Current Smoker 316496128      21.76%
#  Former Smoker 362187774      24.90%
#   Never Smoker 775650383      53.3%
Rec_Cell_Dist_Width_SmokingStatus <- svyby(~lbxrdw, ~factor(SmokingStatus), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_SmokingStatus_sd <- svyby(~lbxrdw, ~factor(SmokingStatus), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_SmokingStatus_range <- svyby(~lbxrdw, ~SmokingStatus, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_SmokingStatus_summary <- data.frame(Rec_Cell_Dist_Width_SmokingStatus)
Rec_Cell_Dist_Width_SmokingStatus_summarysd<-data.frame(Rec_Cell_Dist_Width_SmokingStatus_sd)
Rec_Cell_Dist_Width_SmokingStatus_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_SmokingStatus_summarysd$lbxrdw)


# View the results
print(Rec_Cell_Dist_Width_SmokingStatus_summary)
Rec_Cell_Dist_Width_SmokingStatus_summarysd
Rec_Cell_Dist_Width_SmokingStatus_range



# Weighted Frequency table for Diabetes
table_weighted_Diabetes<-svytable(~Diabetes, nhanesDesign)
tabledataframe_Diabetes<-data.frame(table_weighted_Diabetes)
total_freq <- sum(tabledataframe_Diabetes$Freq)
tabledataframe_Diabetes$Percentage <- (tabledataframe_Diabetes$Freq/total_freq)*100
tabledataframe_Diabetes$Percentage <- round(tabledataframe_Diabetes$Percentage, 2) 
tabledataframe_Diabetes

#Diabetes       Freq Percentage
#                No 1322298535      91.39%
#               Yes  124521799       8.61%
Rec_Cell_Dist_Width_Diabetes <- svyby(~lbxrdw, ~factor(Diabetes), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_Diabetes_sd <- svyby(~lbxrdw, ~factor(Diabetes), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_Diabetes_range <- svyby(~lbxrdw, ~Diabetes, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation int

Rec_Cell_Dist_Width_Diabetes_summary <- data.frame(Rec_Cell_Dist_Width_Diabetes)
Rec_Cell_Dist_Width_Diabetes_summarysd<-data.frame(Rec_Cell_Dist_Width_Diabetes_sd)
Rec_Cell_Dist_Width_Diabetes_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_Diabetes_summarysd$lbxrdw)

# View the results
print(Rec_Cell_Dist_Width_Diabetes_summary)
Rec_Cell_Dist_Width_Diabetes_summarysd
Rec_Cell_Dist_Width_Diabetes_range



# Weighted Frequency table for Hypertension
table_weighted_Hypertension<-svytable(~Hypertension, nhanesDesign)
tabledataframe_Hypertension<-data.frame(table_weighted_Hypertension)
total_freq <- sum(tabledataframe_Hypertension$Freq)
tabledataframe_Hypertension$Percentage <- (tabledataframe_Hypertension$Freq/total_freq)*100
tabledataframe_Hypertension$Percentage <- round(tabledataframe_Hypertension$Percentage, 2) 
tabledataframe_Hypertension
#Hypertension       Freq Percentage
#1                      0 1125223737      84.25
#2                      1  210355103      15.75

Rec_Cell_Dist_Width_Hypertension <- svyby(~lbxrdw, ~factor(Hypertension), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_Hypertension_sd <- svyby(~lbxrdw, ~factor(Hypertension), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_Hypertension_range <- svyby(~lbxrdw, ~Hypertension, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_Hypertension_summary <- data.frame(Rec_Cell_Dist_Width_Hypertension)
Rec_Cell_Dist_Width_Hypertension_summarysd<-data.frame(Rec_Cell_Dist_Width_Hypertension_sd)
Rec_Cell_Dist_Width_Hypertension_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_Hypertension_summarysd$lbxrdw)

# View the results
print(Rec_Cell_Dist_Width_Hypertension_summary)
Rec_Cell_Dist_Width_Hypertension_summarysd
Rec_Cell_Dist_Width_Hypertension_range


# Weighted Frequency table for Dyslipidemia
table_weighted_Dyslipidemia<-svytable(~Dyslipidemia, nhanesDesign)
tabledataframe_Dyslipidemia<-data.frame(table_weighted_Dyslipidemia)
total_freq <- sum(tabledataframe_Dyslipidemia$Freq)
tabledataframe_Dyslipidemia$Percentage <- (tabledataframe_Dyslipidemia$Freq/total_freq)*100
tabledataframe_Dyslipidemia$Percentage <- round(tabledataframe_Dyslipidemia$Percentage, 2) 
tabledataframe_Dyslipidemia
#Dyslipidemia       Freq Percentage
#1            0 1213253354      82.45
#2            1  258237153      17.55

Rec_Cell_Dist_Width_Dyslipidemia <- svyby(~lbxrdw, ~factor(Dyslipidemia), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_Dyslipidemia_sd <- svyby(~lbxrdw, ~factor(Dyslipidemia), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_Dyslipidemia_range <- svyby(~lbxrdw, ~Dyslipidemia, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_Dyslipidemia_summary <- data.frame(Rec_Cell_Dist_Width_Dyslipidemia)
Rec_Cell_Dist_Width_Dyslipidemia_summarysd<-data.frame(Rec_Cell_Dist_Width_Dyslipidemia_sd)
Rec_Cell_Dist_Width_Dyslipidemia_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_Dyslipidemia_summarysd$lbxrdw)

# Print
Rec_Cell_Dist_Width_Dyslipidemia_summary
Rec_Cell_Dist_Width_Dyslipidemia_summarysd
Rec_Cell_Dist_Width_Dyslipidemia_range



# Weighted Frequency table for Cardiac Disease
table_weighted_HeartDisease<-svytable(~HeartDisease, nhanesDesign)
tabledataframe_HeartDisease<-data.frame(table_weighted_HeartDisease)
total_freq <- sum(tabledataframe_HeartDisease$Freq)
tabledataframe_HeartDisease$Percentage <- (tabledataframe_HeartDisease$Freq/total_freq)*100
tabledataframe_HeartDisease$Percentage <- round(tabledataframe_HeartDisease$Percentage, 2) 
tabledataframe_HeartDisease
#HeartDisease       Freq Percentage
#1            0 1352032083      93.3
#2            1  97031973      6.7

Rec_Cell_Dist_Width_HeartDisease <- svyby(~lbxrdw, ~factor(HeartDisease), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_HeartDisease_sd <- svyby(~lbxrdw, ~factor(HeartDisease), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_HeartDisease_range <- svyby(~lbxrdw, ~HeartDisease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_HeartDisease_summary <- data.frame(Rec_Cell_Dist_Width_HeartDisease)
Rec_Cell_Dist_Width_HeartDisease_summarysd<-data.frame(Rec_Cell_Dist_Width_HeartDisease_sd)
Rec_Cell_Dist_Width_HeartDisease_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_HeartDisease_summarysd$lbxrdw)

#Print
Rec_Cell_Dist_Width_HeartDisease_summary
Rec_Cell_Dist_Width_HeartDisease_summarysd
Rec_Cell_Dist_Width_HeartDisease_range



# Weighted Frequency table for Cancer
table_weighted_Cancer<-svytable(~mcq220, nhanesDesign)
tabledataframe_Cancer<-data.frame(table_weighted_Cancer)
total_freq <- sum(tabledataframe_Cancer$Freq)
tabledataframe_Cancer$Percentage <- (tabledataframe_Cancer$Freq/total_freq)*100
tabledataframe_Cancer$Percentage <- round(tabledataframe_Cancer$Percentage, 2) 
tabledataframe_Cancer
#Cancer       Freq Percentage
#1            0 1314831945      9.15
#2            1  132562783      90.74

Rec_Cell_Dist_Width_Cancer <- svyby(~lbxrdw, ~factor(mcq220), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_Cancer_sd <- svyby(~lbxrdw, ~factor(mcq220), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_Cancer_range <- svyby(~lbxrdw, ~mcq220, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_Cancer_summary <- data.frame(Rec_Cell_Dist_Width_Cancer)
Rec_Cell_Dist_Width_Cancer_summarysd<-data.frame(Rec_Cell_Dist_Width_Cancer_sd)
Rec_Cell_Dist_Width_Cancer_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_Cancer_summarysd$lbxrdw)

# Print
Rec_Cell_Dist_Width_Cancer_summary
Rec_Cell_Dist_Width_Cancer_summarysd
Rec_Cell_Dist_Width_Cancer_range



# Weighted Frequency table for Any_Pulmonary_Disease
table_weighted_Any_Pulmonary_Disease<-svytable(~Any_Pulmonary_Disease, nhanesDesign)
tabledataframe_Any_Pulmonary_Disease<-data.frame(table_weighted_Any_Pulmonary_Disease)
total_freq <- sum(tabledataframe_Any_Pulmonary_Disease$Freq)
tabledataframe_Any_Pulmonary_Disease$Percentage <- (tabledataframe_Any_Pulmonary_Disease$Freq/total_freq)*100
tabledataframe_Any_Pulmonary_Disease$Percentage <- round(tabledataframe_Any_Pulmonary_Disease$Percentage, 2) 
tabledataframe_Any_Pulmonary_Disease
#Any_Pulmonary_Disease       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Rec_Cell_Dist_Width_Any_Pulmonary_Disease <- svyby(~lbxrdw, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_Any_Pulmonary_Disease_sd <- svyby(~lbxrdw, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_Any_Pulmonary_Disease_range <- svyby(~lbxrdw, ~Any_Pulmonary_Disease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_Any_Pulmonary_Disease_summary <- data.frame(Rec_Cell_Dist_Width_Any_Pulmonary_Disease)
Rec_Cell_Dist_Width_Any_Pulmonary_Disease_summarysd<-data.frame(Rec_Cell_Dist_Width_Any_Pulmonary_Disease_sd)
Rec_Cell_Dist_Width_Any_Pulmonary_Disease_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_Any_Pulmonary_Disease_summarysd$lbxrdw)

#Print
Rec_Cell_Dist_Width_Any_Pulmonary_Disease_summary
Rec_Cell_Dist_Width_Any_Pulmonary_Disease_summarysd
Rec_Cell_Dist_Width_Any_Pulmonary_Disease_range



# Weighted Frequency table for VIGOROUS_ACTIVITY
table_weighted_VIGOROUS_ACTIVITY<-svytable(~VIGOROUS_ACTIVITY, nhanesDesign)
tabledataframe_VIGOROUS_ACTIVITY<-data.frame(table_weighted_VIGOROUS_ACTIVITY)
total_freq <- sum(tabledataframe_VIGOROUS_ACTIVITY$Freq)
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- (tabledataframe_VIGOROUS_ACTIVITY$Freq/total_freq)*100
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- round(tabledataframe_VIGOROUS_ACTIVITY$Percentage, 2) 
tabledataframe_VIGOROUS_ACTIVITY
#VIGOROUS_ACTIVITY       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY <- svyby(~lbxrdw, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svymean)
Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY_sd <- svyby(~lbxrdw, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svyvar)
Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY_range <- svyby(~lbxrdw, ~VIGOROUS_ACTIVITY, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY_summary <- data.frame(Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY)
Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY_summarysd<-data.frame(Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY_sd)
Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY_summarysd$lbxrdw)

#Print
Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY_summary
Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY_summarysd
Rec_Cell_Dist_Width_VIGOROUS_ACTIVITY_range


# WBC

### WBC
# Calculate the mean and standard deviation of WBC by age_deciles
WBC_agegroups <- svyby(~lbxwbcsi, ~factor(age_deciles), design = nhanesDesign, svymean)
WBC_agegroups_sd <- svyby(~lbxwbcsi, ~factor(age_deciles), design = nhanesDesign, svyvar)
WBC_agegroups_range <- svyby(~lbxwbcsi, ~age_deciles, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_agegroups_summary <- data.frame(WBC_agegroups)
# Calculate standard deviation
WBC_agegroups_sd$sd <- sqrt(WBC_agegroups_sd$lbxwbcsi)

# Print the result
print(WBC_agegroups_sd)

# View the results
print(WBC_agegroups_summary)
print(WBC_agegroups_summarysd)
print(WBC_agegroups_range)


# Calculate the mean and standard deviation of WBC by age_deciles
WBC_race <- svyby(~lbxwbcsi, ~factor(RACE_RECODE), design = nhanesDesign, svymean)
WBC_race_sd <- svyby(~lbxwbcsi, ~factor(RACE_RECODE), design = nhanesDesign, svyvar)
WBC_race_range <- svyby(~lbxwbcsi, ~RACE_RECODE, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_race_summary <- data.frame(WBC_race)
WBC_race_summarysd<-data.frame(WBC_race_sd)
WBC_race_summarysd$sd <- sqrt(WBC_race_summarysd$lbxwbcsi)

# Print
WBC_race_summary
WBC_race_summarysd
WBC_race_range


# Calculate the mean and standard deviation of WBC by age_deciles
WBC_sex <- svyby(~lbxwbcsi, ~factor(riagendr), design = nhanesDesign, svymean)
WBC_sex_sd <- svyby(~lbxwbcsi, ~factor(riagendr), design = nhanesDesign, svyvar)
WBC_sex_range <- svyby(~lbxwbcsi, ~riagendr, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_sex_summary <- data.frame(WBC_sex)
WBC_sex_summarysd<-data.frame(WBC_sex_sd)
WBC_sex_summarysd$sd <- sqrt(WBC_sex_summarysd$lbxwbcsi)

#Print
WBC_sex_summary
WBC_sex_summarysd
WBC_sex_range


# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
WBC_BMI_Groups <- svyby(~lbxwbcsi, ~factor(BMI_Groups), design = nhanesDesign, svymean)
WBC_BMI_Groups_sd <- svyby(~lbxwbcsi, ~factor(BMI_Groups), design = nhanesDesign, svyvar)
WBC_BMI_Groups_range <- svyby(~lbxwbcsi, ~BMI_Groups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_BMI_Groups_summary <- data.frame(WBC_BMI_Groups)
WBC_BMI_Groups_summarysd<-data.frame(WBC_BMI_Groups_sd)
WBC_BMI_Groups_summarysd$sd <- sqrt(WBC_BMI_Groups_summarysd$lbxwbcsi)

# Print
WBC_BMI_Groups_summary
WBC_BMI_Groups_summarysd
WBC_BMI_Groups_range



# Weighted Frequency table for Education
svytable(~dmdeduc2, nhanesDesign)
# Less than 9th grade (N=4651)	87898201.3	6.066%
# 9-11th grade (Includes 12th grade with no diploma) (N=5402)	159491596.8	11.006%
# High school graduate/GED or equivalent (N=8537)	358733876	24.756%
# Some college or AA degree (N=10183)	444474864	30.673%
# College graduate or above (N=7559)	396998727.5	27.396%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
WBC_Education <- svyby(~lbxwbcsi, ~factor(dmdeduc2), design = nhanesDesign, svymean)
WBC_Education_sd <- svyby(~lbxwbcsi, ~factor(dmdeduc2), design = nhanesDesign, svyvar)
WBC_Education_range <- svyby(~lbxwbcsi, ~dmdeduc2, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_Education_summary <- data.frame(WBC_Education)
WBC_Education_summarysd<-data.frame(WBC_Education_sd)
WBC_Education_summarysd$sd <- sqrt(WBC_Education_summarysd$lbxwbcsi)

# View the results
print(WBC_Education_summary)
WBC_Education_summarysd
WBC_Education_range



# Weighted Frequency table for Income
table_weighted_income<-svytable(~familyincomeannual, nhanesDesign)
tabledataframe<-data.frame(table_weighted_income)
total_freq <- sum(tabledataframe$Freq)
tabledataframe$Percentage <- (tabledataframe$Freq/total_freq)*100
tabledataframe$Percentage <- round(tabledataframe$Percentage, 2)

#   familyincomeannual      Freq Percentage

#             $ <5000  28398917       2.14%
#         10000-15000  92076718       6.92%
#          15000-2000  83075022       6.25%
#         20000-25000 108780678       8.18%
#         25000-35000 136452422      10.26%
#         35000-45000 145722842      10.96%
#         45000-55000 131348748       9.87%
#          5000-10000  48284605       3.63%
#        55000-65000 101404308       7.62%
#        65000-75000  88542639       6.66%
#       75000-100000  70246203       5.28%
#             >100000 295801899      22.24%
WBC_familyincomeannual <- svyby(~lbxwbcsi, ~factor(FamilyIncomeRecode), design = nhanesDesign, svymean)
WBC_familyincomeannual_sd <- svyby(~lbxwbcsi, ~factor(FamilyIncomeRecode), design = nhanesDesign, svyvar)
WBC_familyincomeannual_range <- svyby(~lbxwbcsi, ~FamilyIncomeRecode, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_familyincomeannual_summary <- data.frame(WBC_familyincomeannual)
WBC_familyincomeannual_summarysd<-data.frame(WBC_familyincomeannual_sd)
WBC_familyincomeannual_summarysd$sd <- sqrt(WBC_familyincomeannual_summarysd$lbxwbcsi)

# View the results
print(WBC_familyincomeannual_summary)
WBC_familyincomeannual_summarysd
WBC_familyincomeannual_range


# Weighted Frequency table for Alcohol
table_weighted_alcohol<-svytable(~Alcohol_Any_Categories, nhanesDesign)
tabledataframe_alcohol<-data.frame(table_weighted_alcohol)
total_freq <- sum(tabledataframe_alcohol$Freq)
tabledataframe_alcohol$Percentage <- (tabledataframe_alcohol$Freq/total_freq)*100
tabledataframe_alcohol$Percentage <- round(tabledataframe_alcohol$Percentage, 2) 
tabledataframe_alcohol
# Comparison of a numeric variable by a grouping variable
# Alcohol_Any_Categories      Freq Percentage
#          Not a Drinker 249130415      25.33
#         Heavy Drinker 133341354      13.56
#        Regular Drinker 601083385      61.11

WBC_Alcohol <- svyby(~lbxwbcsi, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svymean)
WBC_Alcohol_sd <- svyby(~lbxwbcsi, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svyvar)
WBC_Alcohol_range <- svyby(~lbxwbcsi, ~Alcohol_Any_Categories, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_Alcohol_summary <- data.frame(WBC_Alcohol)
WBC_Alcohol_summarysd<-data.frame(WBC_Alcohol_sd)
WBC_Alcohol_summarysd$sd <- sqrt(WBC_Alcohol_summarysd$lbxwbcsi)

# View the results
print(WBC_Alcohol_summary)
WBC_Alcohol_summarysd
WBC_Alcohol_range


# Weighted Frequency table for Smoking
table_weighted_SmokingStatus<-svytable(~SmokingStatus, nhanesDesign)
tabledataframe_SmokingStatus<-data.frame(table_weighted_SmokingStatus)
total_freq <- sum(tabledataframe_SmokingStatus$Freq)
tabledataframe_SmokingStatus$Percentage <- (tabledataframe_SmokingStatus$Freq/total_freq)*100
tabledataframe_SmokingStatus$Percentage <- round(tabledataframe_SmokingStatus$Percentage, 2) 
tabledataframe_SmokingStatus
#    SmokingStatus  Freq Percentage
# Current Smoker 316496128      21.76%
#  Former Smoker 362187774      24.90%
#   Never Smoker 775650383      53.3%
WBC_SmokingStatus <- svyby(~lbxwbcsi, ~factor(SmokingStatus), design = nhanesDesign, svymean)
WBC_SmokingStatus_sd <- svyby(~lbxwbcsi, ~factor(SmokingStatus), design = nhanesDesign, svyvar)
WBC_SmokingStatus_range <- svyby(~lbxwbcsi, ~SmokingStatus, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_SmokingStatus_summary <- data.frame(WBC_SmokingStatus)
WBC_SmokingStatus_summarysd<-data.frame(WBC_SmokingStatus_sd)
WBC_SmokingStatus_summarysd$sd <- sqrt(WBC_SmokingStatus_summarysd$lbxwbcsi)


# View the results
print(WBC_SmokingStatus_summary)
WBC_SmokingStatus_summarysd
WBC_SmokingStatus_range



# Weighted Frequency table for Diabetes
table_weighted_Diabetes<-svytable(~Diabetes, nhanesDesign)
tabledataframe_Diabetes<-data.frame(table_weighted_Diabetes)
total_freq <- sum(tabledataframe_Diabetes$Freq)
tabledataframe_Diabetes$Percentage <- (tabledataframe_Diabetes$Freq/total_freq)*100
tabledataframe_Diabetes$Percentage <- round(tabledataframe_Diabetes$Percentage, 2) 
tabledataframe_Diabetes

#Diabetes       Freq Percentage
#                No 1322298535      91.39%
#               Yes  124521799       8.61%
WBC_Diabetes <- svyby(~lbxwbcsi, ~factor(Diabetes), design = nhanesDesign, svymean)
WBC_Diabetes_sd <- svyby(~lbxwbcsi, ~factor(Diabetes), design = nhanesDesign, svyvar)
WBC_Diabetes_range <- svyby(~lbxwbcsi, ~Diabetes, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation int

WBC_Diabetes_summary <- data.frame(WBC_Diabetes)
WBC_Diabetes_summarysd<-data.frame(WBC_Diabetes_sd)
WBC_Diabetes_summarysd$sd <- sqrt(WBC_Diabetes_summarysd$lbxwbcsi)

# View the results
print(WBC_Diabetes_summary)
WBC_Diabetes_summarysd
WBC_Diabetes_range



# Weighted Frequency table for Hypertension
table_weighted_Hypertension<-svytable(~Hypertension, nhanesDesign)
tabledataframe_Hypertension<-data.frame(table_weighted_Hypertension)
total_freq <- sum(tabledataframe_Hypertension$Freq)
tabledataframe_Hypertension$Percentage <- (tabledataframe_Hypertension$Freq/total_freq)*100
tabledataframe_Hypertension$Percentage <- round(tabledataframe_Hypertension$Percentage, 2) 
tabledataframe_Hypertension
#Hypertension       Freq Percentage
#1                      0 1125223737      84.25
#2                      1  210355103      15.75

WBC_Hypertension <- svyby(~lbxwbcsi, ~factor(Hypertension), design = nhanesDesign, svymean)
WBC_Hypertension_sd <- svyby(~lbxwbcsi, ~factor(Hypertension), design = nhanesDesign, svyvar)
WBC_Hypertension_range <- svyby(~lbxwbcsi, ~Hypertension, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_Hypertension_summary <- data.frame(WBC_Hypertension)
WBC_Hypertension_summarysd<-data.frame(WBC_Hypertension_sd)
WBC_Hypertension_summarysd$sd <- sqrt(WBC_Hypertension_summarysd$lbxwbcsi)

# View the results
print(WBC_Hypertension_summary)
WBC_Hypertension_summarysd
WBC_Hypertension_range


# Weighted Frequency table for Dyslipidemia
table_weighted_Dyslipidemia<-svytable(~Dyslipidemia, nhanesDesign)
tabledataframe_Dyslipidemia<-data.frame(table_weighted_Dyslipidemia)
total_freq <- sum(tabledataframe_Dyslipidemia$Freq)
tabledataframe_Dyslipidemia$Percentage <- (tabledataframe_Dyslipidemia$Freq/total_freq)*100
tabledataframe_Dyslipidemia$Percentage <- round(tabledataframe_Dyslipidemia$Percentage, 2) 
tabledataframe_Dyslipidemia
#Dyslipidemia       Freq Percentage
#1            0 1213253354      82.45
#2            1  258237153      17.55

WBC_Dyslipidemia <- svyby(~lbxwbcsi, ~factor(Dyslipidemia), design = nhanesDesign, svymean)
WBC_Dyslipidemia_sd <- svyby(~lbxwbcsi, ~factor(Dyslipidemia), design = nhanesDesign, svyvar)
WBC_Dyslipidemia_range <- svyby(~lbxwbcsi, ~Dyslipidemia, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_Dyslipidemia_summary <- data.frame(WBC_Dyslipidemia)
WBC_Dyslipidemia_summarysd<-data.frame(WBC_Dyslipidemia_sd)
WBC_Dyslipidemia_summarysd$sd <- sqrt(WBC_Dyslipidemia_summarysd$lbxwbcsi)

# Print
WBC_Dyslipidemia_summary
WBC_Dyslipidemia_summarysd
WBC_Dyslipidemia_range



# Weighted Frequency table for Cardiac Disease
table_weighted_HeartDisease<-svytable(~HeartDisease, nhanesDesign)
tabledataframe_HeartDisease<-data.frame(table_weighted_HeartDisease)
total_freq <- sum(tabledataframe_HeartDisease$Freq)
tabledataframe_HeartDisease$Percentage <- (tabledataframe_HeartDisease$Freq/total_freq)*100
tabledataframe_HeartDisease$Percentage <- round(tabledataframe_HeartDisease$Percentage, 2) 
tabledataframe_HeartDisease
#HeartDisease       Freq Percentage
#1            0 1352032083      93.3
#2            1  97031973      6.7

WBC_HeartDisease <- svyby(~lbxwbcsi, ~factor(HeartDisease), design = nhanesDesign, svymean)
WBC_HeartDisease_sd <- svyby(~lbxwbcsi, ~factor(HeartDisease), design = nhanesDesign, svyvar)
WBC_HeartDisease_range <- svyby(~lbxwbcsi, ~HeartDisease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_HeartDisease_summary <- data.frame(WBC_HeartDisease)
WBC_HeartDisease_summarysd<-data.frame(WBC_HeartDisease_sd)
WBC_HeartDisease_summarysd$sd <- sqrt(WBC_HeartDisease_summarysd$lbxwbcsi)

#Print
WBC_HeartDisease_summary
WBC_HeartDisease_summarysd
WBC_HeartDisease_range



# Weighted Frequency table for Cancer
table_weighted_Cancer<-svytable(~mcq220, nhanesDesign)
tabledataframe_Cancer<-data.frame(table_weighted_Cancer)
total_freq <- sum(tabledataframe_Cancer$Freq)
tabledataframe_Cancer$Percentage <- (tabledataframe_Cancer$Freq/total_freq)*100
tabledataframe_Cancer$Percentage <- round(tabledataframe_Cancer$Percentage, 2) 
tabledataframe_Cancer
#Cancer       Freq Percentage
#1            0 1314831945      9.15
#2            1  132562783      90.74

WBC_Cancer <- svyby(~lbxwbcsi, ~factor(mcq220), design = nhanesDesign, svymean)
WBC_Cancer_sd <- svyby(~lbxwbcsi, ~factor(mcq220), design = nhanesDesign, svyvar)
WBC_Cancer_range <- svyby(~lbxwbcsi, ~mcq220, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_Cancer_summary <- data.frame(WBC_Cancer)
WBC_Cancer_summarysd<-data.frame(WBC_Cancer_sd)
WBC_Cancer_summarysd$sd <- sqrt(WBC_Cancer_summarysd$lbxwbcsi)

# Print
WBC_Cancer_summary
WBC_Cancer_summarysd
WBC_Cancer_range



# Weighted Frequency table for Any_Pulmonary_Disease
table_weighted_Any_Pulmonary_Disease<-svytable(~Any_Pulmonary_Disease, nhanesDesign)
tabledataframe_Any_Pulmonary_Disease<-data.frame(table_weighted_Any_Pulmonary_Disease)
total_freq <- sum(tabledataframe_Any_Pulmonary_Disease$Freq)
tabledataframe_Any_Pulmonary_Disease$Percentage <- (tabledataframe_Any_Pulmonary_Disease$Freq/total_freq)*100
tabledataframe_Any_Pulmonary_Disease$Percentage <- round(tabledataframe_Any_Pulmonary_Disease$Percentage, 2) 
tabledataframe_Any_Pulmonary_Disease
#Any_Pulmonary_Disease       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

WBC_Any_Pulmonary_Disease <- svyby(~lbxwbcsi, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svymean)
WBC_Any_Pulmonary_Disease_sd <- svyby(~lbxwbcsi, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svyvar)
WBC_Any_Pulmonary_Disease_range <- svyby(~lbxwbcsi, ~Any_Pulmonary_Disease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_Any_Pulmonary_Disease_summary <- data.frame(WBC_Any_Pulmonary_Disease)
WBC_Any_Pulmonary_Disease_summarysd<-data.frame(WBC_Any_Pulmonary_Disease_sd)
WBC_Any_Pulmonary_Disease_summarysd$sd <- sqrt(WBC_Any_Pulmonary_Disease_summarysd$lbxwbcsi)

#Print
WBC_Any_Pulmonary_Disease_summary
WBC_Any_Pulmonary_Disease_summarysd
WBC_Any_Pulmonary_Disease_range



# Weighted Frequency table for VIGOROUS_ACTIVITY
table_weighted_VIGOROUS_ACTIVITY<-svytable(~VIGOROUS_ACTIVITY, nhanesDesign)
tabledataframe_VIGOROUS_ACTIVITY<-data.frame(table_weighted_VIGOROUS_ACTIVITY)
total_freq <- sum(tabledataframe_VIGOROUS_ACTIVITY$Freq)
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- (tabledataframe_VIGOROUS_ACTIVITY$Freq/total_freq)*100
tabledataframe_VIGOROUS_ACTIVITY$Percentage <- round(tabledataframe_VIGOROUS_ACTIVITY$Percentage, 2) 
tabledataframe_VIGOROUS_ACTIVITY
#VIGOROUS_ACTIVITY       Freq Percentage
#1            0 1191956126      9.15
#2            1  262092163      90.74

WBC_VIGOROUS_ACTIVITY <- svyby(~lbxwbcsi, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svymean)
WBC_VIGOROUS_ACTIVITY_sd <- svyby(~lbxwbcsi, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svyvar)
WBC_VIGOROUS_ACTIVITY_range <- svyby(~lbxwbcsi, ~VIGOROUS_ACTIVITY, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
WBC_VIGOROUS_ACTIVITY_summary <- data.frame(WBC_VIGOROUS_ACTIVITY)
WBC_VIGOROUS_ACTIVITY_summarysd<-data.frame(WBC_VIGOROUS_ACTIVITY_sd)
WBC_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(WBC_VIGOROUS_ACTIVITY_summarysd$lbxwbcsi)

#Print
WBC_VIGOROUS_ACTIVITY_summary
WBC_VIGOROUS_ACTIVITY_summarysd
WBC_VIGOROUS_ACTIVITY_range




