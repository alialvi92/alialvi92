install.packages("readr")
library(readr)
library(httr)
library(xml2)
library(XML)
library(dplyr)
library(purrr)
library(tidyverse)
library(jsonlite)
library(haven)
library(dplyr)
library(gh)
library(tidyverse)
library(stringr)
library(purrr)
library(dplyr)
if(!require("gh")) install.packages("gh")
library(gh)


# Preparing the Dataset to Calculate Phenoage for
# the Cycle 1999-2000

# Set the repository URL and API endpoint
url <- "https://api.github.com/repos/alialvi92/alialvi92/contents/NHANES%201999-2000"
endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s", "alialvi92/alialvi92", "NHANES%201999-2000")

# Make the API request
response <- GET(endpoint)

# Parse the response JSON
file_list <- fromJSON(content(response, as = "text"))

# Extract file names and URLs
file_names <- file_list$name
file_urls <- file_list$download_url

# Read in CSV files as a list of data frames
data_list <- lapply(file_urls, read.csv)
merged_df <- reduce(data_list, full_join, by = "seqn")

# Removing duplicate observations (generated from files that
# have "long" format data, where each respondent has multiple rows
# of data) merged_df_wo_duplicates <- merged_df %>% 
merged_df_wo_duplicates <- merged_df %>% 
  distinct(seqn, .keep_all = TRUE)

# Variables from NHANES (Other than laboratory values) 
# to be selected 
# sdmvpsu (id),sdmvstra (strata), wtint2yr (weights)-> These variables 
# will be used for weighting
# mcq010 mcq160g mcq160k (pulmonary), 
# mcq220 (cancer),
# lbdtcsi (dyslipidemia),
# mcq160b mcq160c mcq160f (Heart disease),
# kiq022 kiq020 (CKD), 
# indhhinc ind235 (family income), 
# dmdeduc (education),
# bmxbmi (bmi), 
# bpxdi1 (diastolic pressure) 
# bpxsy1 (systolic), 
# bpq020 (told had hypertension), 
# diq010 (told had diabetes), 
# alq150 (alcohol) , 
# smq040 smq020 (smoking variables), 
# pad200 pad320 paq650 paq665 (activity), 
# ridreth1 (race)

NHANES_19992000Variables<-merged_df_wo_duplicates[,c("seqn", "sdmvpsu",
                                                     "sdmvstra",
                                                     "wtint2yr",
                                                     "mcq010",
                                                     "mcq160g",
                                                     "mcq160k",
                                                     "mcq220",
                                                     "lbdtcsi",
                                                     "mcq160b",
                                                     "mcq160c",
                                                     "mcq160f",
                                                     "kiq020",
                                                     "indfminc",
                                                     
                                                     "dmdeduc2",
                                                     "bmxbmi",
                                                     "bpxdi1",
                                                     "bpxsy1",
                                                     "bpq020",
                                                     "diq010",
                                                     "alq110",
                                                     "alq150",
                                                     "smq040",
                                                     "smq020",
                                                     "pad200",
                                                     "pad320",
                                                     "ridreth1")]

NHANES_19992000Variables$alq100<-NHANES_19992000Variables$alq110

# Selecting only those columns required for calculating 
# pheno age
Biomarker_NHANES_19992000<-merged_df_wo_duplicates[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                                       "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                       "lbxsapsi","lbxwbcsi","ridageyr")]

# Removing observations with missing values for each 
# lab value one by one to only select those observations
# in the end with no missing lab values
merged_df1<-subset(Biomarker_NHANES_19992000,!is.na(Biomarker_NHANES_19992000$lbdsalsi))
merged_df2<-subset(merged_df1,!is.na(merged_df1$lbdscrsi))
merged_df3<-subset(merged_df2,!is.na(merged_df2$lbdsglsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbxcrp))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbxlypct))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxmcvsi))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxrdw))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxsapsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxwbcsi))
merged_df10<-subset(merged_df9,!is.na(merged_df9$ridageyr))

# Renaming the dataset to its respective year cycle
Final_Biomarker_NHANES_19992000<-merged_df10

# Calculating Phenoage

# First Calculating the Terms for Each Individual Lab Value

# Albumin
summary(Final_Biomarker_NHANES_19992000$lbdsalsi)
Final_Biomarker_NHANES_19992000$lbdsalsi_Wts<--0.0336
Final_Biomarker_NHANES_19992000$lbdsalsi_Terms<-Final_Biomarker_NHANES_19992000$lbdsalsi*Final_Biomarker_NHANES_19992000$lbdsalsi_Wts

# Creatinine
summary(Final_Biomarker_NHANES_19992000$lbdscrsi)
Final_Biomarker_NHANES_19992000$lbdscrsi_Wts<-0.0095
Final_Biomarker_NHANES_19992000$lbdscrsi_Terms<-Final_Biomarker_NHANES_19992000$lbdscrsi*Final_Biomarker_NHANES_19992000$lbdscrsi_Wts

# Glucose
summary(Final_Biomarker_NHANES_19992000$lbdsglsi)
Final_Biomarker_NHANES_19992000$lbdsglsi_Wts<-0.1953
Final_Biomarker_NHANES_19992000$lbdsglsi_Terms<-Final_Biomarker_NHANES_19992000$lbdsglsi*Final_Biomarker_NHANES_19992000$lbdsglsi_Wts

# CRP
summary(Final_Biomarker_NHANES_19992000$lbxcrp)
Final_Biomarker_NHANES_19992000$lbxcrp_Conversion<-Final_Biomarker_NHANES_19992000$lbxcrp*0.1
Final_Biomarker_NHANES_19992000$lbxcrp_LN_Conversion<-log(Final_Biomarker_NHANES_19992000$lbxcrp_Conversion)
Final_Biomarker_NHANES_19992000$lbxcrp_Wts<-0.0954
Final_Biomarker_NHANES_19992000$lbxcrp_Terms<-Final_Biomarker_NHANES_19992000$lbxcrp_LN_Conversion*Final_Biomarker_NHANES_19992000$lbxcrp_Wts

# Lymphocyte
summary(Final_Biomarker_NHANES_19992000$lbxlypct)
Final_Biomarker_NHANES_19992000$lbxlypct_Wts<--0.012
Final_Biomarker_NHANES_19992000$lbxlypct_Terms<-Final_Biomarker_NHANES_19992000$lbxlypct*Final_Biomarker_NHANES_19992000$lbxlypct_Wts

# Mean Cell Volume
summary(Final_Biomarker_NHANES_19992000$lbxmcvsi)
Final_Biomarker_NHANES_19992000$lbxmcvsi_Wts<-0.0268
Final_Biomarker_NHANES_19992000$lbxmcvsi_Terms<-Final_Biomarker_NHANES_19992000$lbxmcvsi*Final_Biomarker_NHANES_19992000$lbxmcvsi_Wts

# Red Cell Dist Width
summary(Final_Biomarker_NHANES_19992000$lbxrdw)
Final_Biomarker_NHANES_19992000$lbxrdw_Wts<-0.3306
Final_Biomarker_NHANES_19992000$lbxrdw_Terms<-Final_Biomarker_NHANES_19992000$lbxrdw*Final_Biomarker_NHANES_19992000$lbxrdw_Wts

# Alkaline Phosphatase
summary(Final_Biomarker_NHANES_19992000$lbxsapsi)
Final_Biomarker_NHANES_19992000$lbxsapsi_Wts<-0.0019
Final_Biomarker_NHANES_19992000$lbxsapsi_Terms<-Final_Biomarker_NHANES_19992000$lbxrdw*Final_Biomarker_NHANES_19992000$lbxsapsi_Wts

# White Blood Cells
summary(Final_Biomarker_NHANES_19992000$lbxwbcsi)
Final_Biomarker_NHANES_19992000$lbxwbcsi_Wts<-0.0554
Final_Biomarker_NHANES_19992000$lbxwbcsi_Terms<-Final_Biomarker_NHANES_19992000$lbxwbcsi*Final_Biomarker_NHANES_19992000$lbxwbcsi_Wts

# Age
summary(Final_Biomarker_NHANES_19992000$ridageyr)
Final_Biomarker_NHANES_19992000$ridageyr_Wts<-0.0804
Final_Biomarker_NHANES_19992000$ridageyr_Terms<-Final_Biomarker_NHANES_19992000$ridageyr*Final_Biomarker_NHANES_19992000$ridageyr_Wts

# Making a variable for "B0"
Final_Biomarker_NHANES_19992000$B0<--19.9067

# Making a variable which is the sum of all variables above.
Final_Biomarker_NHANES_19992000$LinComb<-rowSums(Final_Biomarker_NHANES_19992000[, c("B0", "lbdsalsi_Terms", "lbdscrsi_Terms", "lbdsglsi_Terms",
                                                                                     "lbxcrp_Terms", "lbxlypct_Terms", "lbxmcvsi_Terms","lbxrdw_Terms",
                                                                             "lbxsapsi_Terms","lbxwbcsi_Terms","ridageyr_Terms")])

#Making a variable for "Gamma"
Final_Biomarker_NHANES_19992000$Gamma<-0.0076927

# Making a variable for "t"
Final_Biomarker_NHANES_19992000$t<-120

# Making a variable for "MortScore" in multiple steps
Final_Biomarker_NHANES_19992000$MortScore1 <-exp(Final_Biomarker_NHANES_19992000$Gamma*120)-1
Final_Biomarker_NHANES_19992000$MortScore2 <--exp(Final_Biomarker_NHANES_19992000$LinComb)
Final_Biomarker_NHANES_19992000$MortScore3 <-Final_Biomarker_NHANES_19992000$MortScore2/Final_Biomarker_NHANES_19992000$Gamma
Final_Biomarker_NHANES_19992000$MortScore4 <-exp(Final_Biomarker_NHANES_19992000$MortScore3)
Final_Biomarker_NHANES_19992000$MortScore5 <-1-Final_Biomarker_NHANES_19992000$MortScore4

# Finally, making a variable for PhenoAge.
Final_Biomarker_NHANES_19992000$PhenoAge<-141.50225+log(-0.00553*log(1-Final_Biomarker_NHANES_19992000$MortScore5))/0.09165
summary(Final_Biomarker_NHANES_19992000$PhenoAge)


# Preparing the Dataset to Calculate Phenoage for
# the Cycle 2001-2002

# set the URL of your GitHub repository
url <- "https://github.com/alialvi92/alialvi92"

library(jsonlite)

# Set the repository URL and API endpoint
url <- "https://api.github.com/repos/alialvi92/alialvi92/contents/NHANES%202001-2002"
endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s", "alialvi92/alialvi92", "NHANES%202001-2002")

# Make the API request
response <- GET(endpoint)

# Parse the response JSON
file_list <- fromJSON(content(response, as = "text"))

# Extract file names and URLs
file_names <- file_list$name
file_urls <- file_list$download_url

# Read in CSV files as a list of data frames
data_list <- lapply(file_urls, read.csv)
merged_df <- reduce(data_list, full_join, by = "seqn")

# Removing duplicate observations (generated from files that
# have "long" format data, where each respondent has multiple rows
# of data)
merged_df_wo_duplicates <- merged_df %>% 
  distinct(seqn, .keep_all = TRUE)

# Variables from NHANES (Other than laboratory values) 
# to be selected 
# sdmvpsu (id),sdmvstra (strata), wtint2yr (weights)-> These variables 
# will be used for weighting
# mcq010 mcq160g mcq160k (pulmonary), 
# mcq220 (cancer),
# lbdtcsi (dyslipidemia),
# mcq160b mcq160c mcq160f (Heart disease),
# kiq022 kiq020 (CKD), 
# indhhinc ind235 (family income), 
# dmdeduc (education),
# bmxbmi (bmi), 
# bpxdi1 (diastolic pressure) 
# bpxsy1 (systolic), 
# bpq020 (told had hypertension), 
# diq010 (told had diabetes), 
# alq150 (alcohol) , 
# smq040 smq020 (smoking variables), 
# pad200 pad320 paq650 paq665 (activity), 
# ridreth1 (race)

NHANES_20012002Variables<-merged_df_wo_duplicates[,c("seqn", "sdmvpsu",
                                                     "sdmvstra",
                                                     "wtint2yr",
                                                     "mcq010",
                                                     "mcq160g",
                                                     "mcq160k",
                                                     "mcq220",
                                                     "lbdtcsi",
                                                     "mcq160b",
                                                     "mcq160c",
                                                     "mcq160f",
                                                     "kiq022",
                                                     "indfminc",
                                                     
                                                     "dmdeduc2",
                                                     "bmxbmi",
                                                     "bpxdi1",
                                                     "bpxsy1",
                                                     "bpq020",
                                                     "diq010",
                                                     "alq110",
                                                     "alq150",
                                                     "smq040",
                                                     "smq020",
                                                     "pad200",
                                                     "pad320",
                                                     "ridreth1")]
NHANES_20012002Variables$alq100<-NHANES_20012002Variables$alq110
# Selecting only those columns required for calculating 
# pheno age
Biomarker_NHANES_20012002<-merged_df_wo_duplicates[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                                       "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                       "lbdsapsi","lbxwbcsi","ridageyr")]
# Removing observations with missing values for each 
# lab value one by one to only select those observations
# in the end with no missing lab values
merged_df1<-subset(Biomarker_NHANES_20012002,!is.na(Biomarker_NHANES_20012002$lbdsalsi))
merged_df2<-subset(merged_df1,!is.na(merged_df1$lbdscrsi))
merged_df3<-subset(merged_df2,!is.na(merged_df2$lbdsglsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbxcrp))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbxlypct))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxmcvsi))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxrdw))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbdsapsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxwbcsi))
merged_df10<-subset(merged_df9,!is.na(merged_df9$ridageyr))

# Renaming the dataset to its respective year cycle
Final_Biomarker_NHANES_20012002<-merged_df10

# Calculating Phenoage

# First Calculating the Terms for Each Individual Lab Value

# Albumin
summary(Final_Biomarker_NHANES_20012002$lbdsalsi)
Final_Biomarker_NHANES_20012002$lbdsalsi_Wts<--0.0336
Final_Biomarker_NHANES_20012002$lbdsalsi_Terms<-Final_Biomarker_NHANES_20012002$lbdsalsi*Final_Biomarker_NHANES_20012002$lbdsalsi_Wts

# Creatinine
summary(Final_Biomarker_NHANES_20012002$lbdscrsi)
Final_Biomarker_NHANES_20012002$lbdscrsi_Wts<-0.0095
Final_Biomarker_NHANES_20012002$lbdscrsi_Terms<-Final_Biomarker_NHANES_20012002$lbdscrsi*Final_Biomarker_NHANES_20012002$lbdscrsi_Wts

# Glucose
summary(Final_Biomarker_NHANES_20012002$lbdsglsi)
Final_Biomarker_NHANES_20012002$lbdsglsi_Wts<-0.1953
Final_Biomarker_NHANES_20012002$lbdsglsi_Terms<-Final_Biomarker_NHANES_20012002$lbdsglsi*Final_Biomarker_NHANES_20012002$lbdsglsi_Wts

# CRP
summary(Final_Biomarker_NHANES_20012002$lbxcrp)
Final_Biomarker_NHANES_20012002$lbxcrp_Conversion<-Final_Biomarker_NHANES_20012002$lbxcrp*0.1
Final_Biomarker_NHANES_20012002$lbxcrp_LN_Conversion<-log(Final_Biomarker_NHANES_20012002$lbxcrp_Conversion)
Final_Biomarker_NHANES_20012002$lbxcrp_Wts<-0.0954
Final_Biomarker_NHANES_20012002$lbxcrp_Terms<-Final_Biomarker_NHANES_20012002$lbxcrp_LN_Conversion*Final_Biomarker_NHANES_20012002$lbxcrp_Wts

# Lymphocyte
summary(Final_Biomarker_NHANES_20012002$lbxlypct)
Final_Biomarker_NHANES_20012002$lbxlypct_Wts<--0.012
Final_Biomarker_NHANES_20012002$lbxlypct_Terms<-Final_Biomarker_NHANES_20012002$lbxlypct*Final_Biomarker_NHANES_20012002$lbxlypct_Wts

# Mean Cell Volume
summary(Final_Biomarker_NHANES_20012002$lbxmcvsi)
Final_Biomarker_NHANES_20012002$lbxmcvsi_Wts<-0.0268
Final_Biomarker_NHANES_20012002$lbxmcvsi_Terms<-Final_Biomarker_NHANES_20012002$lbxmcvsi*Final_Biomarker_NHANES_20012002$lbxmcvsi_Wts

# Red Cell Dist Width
summary(Final_Biomarker_NHANES_20012002$lbxrdw)
Final_Biomarker_NHANES_20012002$lbxrdw_Wts<-0.3306
Final_Biomarker_NHANES_20012002$lbxrdw_Terms<-Final_Biomarker_NHANES_20012002$lbxrdw*Final_Biomarker_NHANES_20012002$lbxrdw_Wts

# Alkaline Phosphatase
summary(Final_Biomarker_NHANES_20012002$lbxsapsi)
Final_Biomarker_NHANES_20012002$lbxsapsi_Wts<-0.0019
Final_Biomarker_NHANES_20012002$lbxsapsi_Terms<-Final_Biomarker_NHANES_20012002$lbxrdw*Final_Biomarker_NHANES_20012002$lbxsapsi_Wts

# White Blood Cells
summary(Final_Biomarker_NHANES_20012002$lbxwbcsi)
Final_Biomarker_NHANES_20012002$lbxwbcsi_Wts<-0.0554
Final_Biomarker_NHANES_20012002$lbxwbcsi_Terms<-Final_Biomarker_NHANES_20012002$lbxwbcsi*Final_Biomarker_NHANES_20012002$lbxwbcsi_Wts

# Age
summary(Final_Biomarker_NHANES_20012002$ridageyr)
Final_Biomarker_NHANES_20012002$ridageyr_Wts<-0.0804
Final_Biomarker_NHANES_20012002$ridageyr_Terms<-Final_Biomarker_NHANES_20012002$ridageyr*Final_Biomarker_NHANES_20012002$ridageyr_Wts

# Making a variable for "B0"
Final_Biomarker_NHANES_20012002$B0<--19.9067

# Making a variable which is the sum of all variables above.
Final_Biomarker_NHANES_20012002$LinComb<-rowSums(Final_Biomarker_NHANES_20012002[, c("B0", "lbdsalsi_Terms", "lbdscrsi_Terms", "lbdsglsi_Terms",
                                                                                     "lbxcrp_Terms", "lbxlypct_Terms", "lbxmcvsi_Terms","lbxrdw_Terms",
                                                                                     "lbxsapsi_Terms","lbxwbcsi_Terms","ridageyr_Terms")])

#Making a variable for "Gamma"
Final_Biomarker_NHANES_20012002$Gamma<-0.0076927

# Making a variable for "t"
Final_Biomarker_NHANES_20012002$t<-120

# Making a variable for "MortScore" in multiple steps
Final_Biomarker_NHANES_20012002$MortScore1 <-exp(Final_Biomarker_NHANES_20012002$Gamma*120)-1
Final_Biomarker_NHANES_20012002$MortScore2 <--exp(Final_Biomarker_NHANES_20012002$LinComb)
Final_Biomarker_NHANES_20012002$MortScore3 <-Final_Biomarker_NHANES_20012002$MortScore2/Final_Biomarker_NHANES_20012002$Gamma
Final_Biomarker_NHANES_20012002$MortScore4 <-exp(Final_Biomarker_NHANES_20012002$MortScore3)
Final_Biomarker_NHANES_20012002$MortScore5 <-1-Final_Biomarker_NHANES_20012002$MortScore4

# Finally, making a variable for PhenoAge.
Final_Biomarker_NHANES_20012002$PhenoAge<-141.50225+log(-0.00553*log(1-Final_Biomarker_NHANES_20012002$MortScore5))/0.09165
summary(Final_Biomarker_NHANES_20012002$PhenoAge)




# Preparing the Dataset to Calculate Phenoage for
# the Cycle 2003-2004

# Set the repository URL and API endpoint
url <- "https://api.github.com/repos/alialvi92/alialvi92/contents/NHANES%202003-2004"
endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s", "alialvi92/alialvi92", "NHANES%202003-2004")

# Make the API request
response <- GET(endpoint)

# Parse the response JSON
file_list <- fromJSON(content(response, as = "text"))

# Extract file names and URLs
file_names <- file_list$name
file_urls <- file_list$download_url

# Read in CSV files as a list of data frames
data_list <- lapply(file_urls, read.csv)
merged_df <- reduce(data_list, full_join, by = "seqn")

# Removing duplicate observations (generated from files that
# have "long" format data, where each respondent has multiple rows
# of data) merged_df_wo_duplicates <- merged_df %>% 
merged_df_wo_duplicates <- merged_df %>% distinct(seqn, .keep_all = TRUE)

# Variables from NHANES (Other than laboratory values) 
# to be selected 
# sdmvpsu (id),sdmvstra (strata), wtint2yr (weights)-> These variables 
# will be used for weighting
# mcq010 mcq160g mcq160k (pulmonary), 
# mcq220 (cancer),
# lbdtcsi (dyslipidemia),
# mcq160b mcq160c mcq160f (Heart disease),
# kiq022 kiq020 (CKD), 
# indhhinc ind235 (family income), 
# dmdeduc (education),
# bmxbmi (bmi), 
# bpxdi1 (diastolic pressure) 
# bpxsy1 (systolic), 
# bpq020 (told had hypertension), 
# diq010 (told had diabetes), 
# alq150 (alcohol) , 
# smq040 smq020 (smoking variables), 
# pad200 pad320 paq650 paq665 (activity), 
# ridreth1 (race)

NHANES_20032004Variables<-merged_df_wo_duplicates[,c("seqn", "sdmvpsu",
                                                     "sdmvstra",
                                                     "wtint2yr",
                                                     "mcq010",
                                                     "mcq160g",
                                                     "mcq160k",
                                                     "mcq220",
                                                     "lbdtcsi",
                                                     "mcq160b",
                                                     "mcq160c",
                                                     "mcq160f",
                                                     "kiq022",
                                                     "indfminc",
                                                     
                                                     "dmdeduc2",
                                                     "bmxbmi",
                                                     "bpxdi1",
                                                     "bpxsy1",
                                                     "bpq020",
                                                     "diq010",
                                                     "alq101",
                                                     "alq150",
                                                     "smq040",
                                                     "smq020",
                                                     "pad200",
                                                     "pad320",
                                                     "ridreth1")]


# Selecting only those columns required for calculating 
# pheno age
Biomarker_NHANES_20032004<-merged_df_wo_duplicates[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                                       "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                       "lbxsapsi","lbxwbcsi","ridageyr")]

# Removing observations with missing values for each 
# lab value one by one to only select those observations
# in the end with no missing lab values
merged_df1<-subset(Biomarker_NHANES_20032004,!is.na(Biomarker_NHANES_20032004$lbdsalsi))
merged_df2<-subset(merged_df1,!is.na(merged_df1$lbdscrsi))
merged_df3<-subset(merged_df2,!is.na(merged_df2$lbdsglsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbxcrp))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbxlypct))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxmcvsi))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxrdw))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxsapsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxwbcsi))
merged_df10<-subset(merged_df9,!is.na(merged_df9$ridageyr))

# Renaming the dataset to its respective year cycle
Final_Biomarker_NHANES_20032004<-merged_df10

# Calculating Phenoage

# First Calculating the Terms for Each Individual Lab Value

# Albumin
summary(Final_Biomarker_NHANES_20032004$lbdsalsi)
Final_Biomarker_NHANES_20032004$lbdsalsi_Wts<--0.0336
Final_Biomarker_NHANES_20032004$lbdsalsi_Terms<-Final_Biomarker_NHANES_20032004$lbdsalsi*Final_Biomarker_NHANES_20032004$lbdsalsi_Wts

# Creatinine
summary(Final_Biomarker_NHANES_20032004$lbdscrsi)
Final_Biomarker_NHANES_20032004$lbdscrsi_Wts<-0.0095
Final_Biomarker_NHANES_20032004$lbdscrsi_Terms<-Final_Biomarker_NHANES_20032004$lbdscrsi*Final_Biomarker_NHANES_20032004$lbdscrsi_Wts

# Glucose
summary(Final_Biomarker_NHANES_20032004$lbdsglsi)
Final_Biomarker_NHANES_20032004$lbdsglsi_Wts<-0.1953
Final_Biomarker_NHANES_20032004$lbdsglsi_Terms<-Final_Biomarker_NHANES_20032004$lbdsglsi*Final_Biomarker_NHANES_20032004$lbdsglsi_Wts

# CRP
summary(Final_Biomarker_NHANES_20032004$lbxcrp)
Final_Biomarker_NHANES_20032004$lbxcrp_Conversion<-Final_Biomarker_NHANES_20032004$lbxcrp*0.1
Final_Biomarker_NHANES_20032004$lbxcrp_LN_Conversion<-log(Final_Biomarker_NHANES_20032004$lbxcrp_Conversion)
Final_Biomarker_NHANES_20032004$lbxcrp_Wts<-0.0954
Final_Biomarker_NHANES_20032004$lbxcrp_Terms<-Final_Biomarker_NHANES_20032004$lbxcrp_LN_Conversion*Final_Biomarker_NHANES_20032004$lbxcrp_Wts

# Lymphocyte
summary(Final_Biomarker_NHANES_20032004$lbxlypct)
Final_Biomarker_NHANES_20032004$lbxlypct_Wts<--0.012
Final_Biomarker_NHANES_20032004$lbxlypct_Terms<-Final_Biomarker_NHANES_20032004$lbxlypct*Final_Biomarker_NHANES_20032004$lbxlypct_Wts

# Mean Cell Volume
summary(Final_Biomarker_NHANES_20032004$lbxmcvsi)
Final_Biomarker_NHANES_20032004$lbxmcvsi_Wts<-0.0268
Final_Biomarker_NHANES_20032004$lbxmcvsi_Terms<-Final_Biomarker_NHANES_20032004$lbxmcvsi*Final_Biomarker_NHANES_20032004$lbxmcvsi_Wts

# Red Cell Dist Width
summary(Final_Biomarker_NHANES_20032004$lbxrdw)
Final_Biomarker_NHANES_20032004$lbxrdw_Wts<-0.3306
Final_Biomarker_NHANES_20032004$lbxrdw_Terms<-Final_Biomarker_NHANES_20032004$lbxrdw*Final_Biomarker_NHANES_20032004$lbxrdw_Wts

# Alkaline Phosphatase
summary(Final_Biomarker_NHANES_20032004$lbxsapsi)
Final_Biomarker_NHANES_20032004$lbxsapsi_Wts<-0.0019
Final_Biomarker_NHANES_20032004$lbxsapsi_Terms<-Final_Biomarker_NHANES_20032004$lbxrdw*Final_Biomarker_NHANES_20032004$lbxsapsi_Wts

# White Blood Cells
summary(Final_Biomarker_NHANES_20032004$lbxwbcsi)
Final_Biomarker_NHANES_20032004$lbxwbcsi_Wts<-0.0554
Final_Biomarker_NHANES_20032004$lbxwbcsi_Terms<-Final_Biomarker_NHANES_20032004$lbxwbcsi*Final_Biomarker_NHANES_20032004$lbxwbcsi_Wts

# Age
summary(Final_Biomarker_NHANES_20032004$ridageyr)
Final_Biomarker_NHANES_20032004$ridageyr_Wts<-0.0804
Final_Biomarker_NHANES_20032004$ridageyr_Terms<-Final_Biomarker_NHANES_20032004$ridageyr*Final_Biomarker_NHANES_20032004$ridageyr_Wts

# Making a variable for "B0"
Final_Biomarker_NHANES_20032004$B0<--19.9067

# Making a variable which is the sum of all variables above.
Final_Biomarker_NHANES_20032004$LinComb<-rowSums(Final_Biomarker_NHANES_20032004[, c("B0", "lbdsalsi_Terms", "lbdscrsi_Terms", "lbdsglsi_Terms",
                                                                                     "lbxcrp_Terms", "lbxlypct_Terms", "lbxmcvsi_Terms","lbxrdw_Terms",
                                                                                     "lbxsapsi_Terms","lbxwbcsi_Terms","ridageyr_Terms")])

#Making a variable for "Gamma"
Final_Biomarker_NHANES_20032004$Gamma<-0.0076927

# Making a variable for "t"
Final_Biomarker_NHANES_20032004$t<-120

# Making a variable for "MortScore" in multiple steps
Final_Biomarker_NHANES_20032004$MortScore1 <-exp(Final_Biomarker_NHANES_20032004$Gamma*120)-1
Final_Biomarker_NHANES_20032004$MortScore2 <--exp(Final_Biomarker_NHANES_20032004$LinComb)
Final_Biomarker_NHANES_20032004$MortScore3 <-Final_Biomarker_NHANES_20032004$MortScore2/Final_Biomarker_NHANES_20032004$Gamma
Final_Biomarker_NHANES_20032004$MortScore4 <-exp(Final_Biomarker_NHANES_20032004$MortScore3)
Final_Biomarker_NHANES_20032004$MortScore5 <-1-Final_Biomarker_NHANES_20032004$MortScore4

# Finally, making a variable for PhenoAge.
Final_Biomarker_NHANES_20032004$PhenoAge<-141.50225+log(-0.00553*log(1-Final_Biomarker_NHANES_20032004$MortScore5))/0.09165
summary(Final_Biomarker_NHANES_20032004$PhenoAge)


# Preparing the Dataset to Calculate Phenoage for
# the Cycle 2005-2006

# Set the repository URL and API endpoint
url <- "https://api.github.com/repos/alialvi92/alialvi92/contents/NHANES%202005-2006"
endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s", "alialvi92/alialvi92", "NHANES%202005-2006")

# Make the API request
response <- GET(endpoint)

# Parse the response JSON
file_list <- fromJSON(content(response, as = "text"))

# Extract file names and URLs
file_names <- file_list$name
file_urls <- file_list$download_url

# Read in CSV files as a list of data frames
data_list <- lapply(file_urls, read.csv)
merged_df <- reduce(data_list, full_join, by = "seqn")

# Removing duplicate observations (generated from files that
# have "long" format data, where each respondent has multiple rows
# of data) merged_df_wo_duplicates <- merged_df %>% 
merged_df_wo_duplicates <- merged_df %>% distinct(seqn, .keep_all = TRUE)

# Selecting only those columns required for calculating 
# pheno age
Biomarker_NHANES_20052006<-merged_df_wo_duplicates[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                                       "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                       "lbxsapsi","lbxwbcsi","ridageyr")]
# Variables from NHANES (Other than laboratory values) 
# to be selected 
# sdmvpsu (id),sdmvstra (strata), wtint2yr (weights)-> These variables 
# will be used for weighting
# mcq010 mcq160g mcq160k (pulmonary), 
# mcq220 (cancer),
# lbdtcsi (dyslipidemia),
# mcq160b mcq160c mcq160f (Heart disease),
# kiq022 kiq020 (CKD), 
# indhhinc ind235 (family income), 
# dmdeduc (education),
# bmxbmi (bmi), 
# bpxdi1 (diastolic pressure) 
# bpxsy1 (systolic), 
# bpq020 (told had hypertension), 
# diq010 (told had diabetes), 
# alq150 (alcohol) , 
# smq040 smq020 (smoking variables), 
# pad200 pad320 paq650 paq665 (activity), 
# ridreth1 (race)

NHANES_20052006Variables<-merged_df_wo_duplicates[,c("seqn", "sdmvpsu",
                                                     "sdmvstra",
                                                     "wtint2yr",
                                                     "mcq010",
                                                     "mcq160g",
                                                     "mcq160k",
                                                     "mcq220",
                                                     "lbdtcsi",
                                                     "mcq160b",
                                                     "mcq160c",
                                                     "mcq160f",
                                                     "kiq022",
                                                     "indfminc",
                                                     
                                                     "dmdeduc2",
                                                     "bmxbmi",
                                                     "bpxdi1",
                                                     "bpxsy1",
                                                     "bpq020",
                                                     "diq010",
                                                     "alq101",
                                                     "alq150",
                                                     "smq040",
                                                     "smq020",
                                                     "pad200",
                                                     "pad320",
                                                     "ridreth1")]

# Removing observations with missing values for each 
# lab value one by one to only select those observations
# in the end with no missing lab values
merged_df1<-subset(Biomarker_NHANES_20052006,!is.na(Biomarker_NHANES_20052006$lbdsalsi))
merged_df2<-subset(merged_df1,!is.na(merged_df1$lbdscrsi))
merged_df3<-subset(merged_df2,!is.na(merged_df2$lbdsglsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbxcrp))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbxlypct))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxmcvsi))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxrdw))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxsapsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxwbcsi))
merged_df10<-subset(merged_df9,!is.na(merged_df9$ridageyr))

# Renaming the dataset to its respective year cycle
Final_Biomarker_NHANES_20052006<-merged_df10

# Calculating Phenoage

# First Calculating the Terms for Each Individual Lab Value

# Albumin
summary(Final_Biomarker_NHANES_20052006$lbdsalsi)
Final_Biomarker_NHANES_20052006$lbdsalsi_Wts<--0.0336
Final_Biomarker_NHANES_20052006$lbdsalsi_Terms<-Final_Biomarker_NHANES_20052006$lbdsalsi*Final_Biomarker_NHANES_20052006$lbdsalsi_Wts

# Creatinine
summary(Final_Biomarker_NHANES_20052006$lbdscrsi)
Final_Biomarker_NHANES_20052006$lbdscrsi_Wts<-0.0095
Final_Biomarker_NHANES_20052006$lbdscrsi_Terms<-Final_Biomarker_NHANES_20052006$lbdscrsi*Final_Biomarker_NHANES_20052006$lbdscrsi_Wts

# Glucose
summary(Final_Biomarker_NHANES_20052006$lbdsglsi)
Final_Biomarker_NHANES_20052006$lbdsglsi_Wts<-0.1953
Final_Biomarker_NHANES_20052006$lbdsglsi_Terms<-Final_Biomarker_NHANES_20052006$lbdsglsi*Final_Biomarker_NHANES_20052006$lbdsglsi_Wts

# CRP
summary(Final_Biomarker_NHANES_20052006$lbxcrp)
Final_Biomarker_NHANES_20052006$lbxcrp_Conversion<-Final_Biomarker_NHANES_20052006$lbxcrp*0.1
Final_Biomarker_NHANES_20052006$lbxcrp_LN_Conversion<-log(Final_Biomarker_NHANES_20052006$lbxcrp_Conversion)
Final_Biomarker_NHANES_20052006$lbxcrp_Wts<-0.0954
Final_Biomarker_NHANES_20052006$lbxcrp_Terms<-Final_Biomarker_NHANES_20052006$lbxcrp_LN_Conversion*Final_Biomarker_NHANES_20052006$lbxcrp_Wts

# Lymphocyte
summary(Final_Biomarker_NHANES_20052006$lbxlypct)
Final_Biomarker_NHANES_20052006$lbxlypct_Wts<--0.012
Final_Biomarker_NHANES_20052006$lbxlypct_Terms<-Final_Biomarker_NHANES_20052006$lbxlypct*Final_Biomarker_NHANES_20052006$lbxlypct_Wts

# Mean Cell Volume
summary(Final_Biomarker_NHANES_20052006$lbxmcvsi)
Final_Biomarker_NHANES_20052006$lbxmcvsi_Wts<-0.0268
Final_Biomarker_NHANES_20052006$lbxmcvsi_Terms<-Final_Biomarker_NHANES_20052006$lbxmcvsi*Final_Biomarker_NHANES_20052006$lbxmcvsi_Wts

# Red Cell Dist Width
summary(Final_Biomarker_NHANES_20052006$lbxrdw)
Final_Biomarker_NHANES_20052006$lbxrdw_Wts<-0.3306
Final_Biomarker_NHANES_20052006$lbxrdw_Terms<-Final_Biomarker_NHANES_20052006$lbxrdw*Final_Biomarker_NHANES_20052006$lbxrdw_Wts

# Alkaline Phosphatase
summary(Final_Biomarker_NHANES_20052006$lbxsapsi)
Final_Biomarker_NHANES_20052006$lbxsapsi_Wts<-0.0019
Final_Biomarker_NHANES_20052006$lbxsapsi_Terms<-Final_Biomarker_NHANES_20052006$lbxrdw*Final_Biomarker_NHANES_20052006$lbxsapsi_Wts

# White Blood Cells
summary(Final_Biomarker_NHANES_20052006$lbxwbcsi)
Final_Biomarker_NHANES_20052006$lbxwbcsi_Wts<-0.0554
Final_Biomarker_NHANES_20052006$lbxwbcsi_Terms<-Final_Biomarker_NHANES_20052006$lbxwbcsi*Final_Biomarker_NHANES_20052006$lbxwbcsi_Wts

# Age
summary(Final_Biomarker_NHANES_20052006$ridageyr)
Final_Biomarker_NHANES_20052006$ridageyr_Wts<-0.0804
Final_Biomarker_NHANES_20052006$ridageyr_Terms<-Final_Biomarker_NHANES_20052006$ridageyr*Final_Biomarker_NHANES_20052006$ridageyr_Wts

# Making a variable for "B0"
Final_Biomarker_NHANES_20052006$B0<--19.9067

# Making a variable which is the sum of all variables above.
Final_Biomarker_NHANES_20052006$LinComb<-rowSums(Final_Biomarker_NHANES_20052006[, c("B0", "lbdsalsi_Terms", "lbdscrsi_Terms", "lbdsglsi_Terms",
                                                                                     "lbxcrp_Terms", "lbxlypct_Terms", "lbxmcvsi_Terms","lbxrdw_Terms",
                                                                                     "lbxsapsi_Terms","lbxwbcsi_Terms","ridageyr_Terms")])

#Making a variable for "Gamma"
Final_Biomarker_NHANES_20052006$Gamma<-0.0076927

# Making a variable for "t"
Final_Biomarker_NHANES_20052006$t<-120

# Making a variable for "MortScore" in multiple steps
Final_Biomarker_NHANES_20052006$MortScore1 <-exp(Final_Biomarker_NHANES_20052006$Gamma*120)-1
Final_Biomarker_NHANES_20052006$MortScore2 <--exp(Final_Biomarker_NHANES_20052006$LinComb)
Final_Biomarker_NHANES_20052006$MortScore3 <-Final_Biomarker_NHANES_20052006$MortScore2/Final_Biomarker_NHANES_20052006$Gamma
Final_Biomarker_NHANES_20052006$MortScore4 <-exp(Final_Biomarker_NHANES_20052006$MortScore3)
Final_Biomarker_NHANES_20052006$MortScore5 <-1-Final_Biomarker_NHANES_20052006$MortScore4

# Finally, making a variable for PhenoAge.
Final_Biomarker_NHANES_20052006$PhenoAge<-141.50225+log(-0.00553*log(1-Final_Biomarker_NHANES_20052006$MortScore5))/0.09165
summary(Final_Biomarker_NHANES_20052006$PhenoAge)






# Preparing the Dataset to Calculate Phenoage for
# the Cycle 2007-2008

# Set the repository URL and API endpoint
url <- "https://api.github.com/repos/alialvi92/alialvi92/contents/NHANES%202007-2008"
endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s", "alialvi92/alialvi92", "NHANES%202007-2008")

# Make the API request
response <- GET(endpoint)

# Parse the response JSON
file_list <- fromJSON(content(response, as = "text"))

# Extract file names and URLs
file_names <- file_list$name
file_urls <- file_list$download_url

# Read in CSV files as a list of data frames
data_list <- lapply(file_urls, read.csv)
merged_df <- reduce(data_list, full_join, by = "seqn")

# Removing duplicate observations (generated from files that
# have "long" format data, where each respondent has multiple rows
# of data) merged_df_wo_duplicates <- merged_df %>% 
merged_df_wo_duplicates <- merged_df %>% distinct(seqn, .keep_all = TRUE)

Biomarker_NHANES_20072008<-merged_df_wo_duplicates[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                                       "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                       "lbxsapsi","lbxwbcsi","ridageyr")]
# Variables from NHANES (Other than laboratory values) 
# to be selected 
# sdmvpsu (id),sdmvstra (strata), wtint2yr (weights)-> These variables 
# will be used for weighting
# mcq010 mcq160g mcq160k (pulmonary), 
# mcq220 (cancer),
# lbdtcsi (dyslipidemia),
# mcq160b mcq160c mcq160f (Heart disease),
# kiq022 kiq020 (CKD), 
# indhhinc ind235 (family income), 
# dmdeduc (education),
# bmxbmi (bmi), 
# bpxdi1 (diastolic pressure) 
# bpxsy1 (systolic), 
# bpq020 (told had hypertension), 
# diq010 (told had diabetes), 
# alq150 (alcohol) , 
# smq040 smq020 (smoking variables), 
# pad200 pad320 paq650 paq665 (activity), 
# ridreth1 (race)

NHANES_20072008Variables<-merged_df_wo_duplicates[,c("seqn", "sdmvpsu",
                                                     "sdmvstra",
                                                     "wtint2yr",
                                                     "mcq010",
                                                     "mcq160g",
                                                     "mcq160k",
                                                     "mcq220",
                                                     "lbdtcsi",
                                                     "mcq160b",
                                                     "mcq160c",
                                                     "mcq160f",
                                                     "kiq022",
                                                     "indfmin2",
                                                     
                                                     "dmdeduc2",
                                                     "bmxbmi",
                                                     "bpxdi1",
                                                     "bpxsy1",
                                                     "bpq020",
                                                     "diq010",
                                                     "alq101",
                                                     "alq150",
                                                     "smq040",
                                                     "smq020",
                                                     "paq650",
                                                     "paq665",
                                                     "ridreth1")]

# Selecting only those columns required for calculating 
# pheno age
Biomarker_NHANES_20072008<-merged_df_wo_duplicates[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                                       "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                       "lbxsapsi","lbxwbcsi","ridageyr")]

# Removing observations with missing values for each 
# lab value one by one to only select those observations
# in the end with no missing lab values
merged_df1<-subset(Biomarker_NHANES_20072008,!is.na(Biomarker_NHANES_20072008$lbdsalsi))
merged_df2<-subset(merged_df1,!is.na(merged_df1$lbdscrsi))
merged_df3<-subset(merged_df2,!is.na(merged_df2$lbdsglsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbxcrp))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbxlypct))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxmcvsi))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxrdw))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxsapsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxwbcsi))
merged_df10<-subset(merged_df9,!is.na(merged_df9$ridageyr))

# Renaming the dataset to its respective year cycle
Final_Biomarker_NHANES_20072008<-merged_df10
# Calculating Phenoage

# First Calculating the Terms for Each Individual Lab Value

# Albumin
summary(Final_Biomarker_NHANES_20072008$lbdsalsi)
Final_Biomarker_NHANES_20072008$lbdsalsi_Wts<--0.0336
Final_Biomarker_NHANES_20072008$lbdsalsi_Terms<-Final_Biomarker_NHANES_20072008$lbdsalsi*Final_Biomarker_NHANES_20072008$lbdsalsi_Wts

# Creatinine
summary(Final_Biomarker_NHANES_20072008$lbdscrsi)
Final_Biomarker_NHANES_20072008$lbdscrsi_Wts<-0.0095
Final_Biomarker_NHANES_20072008$lbdscrsi_Terms<-Final_Biomarker_NHANES_20072008$lbdscrsi*Final_Biomarker_NHANES_20072008$lbdscrsi_Wts

# Glucose
summary(Final_Biomarker_NHANES_20072008$lbdsglsi)
Final_Biomarker_NHANES_20072008$lbdsglsi_Wts<-0.1953
Final_Biomarker_NHANES_20072008$lbdsglsi_Terms<-Final_Biomarker_NHANES_20072008$lbdsglsi*Final_Biomarker_NHANES_20072008$lbdsglsi_Wts

# CRP
summary(Final_Biomarker_NHANES_20072008$lbxcrp)
Final_Biomarker_NHANES_20072008$lbxcrp_Conversion<-Final_Biomarker_NHANES_20072008$lbxcrp*0.1
Final_Biomarker_NHANES_20072008$lbxcrp_LN_Conversion<-log(Final_Biomarker_NHANES_20072008$lbxcrp_Conversion)
Final_Biomarker_NHANES_20072008$lbxcrp_Wts<-0.0954
Final_Biomarker_NHANES_20072008$lbxcrp_Terms<-Final_Biomarker_NHANES_20072008$lbxcrp_LN_Conversion*Final_Biomarker_NHANES_20072008$lbxcrp_Wts

# Lymphocyte
summary(Final_Biomarker_NHANES_20072008$lbxlypct)
Final_Biomarker_NHANES_20072008$lbxlypct_Wts<--0.012
Final_Biomarker_NHANES_20072008$lbxlypct_Terms<-Final_Biomarker_NHANES_20072008$lbxlypct*Final_Biomarker_NHANES_20072008$lbxlypct_Wts

# Mean Cell Volume
summary(Final_Biomarker_NHANES_20072008$lbxmcvsi)
Final_Biomarker_NHANES_20072008$lbxmcvsi_Wts<-0.0268
Final_Biomarker_NHANES_20072008$lbxmcvsi_Terms<-Final_Biomarker_NHANES_20072008$lbxmcvsi*Final_Biomarker_NHANES_20072008$lbxmcvsi_Wts

# Red Cell Dist Width
summary(Final_Biomarker_NHANES_20072008$lbxrdw)
Final_Biomarker_NHANES_20072008$lbxrdw_Wts<-0.3306
Final_Biomarker_NHANES_20072008$lbxrdw_Terms<-Final_Biomarker_NHANES_20072008$lbxrdw*Final_Biomarker_NHANES_20072008$lbxrdw_Wts

# Alkaline Phosphatase
summary(Final_Biomarker_NHANES_20072008$lbxsapsi)
Final_Biomarker_NHANES_20072008$lbxsapsi_Wts<-0.0019
Final_Biomarker_NHANES_20072008$lbxsapsi_Terms<-Final_Biomarker_NHANES_20072008$lbxrdw*Final_Biomarker_NHANES_20072008$lbxsapsi_Wts

# White Blood Cells
summary(Final_Biomarker_NHANES_20072008$lbxwbcsi)
Final_Biomarker_NHANES_20072008$lbxwbcsi_Wts<-0.0554
Final_Biomarker_NHANES_20072008$lbxwbcsi_Terms<-Final_Biomarker_NHANES_20072008$lbxwbcsi*Final_Biomarker_NHANES_20072008$lbxwbcsi_Wts

# Age
summary(Final_Biomarker_NHANES_20072008$ridageyr)
Final_Biomarker_NHANES_20072008$ridageyr_Wts<-0.0804
Final_Biomarker_NHANES_20072008$ridageyr_Terms<-Final_Biomarker_NHANES_20072008$ridageyr*Final_Biomarker_NHANES_20072008$ridageyr_Wts

# Making a variable for "B0"
Final_Biomarker_NHANES_20072008$B0<--19.9067

# Making a variable which is the sum of all variables above.
Final_Biomarker_NHANES_20072008$LinComb<-rowSums(Final_Biomarker_NHANES_20072008[, c("B0", "lbdsalsi_Terms", "lbdscrsi_Terms", "lbdsglsi_Terms",
                                                                                     "lbxcrp_Terms", "lbxlypct_Terms", "lbxmcvsi_Terms","lbxrdw_Terms",
                                                                                     "lbxsapsi_Terms","lbxwbcsi_Terms","ridageyr_Terms")])

#Making a variable for "Gamma"
Final_Biomarker_NHANES_20072008$Gamma<-0.0076927

# Making a variable for "t"
Final_Biomarker_NHANES_20072008$t<-120

# Making a variable for "MortScore" in multiple steps
Final_Biomarker_NHANES_20072008$MortScore1 <-exp(Final_Biomarker_NHANES_20072008$Gamma*120)-1
Final_Biomarker_NHANES_20072008$MortScore2 <--exp(Final_Biomarker_NHANES_20072008$LinComb)
Final_Biomarker_NHANES_20072008$MortScore3 <-Final_Biomarker_NHANES_20072008$MortScore2/Final_Biomarker_NHANES_20072008$Gamma
Final_Biomarker_NHANES_20072008$MortScore4 <-exp(Final_Biomarker_NHANES_20072008$MortScore3)
Final_Biomarker_NHANES_20072008$MortScore5 <-1-Final_Biomarker_NHANES_20072008$MortScore4

# Finally, making a variable for PhenoAge.
Final_Biomarker_NHANES_20072008$PhenoAge<-141.50225+log(-0.00553*log(1-Final_Biomarker_NHANES_20072008$MortScore5))/0.09165
summary(Final_Biomarker_NHANES_20072008$PhenoAge)




# Preparing the Dataset to Calculate Phenoage for
# the Cycle 2009-2010

# Set the repository URL and API endpoint
url <- "https://api.github.com/repos/alialvi92/alialvi92/contents/NHANES%202009-2010"
endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s", "alialvi92/alialvi92", "NHANES%202009-2010")

# Make the API request
response <- GET(endpoint)

# Parse the response JSON
file_list <- fromJSON(content(response, as = "text"))

# Extract file names and URLs
file_names <- file_list$name
file_urls <- file_list$download_url

# Read in CSV files as a list of data frames
data_list <- lapply(file_urls, read.csv)
merged_df <- reduce(data_list, full_join, by = "seqn")

# Removing duplicate observations (generated from files that
# have "long" format data, where each respondent has multiple rows
# of data) merged_df_wo_duplicates <- merged_df %>% 
merged_df_wo_duplicates <- merged_df %>% distinct(seqn, .keep_all = TRUE)

# Selecting only those columns required for calculating 
# pheno age
Biomarker_NHANES_20092010<-merged_df_wo_duplicates[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                                       "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                       "lbxsapsi","lbxwbcsi","ridageyr")]

# Variables from NHANES (Other than laboratory values) 
# to be selected 
# sdmvpsu (id),sdmvstra (strata), wtint2yr (weights)-> These variables 
# will be used for weighting
# mcq010 mcq160g mcq160k (pulmonary), 
# mcq220 (cancer),
# lbdtcsi (dyslipidemia),
# mcq160b mcq160c mcq160f (Heart disease),
# kiq022 kiq020 (CKD), 
# indhhinc ind235 (family income), 
# dmdeduc (education),
# bmxbmi (bmi), 
# bpxdi1 (diastolic pressure) 
# bpxsy1 (systolic), 
# bpq020 (told had hypertension), 
# diq010 (told had diabetes), 
# alq150 (alcohol) , 
# smq040 smq020 (smoking variables), 
# pad200 pad320 paq650 paq665 (activity), 
# ridreth1 (race)

NHANES_20092010Variables<-merged_df_wo_duplicates[,c("seqn", "sdmvpsu",
                                                     "sdmvstra",
                                                     "wtint2yr",
                                                     "mcq010",
                                                     "mcq160g",
                                                     "mcq160k",
                                                     "mcq220",
                                                     "lbdtcsi",
                                                     "mcq160b",
                                                     "mcq160c",
                                                     "mcq160f",
                                                     "kiq022",
                                                     "indfmin2",
                                                     
                                                     "dmdeduc2",
                                                     "bmxbmi",
                                                     "bpxdi1",
                                                     "bpxsy1",
                                                     "bpq020",
                                                     "diq010",
                                                     "alq101",
                                                     "alq150",
                                                     "smq040",
                                                     "smq020",
                                                     "paq650",
                                                     "paq665",
                                                     "ridreth1")]

# Removing observations with missing values for each 
# lab value one by one to only select those observations
# in the end with no missing lab values
merged_df1<-subset(Biomarker_NHANES_20092010,!is.na(Biomarker_NHANES_20092010$lbdsalsi))
merged_df2<-subset(merged_df1,!is.na(merged_df1$lbdscrsi))
merged_df3<-subset(merged_df2,!is.na(merged_df2$lbdsglsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbxcrp))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbxlypct))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxmcvsi))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxrdw))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxsapsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxwbcsi))
merged_df10<-subset(merged_df9,!is.na(merged_df9$ridageyr))

# Renaming the dataset to its respective year cycle
Final_Biomarker_NHANES_20092010<-merged_df10

# Calculating Phenoage

# First Calculating the Terms for Each Individual Lab Value

# Albumin
summary(Final_Biomarker_NHANES_20092010$lbdsalsi)
Final_Biomarker_NHANES_20092010$lbdsalsi_Wts<--0.0336
Final_Biomarker_NHANES_20092010$lbdsalsi_Terms<-Final_Biomarker_NHANES_20092010$lbdsalsi*Final_Biomarker_NHANES_20092010$lbdsalsi_Wts

# Creatinine
summary(Final_Biomarker_NHANES_20092010$lbdscrsi)
Final_Biomarker_NHANES_20092010$lbdscrsi_Wts<-0.0095
Final_Biomarker_NHANES_20092010$lbdscrsi_Terms<-Final_Biomarker_NHANES_20092010$lbdscrsi*Final_Biomarker_NHANES_20092010$lbdscrsi_Wts

# Glucose
summary(Final_Biomarker_NHANES_20092010$lbdsglsi)
Final_Biomarker_NHANES_20092010$lbdsglsi_Wts<-0.1953
Final_Biomarker_NHANES_20092010$lbdsglsi_Terms<-Final_Biomarker_NHANES_20092010$lbdsglsi*Final_Biomarker_NHANES_20092010$lbdsglsi_Wts

# CRP
summary(Final_Biomarker_NHANES_20092010$lbxcrp)
Final_Biomarker_NHANES_20092010$lbxcrp_Conversion<-Final_Biomarker_NHANES_20092010$lbxcrp*0.1
Final_Biomarker_NHANES_20092010$lbxcrp_LN_Conversion<-log(Final_Biomarker_NHANES_20092010$lbxcrp_Conversion)
Final_Biomarker_NHANES_20092010$lbxcrp_Wts<-0.0954
Final_Biomarker_NHANES_20092010$lbxcrp_Terms<-Final_Biomarker_NHANES_20092010$lbxcrp_LN_Conversion*Final_Biomarker_NHANES_20092010$lbxcrp_Wts

# Lymphocyte
summary(Final_Biomarker_NHANES_20092010$lbxlypct)
Final_Biomarker_NHANES_20092010$lbxlypct_Wts<--0.012
Final_Biomarker_NHANES_20092010$lbxlypct_Terms<-Final_Biomarker_NHANES_20092010$lbxlypct*Final_Biomarker_NHANES_20092010$lbxlypct_Wts

# Mean Cell Volume
summary(Final_Biomarker_NHANES_20092010$lbxmcvsi)
Final_Biomarker_NHANES_20092010$lbxmcvsi_Wts<-0.0268
Final_Biomarker_NHANES_20092010$lbxmcvsi_Terms<-Final_Biomarker_NHANES_20092010$lbxmcvsi*Final_Biomarker_NHANES_20092010$lbxmcvsi_Wts

# Red Cell Dist Width
summary(Final_Biomarker_NHANES_20092010$lbxrdw)
Final_Biomarker_NHANES_20092010$lbxrdw_Wts<-0.3306
Final_Biomarker_NHANES_20092010$lbxrdw_Terms<-Final_Biomarker_NHANES_20092010$lbxrdw*Final_Biomarker_NHANES_20092010$lbxrdw_Wts

# Alkaline Phosphatase
summary(Final_Biomarker_NHANES_20092010$lbxsapsi)
Final_Biomarker_NHANES_20092010$lbxsapsi_Wts<-0.0019
Final_Biomarker_NHANES_20092010$lbxsapsi_Terms<-Final_Biomarker_NHANES_20092010$lbxrdw*Final_Biomarker_NHANES_20092010$lbxsapsi_Wts

# White Blood Cells
summary(Final_Biomarker_NHANES_20092010$lbxwbcsi)
Final_Biomarker_NHANES_20092010$lbxwbcsi_Wts<-0.0554
Final_Biomarker_NHANES_20092010$lbxwbcsi_Terms<-Final_Biomarker_NHANES_20092010$lbxwbcsi*Final_Biomarker_NHANES_20092010$lbxwbcsi_Wts

# Age
summary(Final_Biomarker_NHANES_20092010$ridageyr)
Final_Biomarker_NHANES_20092010$ridageyr_Wts<-0.0804
Final_Biomarker_NHANES_20092010$ridageyr_Terms<-Final_Biomarker_NHANES_20092010$ridageyr*Final_Biomarker_NHANES_20092010$ridageyr_Wts

# Making a variable for "B0"
Final_Biomarker_NHANES_20092010$B0<--19.9067

# Making a variable which is the sum of all variables above.
Final_Biomarker_NHANES_20092010$LinComb<-rowSums(Final_Biomarker_NHANES_20092010[, c("B0", "lbdsalsi_Terms", "lbdscrsi_Terms", "lbdsglsi_Terms",
                                                                                     "lbxcrp_Terms", "lbxlypct_Terms", "lbxmcvsi_Terms","lbxrdw_Terms",
                                                                                     "lbxsapsi_Terms","lbxwbcsi_Terms","ridageyr_Terms")])

#Making a variable for "Gamma"
Final_Biomarker_NHANES_20092010$Gamma<-0.0076927

# Making a variable for "t"
Final_Biomarker_NHANES_20092010$t<-120

# Making a variable for "MortScore" in multiple steps
Final_Biomarker_NHANES_20092010$MortScore1 <-exp(Final_Biomarker_NHANES_20092010$Gamma*120)-1
Final_Biomarker_NHANES_20092010$MortScore2 <--exp(Final_Biomarker_NHANES_20092010$LinComb)
Final_Biomarker_NHANES_20092010$MortScore3 <-Final_Biomarker_NHANES_20092010$MortScore2/Final_Biomarker_NHANES_20092010$Gamma
Final_Biomarker_NHANES_20092010$MortScore4 <-exp(Final_Biomarker_NHANES_20092010$MortScore3)
Final_Biomarker_NHANES_20092010$MortScore5 <-1-Final_Biomarker_NHANES_20092010$MortScore4

# Finally, making a variable for PhenoAge.
Final_Biomarker_NHANES_20092010$PhenoAge<-141.50225+log(-0.00553*log(1-Final_Biomarker_NHANES_20092010$MortScore5))/0.09165
summary(Final_Biomarker_NHANES_20092010$PhenoAge)


# NOT USING THE CYCLES 2011-2012 AND 2013-2014
# BECAUSE THERE IS NO VARIABLE FOR CRP IN THESE CYCLES

# Preparing the Dataset to Calculate Phenoage for
# the Cycle 2015-2016

# Set the repository URL and API endpoint
url <- "https://api.github.com/repos/alialvi92/alialvi92/contents/NHANES%202015-2016"
endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s", "alialvi92/alialvi92", "NHANES%202015-2016")

# Make the API request
response <- GET(endpoint)

# Parse the response JSON
file_list <- fromJSON(content(response, as = "text"))

# Extract file names and URLs
file_names <- file_list$name
file_urls <- file_list$download_url

# Read in CSV files as a list of data frames
data_list <- lapply(file_urls, read.csv)
merged_df <- reduce(data_list, full_join, by = "seqn")

# Removing duplicate observations (generated from files that
# have "long" format data, where each respondent has multiple rows
# of data) merged_df_wo_duplicates <- merged_df %>% 
merged_df_wo_duplicates <- merged_df %>% distinct(seqn, .keep_all = TRUE)

# Selecting only those columns required for calculating 
# pheno age
Biomarker_NHANES_20152016<-merged_df_wo_duplicates[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                                       "lbxhscrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                       "lbxsapsi","lbxwbcsi","ridageyr")]


# Variables from NHANES (Other than laboratory values) 
# to be selected 
# sdmvpsu (id),sdmvstra (strata), wtint2yr (weights)-> These variables 
# will be used for weighting
# mcq010 mcq160g mcq160k (pulmonary), 
# mcq220 (cancer),
# lbdtcsi (dyslipidemia),
# mcq160b mcq160c mcq160f (Heart disease),
# kiq022 kiq020 (CKD), 
# indhhinc ind235 (family income), 
# dmdeduc (education),
# bmxbmi (bmi), 
# bpxdi1 (diastolic pressure) 
# bpxsy1 (systolic), 
# bpq020 (told had hypertension), 
# diq010 (told had diabetes), 
# alq150 alq151 (alcohol) , 
# smq040 smq020 (smoking variables), 
# pad200 pad320 paq650 paq665 (activity), 
# ridreth1 (race)

NHANES_20152016Variables<-merged_df_wo_duplicates[,c("seqn", "sdmvpsu",
                                                     "sdmvstra",
                                                     "wtint2yr",
                                                     "mcq010",
                                                     "mcq160g",
                                                     "mcq160k",
                                                     "mcq220",
                                                     "lbdtcsi",
                                                     "mcq160b",
                                                     "mcq160c",
                                                     "mcq160f",
                                                     "kiq022",
                                                     "indfmin2",
                                                     
                                                     "dmdeduc2",
                                                     "bmxbmi",
                                                     "bpxdi1",
                                                     "bpxsy1",
                                                     "bpq020",
                                                     "diq010",
                                                     "alq101",
                                                     "alq151",
                                                     "smq040",
                                                     "smq020",
                                                     "paq650",
                                                     "paq665",
                                                     "ridreth1")]

# Removing observations with missing values for each 
# lab value one by one to only select those observations
# in the end with no missing lab values
merged_df1<-subset(Biomarker_NHANES_20152016,!is.na(Biomarker_NHANES_20152016$lbdsalsi))
merged_df2<-subset(merged_df1,!is.na(merged_df1$lbdscrsi))
merged_df3<-subset(merged_df2,!is.na(merged_df2$lbdsglsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbxhscrp))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbxlypct))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxmcvsi))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxrdw))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxsapsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxwbcsi))
merged_df10<-subset(merged_df9,!is.na(merged_df9$ridageyr))

# Renaming the dataset to its respective year cycle
Final_Biomarker_NHANES_20152016<-merged_df10

# Calculating Phenoage

# First Calculating the Terms for Each Individual Lab Value

# Albumin
summary(Final_Biomarker_NHANES_20152016$lbdsalsi)
Final_Biomarker_NHANES_20152016$lbdsalsi_Wts<--0.0336
Final_Biomarker_NHANES_20152016$lbdsalsi_Terms<-Final_Biomarker_NHANES_20152016$lbdsalsi*Final_Biomarker_NHANES_20152016$lbdsalsi_Wts

# Creatinine
summary(Final_Biomarker_NHANES_20152016$lbdscrsi)
Final_Biomarker_NHANES_20152016$lbdscrsi_Wts<-0.0095
Final_Biomarker_NHANES_20152016$lbdscrsi_Terms<-Final_Biomarker_NHANES_20152016$lbdscrsi*Final_Biomarker_NHANES_20152016$lbdscrsi_Wts

# Glucose
summary(Final_Biomarker_NHANES_20152016$lbdsglsi)
Final_Biomarker_NHANES_20152016$lbdsglsi_Wts<-0.1953
Final_Biomarker_NHANES_20152016$lbdsglsi_Terms<-Final_Biomarker_NHANES_20152016$lbdsglsi*Final_Biomarker_NHANES_20152016$lbdsglsi_Wts

# CRP
summary(Final_Biomarker_NHANES_20152016$lbxhscrp)
Final_Biomarker_NHANES_20152016$lbxcrp_Conversion<-Final_Biomarker_NHANES_20152016$lbxhscrp*0.1
Final_Biomarker_NHANES_20152016$lbxcrp_LN_Conversion<-log(Final_Biomarker_NHANES_20152016$lbxcrp_Conversion)
Final_Biomarker_NHANES_20152016$lbxcrp_Wts<-0.0954
Final_Biomarker_NHANES_20152016$lbxcrp_Terms<-Final_Biomarker_NHANES_20152016$lbxcrp_LN_Conversion*Final_Biomarker_NHANES_20152016$lbxcrp_Wts

# Lymphocyte
summary(Final_Biomarker_NHANES_20152016$lbxlypct)
Final_Biomarker_NHANES_20152016$lbxlypct_Wts<--0.012
Final_Biomarker_NHANES_20152016$lbxlypct_Terms<-Final_Biomarker_NHANES_20152016$lbxlypct*Final_Biomarker_NHANES_20152016$lbxlypct_Wts

# Mean Cell Volume
summary(Final_Biomarker_NHANES_20152016$lbxmcvsi)
Final_Biomarker_NHANES_20152016$lbxmcvsi_Wts<-0.0268
Final_Biomarker_NHANES_20152016$lbxmcvsi_Terms<-Final_Biomarker_NHANES_20152016$lbxmcvsi*Final_Biomarker_NHANES_20152016$lbxmcvsi_Wts

# Red Cell Dist Width
summary(Final_Biomarker_NHANES_20152016$lbxrdw)
Final_Biomarker_NHANES_20152016$lbxrdw_Wts<-0.3306
Final_Biomarker_NHANES_20152016$lbxrdw_Terms<-Final_Biomarker_NHANES_20152016$lbxrdw*Final_Biomarker_NHANES_20152016$lbxrdw_Wts

# Alkaline Phosphatase
summary(Final_Biomarker_NHANES_20152016$lbxsapsi)
Final_Biomarker_NHANES_20152016$lbxsapsi_Wts<-0.0019
Final_Biomarker_NHANES_20152016$lbxsapsi_Terms<-Final_Biomarker_NHANES_20152016$lbxrdw*Final_Biomarker_NHANES_20152016$lbxsapsi_Wts

# White Blood Cells
summary(Final_Biomarker_NHANES_20152016$lbxwbcsi)
Final_Biomarker_NHANES_20152016$lbxwbcsi_Wts<-0.0554
Final_Biomarker_NHANES_20152016$lbxwbcsi_Terms<-Final_Biomarker_NHANES_20152016$lbxwbcsi*Final_Biomarker_NHANES_20152016$lbxwbcsi_Wts

# Age
summary(Final_Biomarker_NHANES_20152016$ridageyr)
Final_Biomarker_NHANES_20152016$ridageyr_Wts<-0.0804
Final_Biomarker_NHANES_20152016$ridageyr_Terms<-Final_Biomarker_NHANES_20152016$ridageyr*Final_Biomarker_NHANES_20152016$ridageyr_Wts

# Making a variable for "B0"
Final_Biomarker_NHANES_20152016$B0<--19.9067

# Making a variable which is the sum of all variables above.
Final_Biomarker_NHANES_20152016$LinComb<-rowSums(Final_Biomarker_NHANES_20152016[, c("B0", "lbdsalsi_Terms", "lbdscrsi_Terms", "lbdsglsi_Terms",
                                                                                     "lbxcrp_Terms", "lbxlypct_Terms", "lbxmcvsi_Terms","lbxrdw_Terms",
                                                                                     "lbxsapsi_Terms","lbxwbcsi_Terms","ridageyr_Terms")])

#Making a variable for "Gamma"
Final_Biomarker_NHANES_20152016$Gamma<-0.0076927

# Making a variable for "t"
Final_Biomarker_NHANES_20152016$t<-120

# Making a variable for "MortScore" in multiple steps
Final_Biomarker_NHANES_20152016$MortScore1 <-exp(Final_Biomarker_NHANES_20152016$Gamma*120)-1
Final_Biomarker_NHANES_20152016$MortScore2 <--exp(Final_Biomarker_NHANES_20152016$LinComb)
Final_Biomarker_NHANES_20152016$MortScore3 <-Final_Biomarker_NHANES_20152016$MortScore2/Final_Biomarker_NHANES_20152016$Gamma
Final_Biomarker_NHANES_20152016$MortScore4 <-exp(Final_Biomarker_NHANES_20152016$MortScore3)
Final_Biomarker_NHANES_20152016$MortScore5 <-1-Final_Biomarker_NHANES_20152016$MortScore4

# Finally, making a variable for PhenoAge.
Final_Biomarker_NHANES_20152016$PhenoAge<-141.50225+log(-0.00553*log(1-Final_Biomarker_NHANES_20152016$MortScore5))/0.09165
summary(Final_Biomarker_NHANES_20152016$PhenoAge)


# Preparing the Dataset to Calculate Phenoage for
# the Cycle 2017-2018

# Set the repository URL and API endpoint
url <- "https://api.github.com/repos/alialvi92/alialvi92/contents/NHANES%202017-2018"
endpoint <- sprintf("https://api.github.com/repos/%s/contents/%s", "alialvi92/alialvi92", "NHANES%202017-2018")

# Make the API request
response <- GET(endpoint)

# Parse the response JSON
file_list <- fromJSON(content(response, as = "text"))

# Extract file names and URLs
file_names <- file_list$name
file_urls <- file_list$download_url

# Read in CSV files as a list of data frames
data_list <- lapply(file_urls, read.csv)
merged_df <- reduce(data_list, full_join, by = "seqn")

# Removing duplicate observations (generated from files that
# have "long" format data, where each respondent has multiple rows
# of data) merged_df_wo_duplicates <- merged_df %>% 
merged_df_wo_duplicates <- merged_df %>% distinct(seqn, .keep_all = TRUE)

# Variables from NHANES (Other than laboratory values) 
# to be selected 
# sdmvpsu (id),sdmvstra (strata), wtint2yr (weights)-> These variables 
# will be used for weighting
# mcq010 mcq160g mcq160k (pulmonary), 
# mcq220 (cancer),
# lbdtcsi (dyslipidemia),
# mcq160b mcq160c mcq160f (Heart disease),
# kiq022 kiq020 (CKD), 
# indhhinc ind235 (family income), 
# dmdeduc (education),
# bmxbmi (bmi), 
# bpxdi1 (diastolic pressure) 
# bpxsy1 (systolic), 
# bpq020 (told had hypertension), 
# diq010 (told had diabetes), 
# alq150 alq151 (alcohol) , 
# smq040 smq020 (smoking variables), 
# pad200 pad320 paq650 paq665 (activity), 
# ridreth1 (race)

NHANES_20172018Variables<-merged_df_wo_duplicates[,c("seqn", "sdmvpsu",
                                                     "sdmvstra",
                                                     "wtint2yr",
                                                     "mcq010",
                                                     "mcq160g",
                                                     "mcq160k",
                                                     "mcq220",
                                                     "lbdtcsi",
                                                     "mcq160b",
                                                     "mcq160c",
                                                     "mcq160f",
                                                     "kiq022",
                                                     "indfmin2",
                                                     
                                                     "dmdeduc2",
                                                     "bmxbmi",
                                                     "bpxdi1",
                                                     "bpxsy1",
                                                     "bpq020",
                                                     "diq010",
                                                     "alq121",
                                                     "alq151",
                                                     "smq040",
                                                     "smq020",
                                                     "paq650",
                                                     "paq665",
                                                     "ridreth1")]
NHANES_20172018Variables$alq101<-NHANES_20172018Variables$alq121

# Selecting only those columns required for calculating 
# pheno age
Biomarker_NHANES_20172018<-merged_df_wo_duplicates[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                                       "lbxhscrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                       "lbxsapsi","lbxwbcsi","ridageyr")]

# Removing observations with missing values for each 
# lab value one by one to only select those observations
# in the end with no missing lab values
merged_df1<-subset(Biomarker_NHANES_20172018,!is.na(Biomarker_NHANES_20172018$lbdsalsi))
merged_df2<-subset(merged_df1,!is.na(merged_df1$lbdscrsi))
merged_df3<-subset(merged_df2,!is.na(merged_df2$lbdsglsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbxhscrp))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbxlypct))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxmcvsi))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxrdw))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxsapsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxwbcsi))
merged_df10<-subset(merged_df9,!is.na(merged_df9$ridageyr))

# Renaming the dataset to its respective year cycle
Final_Biomarker_NHANES_20172018<-merged_df10

# Calculating Phenoage

# First Calculating the Terms for Each Individual Lab Value

# Albumin
summary(Final_Biomarker_NHANES_20172018$lbdsalsi)
Final_Biomarker_NHANES_20172018$lbdsalsi_Wts<--0.0336
Final_Biomarker_NHANES_20172018$lbdsalsi_Terms<-Final_Biomarker_NHANES_20172018$lbdsalsi*Final_Biomarker_NHANES_20172018$lbdsalsi_Wts

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

# Age
summary(Final_Biomarker_NHANES_20172018$ridageyr)
Final_Biomarker_NHANES_20172018$ridageyr_Wts<-0.0804
Final_Biomarker_NHANES_20172018$ridageyr_Terms<-Final_Biomarker_NHANES_20172018$ridageyr*Final_Biomarker_NHANES_20172018$ridageyr_Wts

# Making a variable for "B0"
Final_Biomarker_NHANES_20172018$B0<--19.9067

# Making a variable which is the sum of all variables above.
Final_Biomarker_NHANES_20172018$LinComb<-rowSums(Final_Biomarker_NHANES_20172018[, c("B0", "lbdsalsi_Terms", "lbdscrsi_Terms", "lbdsglsi_Terms",
                                                                                     "lbxcrp_Terms", "lbxlypct_Terms", "lbxmcvsi_Terms","lbxrdw_Terms",
                                                                                     "lbxsapsi_Terms","lbxwbcsi_Terms","ridageyr_Terms")])
#Making a variable for "Gamma"
Final_Biomarker_NHANES_20172018$Gamma<-0.0076927

# Making a variable for "t"
Final_Biomarker_NHANES_20172018$t<-120

# Making a variable for "MortScore" in multiple steps
Final_Biomarker_NHANES_20172018$MortScore1 <-exp(Final_Biomarker_NHANES_20172018$Gamma*120)-1
Final_Biomarker_NHANES_20172018$MortScore2 <--exp(Final_Biomarker_NHANES_20172018$LinComb)
Final_Biomarker_NHANES_20172018$MortScore3 <-Final_Biomarker_NHANES_20172018$MortScore2/Final_Biomarker_NHANES_20172018$Gamma
Final_Biomarker_NHANES_20172018$MortScore4 <-exp(Final_Biomarker_NHANES_20172018$MortScore3)
Final_Biomarker_NHANES_20172018$MortScore5 <-1-Final_Biomarker_NHANES_20172018$MortScore4

# Finally, making a variable for PhenoAge.
Final_Biomarker_NHANES_20172018$PhenoAge<-141.50225+log(-0.00553*log(1-Final_Biomarker_NHANES_20172018$MortScore5))/0.09165
summary(Final_Biomarker_NHANES_20172018$PhenoAge)


# TROUBLESHOOTING: Code to figure out which File in the
# NHANES cycle does not have the variable "seqn"
map_lgl(df_list, ~ "seqn" %in% names(.x))
names(df_list)
false_indices <- which(map_lgl(df_list, ~ "seqn" %in% names(.x)) == FALSE)
false_df_names <- names(df_list)[false_indices]
false_df_names


# Combining All Years (Row-binding) of Biomarker Datasets all years cycles
library(data.table)
Biomarker_All_Years<-rbindlist(list(Final_Biomarker_NHANES_19992000,
                                    Final_Biomarker_NHANES_20012002,
                                    Final_Biomarker_NHANES_20032004,
                                    Final_Biomarker_NHANES_20052006,
                                    Final_Biomarker_NHANES_20072008,
                                    Final_Biomarker_NHANES_20092010,
                                    Final_Biomarker_NHANES_20152016,
                                    Final_Biomarker_NHANES_20172018),
                               fill=TRUE)

# Combining All NHANES Variables files into one single dataset
NHANES_Variables_19992020<-rbindlist(list(NHANES_19992000Variables,
                                          NHANES_20012002Variables,
                                          NHANES_20032004Variables,
                                          NHANES_20052006Variables,
                                          NHANES_20072008Variables,
                                          NHANES_20092010Variables,
                                          NHANES_20152016Variables,
                                          NHANES_20172018Variables),fill=TRUE)
# Only selecting those cases in this combined file that
# are in the Final_Biomarker file
NHANES_Variables_19992020<-subset(NHANES_Variables_19992020,
                                  NHANES_Variables_19992020$seqn %in% Final_subset_Biomarker_All_Years$seqn )





breaks <- seq(10, 130, by = 10)
# Use cut() function to create deciles
Biomarker_All_Years$age_deciles <- cut(Biomarker_All_Years$ridageyr, breaks = breaks, labels = c( "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "101-110", "111-120","121-130"))

#Making a subset where we are excluding those aged <18
subset_Biomarker_All_Years<-subset(Biomarker_All_Years,
                                   Biomarker_All_Years$ridageyr>18)


breaks <- seq(-20, 130, by = 10)
subset_Biomarker_All_Years$Phenotypical_Age_deciles <- cut(subset_Biomarker_All_Years$PhenoAge, breaks = breaks, labels = c("-20- -10", "-10-0", "0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "101-110", "111-120","121-130"))

# Making a variable for difference between age and phenotypic age
subset_Biomarker_All_Years$Difference_Age_Phenotypic_Age<-subset_Biomarker_All_Years$ridageyr-subset_Biomarker_All_Years$PhenoAge
summary(subset_Biomarker_All_Years$Difference_Age_Phenotypic_Age)

#Checking which observations are showing as "infinity"
which(is.infinite(subset_Biomarker_All_Years$Difference_Age_Phenotypic_Age))

# Removing observations where Phenoage and difference between Age and Phenoage are infinity
subset_Biomarker_All_Yearssubset_finite <- subset_Biomarker_All_Years[is.finite(subset_Biomarker_All_Years$PhenoAge),]

summary(subset_Biomarker_All_Yearssubset_finite$Difference_Age_Phenotypic_Age)

Final_subset_Biomarker_All_Years<-subset_Biomarker_All_Yearssubset_finite
# Changing age decile values "11-20" and "21-30" to "19-30"
Final_subset_Biomarker_All_Years$age_deciles<-as.factor(ifelse(Final_subset_Biomarker_All_Years$age_deciles=="11-20"|
                                                           Final_subset_Biomarker_All_Years$age_deciles=="21-30","19-30",
                                                         ifelse(Final_subset_Biomarker_All_Years$age_deciles=="31-40","31-40",
                                                                ifelse(Final_subset_Biomarker_All_Years$age_deciles=="41-50","41-50",
                                                                       ifelse(Final_subset_Biomarker_All_Years$age_deciles=="51-60","51-60",
                                                                              ifelse(Final_subset_Biomarker_All_Years$age_deciles=="61-70","61-70",
                                                                                     ifelse(Final_subset_Biomarker_All_Years$age_deciles=="71-80","71-80",
                                                                                            ifelse(Final_subset_Biomarker_All_Years$age_deciles=="81-90","81-90",NA))))))))
summary(Final_subset_Biomarker_All_Years$Difference_Age_Phenotypic_Age)

# Creating deciles for difference in age & phenotypic age
breaks <- seq(-100, 40, by = 10)
Final_subset_Biomarker_All_Years$Difference_Deciles <- cut(Final_subset_Biomarker_All_Years$Difference_Age_Phenotypic_Age, breaks = breaks, labels = c("-100 - -90","-90 - -80", "-80 - -70" , "-70 - -60", "-60 - -50", "-50 - -40", 
                                                                                                                                             "-40 - -30", "-30 - -20", "-20 - -10",
                                                                                                                                             "-10 - 0", "0-10","10-20","20-30","30-40"))
#Creating a histogram
density <- density(Final_subset_Biomarker_All_Years$PhenoAge)
q_2.5 <- quantile(Final_subset_Biomarker_All_Years$PhenoAge, probs = 0.025, na.rm = TRUE)
q_97.5 <- quantile(Final_subset_Biomarker_All_Years$PhenoAge, probs = 0.975, na.rm = TRUE)

ggplot(data = Final_subset_Biomarker_All_Years, aes(x = PhenoAge)) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "gray") +
  stat_density(aes(y = ..density.., ymin = 0, ymax = density$y[which.max(density$x <= quantile(density$x, 0.975))]), 
               geom = "ribbon", alpha = 0.5, fill = "blue") +
  geom_vline(aes(xintercept = q_2.5), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = q_97.5), linetype = "dashed", color = "red") +
  xlab("Phenotypic Age") +
  ylab("Density") +
  ggtitle("Histogram of Phenotypic Age with 95% CI") +
  annotate("text", x = q_2.5, y = 0.01, label = round(q_2.5, 2), color = "red", vjust = -1) +
  annotate("text", x = q_97.5, y = 0.01, label = round(q_97.5, 2), color = "red", vjust = -1)

#Calculate mean and standard deviation
mean_age <- mean(Final_subset_Biomarker_All_Years$PhenoAge, na.rm = TRUE)
sd_age <- sd(Final_subset_Biomarker_All_Years$PhenoAge, na.rm = TRUE)

ggplot(data = Final_subset_Biomarker_All_Years, aes(x = PhenoAge)) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "gray") +
  stat_density(aes(y = ..density.., ymin = 0, ymax = density$y[which.max(density$x <= mean_age + 2 * sd_age)]), 
               geom = "ribbon", alpha = 0.5, fill = "blue") +
  geom_vline(aes(xintercept = mean_age - 2 * sd_age), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = mean_age + 2 * sd_age), linetype = "dashed", color = "red") +
  xlab("Phenotypic Age") +
  ylab("Density") +
  ggtitle("Histogram of Phenotypic Age with 2SD CI") +
  annotate("text", x = mean_age - 2 * sd_age, y = 0.01, label = round(mean_age - 2 * sd_age, 2), color = "red", vjust = -1) +
  annotate("text", x = mean_age + 2 * sd_age, y = 0.01, label = round(mean_age + 2 * sd_age, 2), color = "red", vjust = -1)


#Creating a histogram
density <- density(Final_subset_Biomarker_All_Years$Difference_Age_Phenotypic_Age)
q_2.5 <- quantile(Final_subset_Biomarker_All_Years$Difference_Age_Phenotypic_Age, probs = 0.025, na.rm = TRUE)
q_97.5 <- quantile(Final_subset_Biomarker_All_Years$Difference_Age_Phenotypic_Age, probs = 0.975, na.rm = TRUE)

ggplot(data = Final_subset_Biomarker_All_Years, aes(x = Difference_Age_Phenotypic_Age)) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "gray") +
  stat_density(aes(y = ..density.., ymin = 0, ymax = density$y[which.max(density$x <= quantile(density$x, 0.975))]), 
               geom = "ribbon", alpha = 0.5, fill = "blue") +
  geom_vline(aes(xintercept = q_2.5), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = q_97.5), linetype = "dashed", color = "red") +
  xlab("Difference Between Actual Age & Phenotypic Age") +
  ylab("Density") +
  ggtitle("Histogram of Difference Between Actual Age and Phenotypic Age with 95% CI") +
  annotate("text", x = q_2.5, y = 0.01, label = round(q_2.5, 2), color = "red", vjust = -1) +
  annotate("text", x = q_97.5, y = 0.01, label = round(q_97.5, 2), color = "red", vjust = -1)


#Calculate mean and standard deviation
mean_age <- mean(Final_subset_Biomarker_All_Years$Difference_Age_Phenotypic_Age, na.rm = TRUE)
sd_age <- sd(Final_subset_Biomarker_All_Years$Difference_Age_Phenotypic_Age, na.rm = TRUE)

ggplot(data = Final_subset_Biomarker_All_Years, aes(x = Difference_Age_Phenotypic_Age)) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "gray") +
  stat_density(aes(y = ..density.., ymin = 0, ymax = density$y[which.max(density$x <= mean_age + 2 * sd_age)]), 
               geom = "ribbon", alpha = 0.5, fill = "blue") +
  geom_vline(aes(xintercept = mean_age - 2 * sd_age), linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept = mean_age + 2 * sd_age), linetype = "dashed", color = "red") +
  xlab("Difference Between Actual Age and Phenotypic Age") +
  ylab("Density") +
  ggtitle("Histogram of Difference Between Actual Age & Phenotypic Age with 2SD CI") +
  annotate("text", x = mean_age - 2 * sd_age, y = 0.01, label = round(mean_age - 2 * sd_age, 2), color = "red", vjust = -1) +
  annotate("text", x = mean_age + 2 * sd_age, y = 0.01, label = round(mean_age + 2 * sd_age, 2), color = "red", vjust = -1)

# Create the Density Plot
ggplot(Final_subset_Biomarker_All_Years, aes(x = PhenoAge)) +
  geom_density() +
  geom_vline(xintercept = quantile(Final_subset_Biomarker_All_Years$PhenoAge, c(0.05, 0.95)), color = "red", linetype = "dashed") +
  xlab("Phenotypic Age") +
  ylab("Density") +
  ggtitle("Density plot of Phenotypic Age with Outlier Range")



library(ggplot2)
Final_subset_Biomarker_All_Years$Phenotypical_Age_deciles <- factor(Final_subset_Biomarker_All_Years$Phenotypical_Age_deciles, 
                                                               levels = c("-10-0", "0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "101-110", "111-120", "121-130"))
# Create a data frame with the deciles and their frequency
df3 <- data.frame(table(Final_subset_Biomarker_All_Years$Phenotypical_Age_deciles))

# Calculate the percentage of each frequency
df3$percent <- round(df3$Freq / sum(df3$Freq) * 100, 2)

# Create a bar chart using ggplot2 and add text annotations
ggplot(df3, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = paste0("n=", Freq, "\n", percent, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribution of Phenotypic Age", x = "Deciles", y = "Frequency")


# Create a data frame with the deciles and their frequency
df4 <- data.frame(table(Final_subset_Biomarker_All_Years$age_deciles))

# Calculate the percentage of each frequency
df4$percent <- round(df4$Freq / sum(df4$Freq) * 100, 2)

# Create a bar chart using ggplot2 and add text annotations
ggplot(df4, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = paste0("n=", Freq, "\n", percent, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribution of Actual Age", x = "Deciles", y = "Frequency")


# Create a data frame with the difference deciles and their frequency
df5 <- data.frame(table(Final_subset_Biomarker_All_Years$Difference_Deciles))

# Calculate the percentage of each frequency
df5$percent <- round(df5$Freq / sum(df5$Freq) * 100, 2)

# Create a bar chart using ggplot2 and add text annotations
ggplot(df5, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = paste0("n=", Freq, "\n", percent, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribution of Difference Between Actual Age & Phenotypic Age", x = "Deciles", y = "Frequency")



# 1 Age groups (Age Deciles)
# variable ridageyr

# 2 Age Groups 2 (Age Groups 19-40, 41-65 and 65 above)
# Variable ridageyr

# 3 Gender
# Variable riagendr


# 4 Race
NHANES_Variables_19992020$RACE_RECODE<-as.factor(ifelse(NHANES_Variables_19992020$ridreth1=="1","Mexican American",
                                                        ifelse(NHANES_Variables_19992020$ridreth1=="2","Other Hispanic",
                                                               ifelse(NHANES_Variables_19992020$ridreth1=="3","Non-Hispanic White",
                                                                      ifelse(NHANES_Variables_19992020$ridreth1=="4","Non-Hispanic Black",
                                                                             "Other Race - Including Multi-Racial")))))
summary(NHANES_Variables_19992020$RACE_RECODE)

# 5 BMI
NHANES_Variables_19992020$BMI_Groups<-as.factor(ifelse(NHANES_Variables_19992020$bmxbmi<20,"<20",
                                                                         ifelse(NHANES_Variables_19992020$bmxbmi>20&NHANES_Variables_19992020$bmxbmi<24.99,"20-25",
                                                                                ifelse(NHANES_Variables_19992020$bmxbmi>25&NHANES_Variables_19992020$bmxbmi<29.99,"25-30",
                                                                                       ifelse(NHANES_Variables_19992020$bmxbmi>30 &
                                                                                                NHANES_Variables_19992020$bmxbmi<34.99,"30-35",
                                                                                              ifelse(NHANES_Variables_19992020$bmxbmi>35 &
                                                                                                       NHANES_Variables_19992020$bmxbmi<39.99,"35-40",
                                                                                                     ifelse(NHANES_Variables_19992020$bmxbmi>40,"40 or higher",NA)))))))
summary(NHANES_Variables_19992020$BMI_Groups)
#Income
NHANES_Variables_19992020$familyincome20072020<-ifelse(NHANES_Variables_19992020$indfmin2=="1","$ <5000",
                                 ifelse(NHANES_Variables_19992020$indfmin2=="2" ,"5000-10000",
                                        ifelse(NHANES_Variables_19992020$indfmin2=="3","10000-15000",
                                               ifelse(NHANES_Variables_19992020$indfmin2=="4","15000-2000",
                                                      ifelse(NHANES_Variables_19992020$indfmin2=="5","20000-25000",
                                                             ifelse(NHANES_Variables_19992020$indfmin2=="6","25000-35000",
                                                                    ifelse(NHANES_Variables_19992020$indfmin2=="7" ,"35000-45000",
                                                                           ifelse(NHANES_Variables_19992020$indfmin2=="8","45000-55000",
                                                                                  ifelse(NHANES_Variables_19992020$indfmin2=="9","55000-65000",
                                                                                         ifelse(NHANES_Variables_19992020$indfmin2=="10","65000-75000",
                                                                                                ifelse(NHANES_Variables_19992020$indfmin2=="11","75000-100000",
                                                                                                       ifelse(NHANES_Variables_19992020$indfmin2=="12",">100000",NA))))))))))))




NHANES_Variables_19992020$familyincomeannual19992006<-ifelse(NHANES_Variables_19992020$indfminc=="1","$ <5000",
                                              ifelse(NHANES_Variables_19992020$indfminc=="2" ,"5000-10000",
                                                     ifelse(NHANES_Variables_19992020$indfminc=="3","10000-15000",
                                                            ifelse(NHANES_Variables_19992020$indfminc=="4","15000-2000",
                                                                   ifelse(NHANES_Variables_19992020$indfminc=="5","20000-25000",
                                                                          ifelse(NHANES_Variables_19992020$indfminc=="6","25000-35000",
                                                                                 ifelse(NHANES_Variables_19992020$indfminc=="7" ,"35000-45000",
                                                                                        ifelse(NHANES_Variables_19992020$indfminc=="8","45000-55000",
                                                                                               ifelse(NHANES_Variables_19992020$indfminc=="9","55000-65000",
                                                                                                      ifelse(NHANES_Variables_19992020$indfminc=="10","65000-75000",">100000"))))))))))
NHANES_Variables_19992020$FamilyIncomeRecode<-as.factor(ifelse(is.na(NHANES_Variables_19992020$familyincomeannual19992006),NHANES_Variables_19992020$familyincome20072020,NHANES_Variables_19992020$familyincomeannual19992006))
summary(NHANES_Variables_19992020$FamilyIncomeRecode)

# Smoker
NHANES_Variables_19992020$CurrentFormerORNeverSmoker<-as.factor(ifelse(NHANES_Variables_19992020$smq020=="1","Current or Former Smoker",
                                                                       ifelse(NHANES_Variables_19992020$smq020=="2","Never Smoker",NA)))

# Alcohol
NHANES_Variables_19992020$Alcohol_HeavyDrinking<-as.factor(ifelse(is.na(NHANES_Variables_19992020$alq150),NHANES_Variables_19992020$alq151,NHANES_Variables_19992020$alq150))
summary(factor(NHANES_Variables_19992020$Alcohol_HeavyDrinking))
NHANES_Variables_19992020$Alcohol_AnyDrinking<-as.factor(ifelse(is.na(NHANES_Variables_19992020$alq100),NHANES_Variables_19992020$alq101,NHANES_Variables_19992020$alq100))
summary(factor(NHANES_Variables_19992020$Alcohol_AnyDrinking))


NHANES_Variables_19992020$Alcohol_Any_Categories<-as.factor(ifelse(
  NHANES_Variables_19992020$Alcohol_AnyDrinking=="1" & 
    NHANES_Variables_19992020$Alcohol_HeavyDrinking=="1","Heavy Drinker", 
  ifelse(NHANES_Variables_19992020$Alcohol_AnyDrinking=="1" & 
           NHANES_Variables_19992020$Alcohol_HeavyDrinking=="2","Regular Drinker", "Not a Drinker")))
summary(NHANES_Variables_19992020$Alcohol_Any_Categories)

# Diabetes
Final_subset_Biomarker_All_Years$Diabetes_Based_on_fastingGlucose<-as.factor(ifelse(Final_subset_Biomarker_All_Years$lbdsglsi>7,"Yes","No"))
summary(NHANES_Variables_19992020$Told_Had_Diabetes)

# HTN
NHANES_Variables_19992020$HTN_Based_on_Systolic<-as.factor(ifelse(NHANES_Variables_19992020$bpxsy1>140,"1","0"))
summary(NHANES_Variables_19992020$HTN_Based_on_Systolic)

NHANES_Variables_19992020$HTN_Based_on_Diastolic<-as.factor(ifelse(NHANES_Variables_19992020$bpxdi1>90,"1","0"))
summary(NHANES_Variables_19992020$HTN_Based_on_Diastolic)

NHANES_Variables_19992020$HTNBasedonSyst_or_Dias<-as.factor(ifelse(NHANES_Variables_19992020$HTN_Based_on_Systolic=="1"|
                                                                     NHANES_Variables_19992020$HTN_Based_on_Diastolic=="1","1","0"))

NHANES_Variables_19992020$Told_Had_Hypertension<-as.factor(ifelse(NHANES_Variables_19992020$bpq020=="1","1","0"))

NHANES_Variables_19992020$Hypertension<-as.factor(ifelse(NHANES_Variables_19992020$HTNBasedonSyst_or_Dias=="1"|
                                                           NHANES_Variables_19992020$Told_Had_Hypertension=="1","1","0"))

# Dyslipidemia 
NHANES_Variables_19992020$Dyslipidemia<-as.factor(ifelse(NHANES_Variables_19992020$lbdtcsi>6,"1","0"))


# CKD
NHANES_Variables_19992020$Kidney_Dissease<-as.factor(ifelse(NHANES_Variables_19992020$kiq020=="1"|
                                                              NHANES_Variables_19992020$kiq022=="1","1","0"))
summary(NHANES_Variables_19992020$Kidney_Dissease)
levels(NHANES_Variables_19992020$Kidney_Dissease) <- c(levels(NHANES_Variables_19992020$Kidney_Dissease), "0")
NHANES_Variables_19992020$Kidney_Dissease[is.na(NHANES_Variables_19992020$Kidney_Dissease)] <- "0"

# HeartDisease
NHANES_Variables_19992020$HeartDisease<-as.factor(ifelse(NHANES_Variables_19992020$mcq160b=="1" | NHANES_Variables_19992020$mcq160c=="1" | NHANES_Variables_19992020$mcq160f=="1","1","0"))

# Cancer
summary(factor(NHANES_Variables_19992020$mcq220))

# Pulmonary
NHANES_Variables_19992020$Any_Pulmonary_Disease<-as.factor(ifelse(NHANES_Variables_19992020$mcq010=="1"|
                                                                                      NHANES_Variables_19992020$mcq160g=="1"|
                                                                                      NHANES_Variables_19992020$mcq160k=="1","1","0"))

# Vigorous Activity
NHANES_Variables_19992020$VIGOROUS_ACTIVITY<-as.factor(ifelse(NHANES_Variables_19992020$pad200=="1"|
                                                                NHANES_Variables_19992020$pad320=="1"|
                                                                NHANES_Variables_19992020$paq650=="1"|
                                                                NHANES_Variables_19992020$paq665=="1","1","0"))
summary(NHANES_Variables_19992020$VIGOROUS_ACTIVITY)
levels(NHANES_Variables_19992020$VIGOROUS_ACTIVITY) <- c(levels(NHANES_Variables_19992020$VIGOROUS_ACTIVITY), "0")
NHANES_Variables_19992020$VIGOROUS_ACTIVITY[is.na(NHANES_Variables_19992020$VIGOROUS_ACTIVITY)] <- "0"


# Now Combining Both the Biomarker File and the Variable file
# into one single File

Final_NHANES_19992020<-merge(NHANES_Variables_19992020,
                             Final_subset_Biomarker_All_Years,
                             by="seqn")

################################################################################
# General Information #
################################################################################

# This is an RScript for showing how to use survey weights with NHANES data. You
# can find a narrative to this script at:
# https://stylizeddata.com/how-to-use-survey-weights-in-r/

################################################################################
# Packages #
################################################################################

# For reading SAS XPT file from NHANES website
# haven::read_xpt

library(haven)

# For using survey weights
# survey::svydesign, svymean, svyglm

library(survey)

# For data wrangling
#dplyr::select, mutate, select, recode

library(dplyr)


################################################################################
# Survey Weights #
################################################################################

# Here we use "svydesign" to assign the weights. We will use this new design
# variable "nhanesDesign" when running our analyses.
library(survey)
nhanesDesign <- svydesign(id      = ~sdmvpsu,
                          strata  = ~sdmvstra,
                          weights = ~wtint2yr,
                          nest    = TRUE,
                          data    = Final_NHANES_19992020)

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
phenotypicage_agegroups <- svyby(~Phenotypic_Age, ~factor(age_deciles), design = nhanesDesign, svymean)
phenotypicage_agegroups_sd <- svyby(~Phenotypic_Age, ~factor(age_deciles), design = nhanesDesign, svyvar)
phenotypicage_agegroups_range <- svyby(~Phenotypic_Age, ~age_deciles, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_agegroups_summary <- data.frame(mean = round(phenotypicage_agegroups, 2),
                                              sd = round(sqrt(phenotypicage_agegroups_sd), 2),
                                              range = round(phenotypicage_agegroups_range, 2))
phenotypicage_agegroups_summarysd<-data.frame(phenotypicage_agegroups_sd)
phenotypicage_agegroups_summarysd$sd <- sqrt(phenotypicage_agegroups_summarysd$se)

# Rename the columns
names(phenotypicage_agegroups_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_agegroups_summary)

library(survey)
# Calculate the mean and standard deviation of Difference_Age_Phenotypic_Age by age_deciles
Difference_Age_Phenotypic_Age_agegroups <- svyby(~Difference_Age_Phenotypic_Age, ~factor(age_deciles), design = nhanesDesign, svymean)
Difference_Age_Phenotypic_Age_agegroups_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(age_deciles), design = nhanesDesign, svyvar)
Difference_Age_Phenotypic_Age_agegroups_range <- svyby(~Difference_Age_Phenotypic_Age, ~age_deciles, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_Age_Phenotypic_Age_agegroups_summary <- data.frame(Difference_Age_Phenotypic_Age_agegroups)
Difference_Age_Phenotypic_Age_agegroups_summarysd<-data.frame(Difference_Age_Phenotypic_Age_agegroups_sd)
Difference_Age_Phenotypic_Age_agegroups_summarysd$sd <- sqrt(Difference_Age_Phenotypic_Age_agegroups_summarysd$se)

# Rename the columns
names(Difference_Age_Phenotypic_Age_agegroups_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(Difference_Age_Phenotypic_Age_agegroups_summary)


# Weighted Frequency table for Age Groups 

svytable(~AgeGroups, nhanesDesign)

# 19-39	320782414	38.6%
# 40-59	277403126	37.68%
# 60+	291228458	23.7%

# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
phenotypicage_agegroups2 <- svyby(~Phenotypic_Age, ~factor(AgeGroups), design = nhanesDesign, svymean)
phenotypicage_agegroups2_sd <- svyby(~Phenotypic_Age, ~factor(AgeGroups), design = nhanesDesign, svyvar)
phenotypicage_agegroups2_range <- svyby(~Phenotypic_Age, ~AgeGroups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_agegroups_summary2 <- data.frame(phenotypicage_agegroups2)
phenotypicage_agegroups_summarysd2<-data.frame(phenotypicage_agegroups2_sd)
phenotypicage_agegroups_summarysd2$sd <- sqrt(phenotypicage_agegroups_summarysd2$se)

# Rename the columns
names(phenotypicage_agegroups_summary2) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_agegroups_summary2)

# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Difference_Age_Phenotypic_Age_agegroups2 <- svyby(~Difference_Age_Phenotypic_Age, ~factor(AgeGroups), design = nhanesDesign, svymean)
Difference_Age_Phenotypic_Age_agegroups2_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(AgeGroups), design = nhanesDesign, svyvar)
Difference_Age_Phenotypic_Age_agegroups2_range <- svyby(~Difference_Age_Phenotypic_Age, ~AgeGroups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_Age_Phenotypic_Age_agegroups_summary2 <- data.frame(Difference_Age_Phenotypic_Age_agegroups2)
Difference_Age_Phenotypic_Age_agegroups_summarysd2<-data.frame(Difference_Age_Phenotypic_Age_agegroups2_sd)
Difference_Age_Phenotypic_Age_agegroups_summarysd2$sd <- sqrt(Difference_Age_Phenotypic_Age_agegroups_summarysd2$se)

# Rename the columns
names(Difference_Age_Phenotypic_Age_agegroups_summary2) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(Difference_Age_Phenotypic_Age_agegroups_summary2)


# Weighted Frequency table for Race
svytable(~RACE_RECODE, nhanesDesign)

#Mexican American 	122268531	8.30%
#Non-Hispanic Black 	158420633	10.76%
#Non-Hispanic White 	1012045779	68.74%
#Other Hispanic 	80102460	5.44%
#Other Race - Including Multi-Racial 	99405741	6.75%

# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
phenotypicage_race <- svyby(~Phenotypic_Age, ~factor(RACE_RECODE), design = nhanesDesign, svymean)
phenotypicage_race_sd <- svyby(~Phenotypic_Age, ~factor(RACE_RECODE), design = nhanesDesign, svyvar)
phenotypicage_race_range <- svyby(~Phenotypic_Age, ~RACE_RECODE, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_race_summary <- data.frame(phenotypicage_race)
phenotypicage_race_summarysd<-data.frame(phenotypicage_race_sd)
phenotypicage_race_summarysd$sd <- sqrt(phenotypicage_race_summarysd$se)

# Rename the columns
names(phenotypicage_race_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_race_summary)

# Calculate the mean and standard deviation of Difference_Age_Phenotypic_Age by age_deciles
Difference_Age_Phenotypic_Age_race <- svyby(~Difference_Age_Phenotypic_Age, ~factor(RACE_RECODE), design = nhanesDesign, svymean)
Difference_Age_Phenotypic_Age_race_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(RACE_RECODE), design = nhanesDesign, svyvar)
Difference_Age_Phenotypic_Age_race_range <- svyby(~Difference_Age_Phenotypic_Age, ~RACE_RECODE, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_Age_Phenotypic_Age_race_summary <- data.frame(Difference_Age_Phenotypic_Age_race)
Difference_Age_Phenotypic_Age_race_summarysd<-data.frame(Difference_Age_Phenotypic_Age_race_sd)
Difference_Age_Phenotypic_Age_race_summarysd$sd <- sqrt(Difference_Age_Phenotypic_Age_race_summarysd$se)


# Weighted Frequency table for Sex

svytable(~riagendr, nhanesDesign)
# Male 711157980 48.3%
# Female 761085164 51.7%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
phenotypicage_sex <- svyby(~Phenotypic_Age, ~factor(riagendr), design = nhanesDesign, svymean)
phenotypicage_sex_sd <- svyby(~Phenotypic_Age, ~factor(riagendr), design = nhanesDesign, svyvar)
phenotypicage_sex_range <- svyby(~Phenotypic_Age, ~riagendr, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_sex_summary <- data.frame(phenotypicage_sex)
phenotypicage_sex_summarysd<-data.frame(phenotypicage_sex_sd)
phenotypicage_sex_summarysd$sd <- sqrt(phenotypicage_sex_summarysd$se)

# Rename the columns
names(phenotypicage_sex_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_sex_summary)

# Calculate the mean and standard deviation of Difference_Age_Phenotypic_Age by age_deciles
Difference_Age_Phenotypic_Age_sex <- svyby(~Difference_Age_Phenotypic_Age, ~factor(riagendr), design = nhanesDesign, svymean)
Difference_Age_Phenotypic_Age_sex_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(riagendr), design = nhanesDesign, svyvar)
Difference_Age_Phenotypic_Age_sex_range <- svyby(~Difference_Age_Phenotypic_Age, ~riagendr, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_Age_Phenotypic_Age_sex_summary <- data.frame(Difference_Age_Phenotypic_Age_sex)
Difference_Age_Phenotypic_Age_sex_summarysd<-data.frame(Difference_Age_Phenotypic_Age_sex_sd)
Difference_Age_Phenotypic_Age_sex_summarysd$sd <- sqrt(Difference_Age_Phenotypic_Age_sex_summarysd$se)



# Weighted Frequency table for BMI groups
svytable(~BMI_Groups, nhanesDesign)
#<20	73972563	5.13%
#20-25	384449356	26.68%
#25-30	476812880	33.08%
#30-35	287313865	19.94%
#35-40	128785256	8.94%
#40 or higher	89820361	6.23%

# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
phenotypicage_BMI_Groups <- svyby(~Phenotypic_Age, ~factor(BMI_Groups), design = nhanesDesign, svymean)
phenotypicage_BMI_Groups_sd <- svyby(~Phenotypic_Age, ~factor(BMI_Groups), design = nhanesDesign, svyvar)
phenotypicage_BMI_Groups_range <- svyby(~Phenotypic_Age, ~BMI_Groups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_BMI_Groups_summary <- data.frame(phenotypicage_BMI_Groups)
phenotypicage_BMI_Groups_summarysd<-data.frame(phenotypicage_BMI_Groups_sd)
phenotypicage_BMI_Groups_summarysd$sd <- sqrt(phenotypicage_BMI_Groups_summarysd$se)

# Rename the columns
names(phenotypicage_BMI_Groups_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_BMI_Groups_summary)

# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Difference_Age_Phenotypic_Age_BMI_Groups <- svyby(~Difference_Age_Phenotypic_Age, ~factor(BMI_Groups), design = nhanesDesign, svymean)
Difference_Age_Phenotypic_Age_BMI_Groups_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(BMI_Groups), design = nhanesDesign, svyvar)
Difference_Age_Phenotypic_Age_BMI_Groups_range <- svyby(~Difference_Age_Phenotypic_Age, ~BMI_Groups, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_Age_Phenotypic_Age_BMI_Groups_summary <- data.frame(Difference_Age_Phenotypic_Age_BMI_Groups)
Difference_Age_Phenotypic_Age_BMI_Groups_summarysd<-data.frame(Difference_Age_Phenotypic_Age_BMI_Groups_sd)
Difference_Age_Phenotypic_Age_BMI_Groups_summarysd$sd <- sqrt(Difference_Age_Phenotypic_Age_BMI_Groups_summarysd$se)


# Weighted Frequency table for Education
svytable(~dmdeduc2, nhanesDesign)
# Less than 9th grade (N=4651)	87898201.3	6.066%
# 9-11th grade (Includes 12th grade with no diploma) (N=5402)	159491596.8	11.006%
# High school graduate/GED or equivalent (N=8537)	358733876	24.756%
# Some college or AA degree (N=10183)	444474864	30.673%
# College graduate or above (N=7559)	396998727.5	27.396%
# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
phenotypicage_Education <- svyby(~Phenotypic_Age, ~factor(dmdeduc2), design = nhanesDesign, svymean)
phenotypicage_Education_sd <- svyby(~Phenotypic_Age, ~factor(dmdeduc2), design = nhanesDesign, svyvar)
phenotypicage_Education_range <- svyby(~Phenotypic_Age, ~dmdeduc2, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_Education_summary <- data.frame(phenotypicage_Education)
phenotypicage_Education_summarysd<-data.frame(phenotypicage_Education_sd)
phenotypicage_Education_summarysd$sd <- sqrt(phenotypicage_Education_summarysd$se)

# Rename the columns
names(phenotypicage_Education_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_Education_summary)


# Calculate the mean and standard deviation of Phenotypic_Age by age_deciles
Difference_Age_Phenotypic_Age_BMI_Groups <- svyby(~Difference_Age_Phenotypic_Age, ~factor(dmdeduc2), design = nhanesDesign, svymean)
Difference_Age_Phenotypic_Age_BMI_Groups_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(dmdeduc2), design = nhanesDesign, svyvar)
Difference_Age_Phenotypic_Age_BMI_Groups_range <- svyby(~Difference_Age_Phenotypic_Age, ~dmdeduc2, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_Age_Phenotypic_Age_BMI_Groups_summary <- data.frame(Difference_Age_Phenotypic_Age_BMI_Groups)
Difference_Age_Phenotypic_Age_BMI_Groups_summarysd<-data.frame(Difference_Age_Phenotypic_Age_BMI_Groups_sd)
Difference_Age_Phenotypic_Age_BMI_Groups_summarysd$sd <- sqrt(Difference_Age_Phenotypic_Age_BMI_Groups_summarysd$se)



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
phenotypicage_familyincomeannual <- svyby(~Phenotypic_Age, ~factor(familyincomeannual), design = nhanesDesign, svymean)
phenotypicage_familyincomeannual_sd <- svyby(~Phenotypic_Age, ~factor(familyincomeannual), design = nhanesDesign, svyvar)
phenotypicage_familyincomeannual_range <- svyby(~Phenotypic_Age, ~familyincomeannual, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_familyincomeannual_summary <- data.frame(phenotypicage_familyincomeannual)
phenotypicage_familyincomeannual_summarysd<-data.frame(phenotypicage_familyincomeannual_sd)
phenotypicage_familyincomeannual_summarysd$sd <- sqrt(phenotypicage_familyincomeannual_summarysd$se)

# Rename the columns
names(phenotypicage_familyincomeannual_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_familyincomeannual_summary)

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
phenotypicage_Alcohol <- svyby(~Phenotypic_Age, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svymean)
phenotypicage_Alcohol_sd <- svyby(~Phenotypic_Age, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svyvar)
phenotypicage_Alcohol_range <- svyby(~Phenotypic_Age, ~Alcohol_Any_Categories, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_Alcohol_summary <- data.frame(phenotypicage_Alcohol)
phenotypicage_Alcohol_summarysd<-data.frame(phenotypicage_Alcohol_sd)
phenotypicage_Alcohol_summarysd$sd <- sqrt(phenotypicage_Alcohol_summarysd$se)

# Rename the columns
names(phenotypicage_Alcohol_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_Alcohol_summary)


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
phenotypicage_SmokingStatus <- svyby(~Phenotypic_Age, ~factor(SmokingStatus), design = nhanesDesign, svymean)
phenotypicage_SmokingStatus_sd <- svyby(~Phenotypic_Age, ~factor(SmokingStatus), design = nhanesDesign, svyvar)
phenotypicage_SmokingStatus_range <- svyby(~Phenotypic_Age, ~SmokingStatus, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_SmokingStatus_summary <- data.frame(phenotypicage_SmokingStatus)
phenotypicage_SmokingStatus_summarysd<-data.frame(phenotypicage_SmokingStatus_sd)
phenotypicage_SmokingStatus_summarysd$sd <- sqrt(phenotypicage_SmokingStatus_summarysd$se)

# Rename the columns
names(phenotypicage_SmokingStatus_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_SmokingStatus_summary)


# Weighted Frequency table for Diabetes
table_weighted_Diabetes<-svytable(~Told_Had_Diabetes, nhanesDesign)
tabledataframe_Diabetes<-data.frame(table_weighted_Diabetes)
total_freq <- sum(tabledataframe_Diabetes$Freq)
tabledataframe_Diabetes$Percentage <- (tabledataframe_Diabetes$Freq/total_freq)*100
tabledataframe_Diabetes$Percentage <- round(tabledataframe_Diabetes$Percentage, 2) 
tabledataframe_Diabetes

#Told_Had_Diabetes       Freq Percentage
#                No 1322298535      91.39%
#               Yes  124521799       8.61%
phenotypicage_Diabetes <- svyby(~Phenotypic_Age, ~factor(Told_Had_Diabetes), design = nhanesDesign, svymean)
phenotypicage_Diabetes_sd <- svyby(~Phenotypic_Age, ~factor(Told_Had_Diabetes), design = nhanesDesign, svyvar)
phenotypicage_Diabetes_range <- svyby(~Phenotypic_Age, ~Told_Had_Diabetes, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_Diabetes_summary <- data.frame(phenotypicage_Diabetes)
phenotypicage_Diabetes_summarysd<-data.frame(phenotypicage_Diabetes_sd)
phenotypicage_Diabetes_summarysd$sd <- sqrt(phenotypicage_Diabetes_summarysd$se)

# Rename the columns
names(phenotypicage_Diabetes_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_Diabetes_summary)


# Weighted Frequency table for Hypertension
table_weighted_Hypertension<-svytable(~HTNBasedonSyst_or_Dias, nhanesDesign)
tabledataframe_Hypertension<-data.frame(table_weighted_Hypertension)
total_freq <- sum(tabledataframe_Hypertension$Freq)
tabledataframe_Hypertension$Percentage <- (tabledataframe_Hypertension$Freq/total_freq)*100
tabledataframe_Hypertension$Percentage <- round(tabledataframe_Hypertension$Percentage, 2) 
tabledataframe_Hypertension
#HTNBasedonSyst_or_Dias       Freq Percentage
#1                      0 1125223737      84.25
#2                      1  210355103      15.75

phenotypicage_Hypertension <- svyby(~Phenotypic_Age, ~factor(HTNBasedonSyst_or_Dias), design = nhanesDesign, svymean)
phenotypicage_Hypertension_sd <- svyby(~Phenotypic_Age, ~factor(HTNBasedonSyst_or_Dias), design = nhanesDesign, svyvar)
phenotypicage_Hypertension_range <- svyby(~Phenotypic_Age, ~HTNBasedonSyst_or_Dias, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_Hypertension_summary <- data.frame(phenotypicage_Hypertension)
phenotypicage_Hypertension_summarysd<-data.frame(phenotypicage_Hypertension_sd)
phenotypicage_Hypertension_summarysd$sd <- sqrt(phenotypicage_Hypertension_summarysd$se)

# Rename the columns
names(phenotypicage_Hypertension_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_Hypertension_summary)


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

phenotypicage_Dyslipidemia <- svyby(~Phenotypic_Age, ~factor(Dyslipidemia), design = nhanesDesign, svymean)
phenotypicage_Dyslipidemia_sd <- svyby(~Phenotypic_Age, ~factor(Dyslipidemia), design = nhanesDesign, svyvar)
phenotypicage_Dyslipidemia_range <- svyby(~Phenotypic_Age, ~Dyslipidemia, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_Dyslipidemia_summary <- data.frame(phenotypicage_Dyslipidemia)
phenotypicage_Dyslipidemia_summarysd<-data.frame(phenotypicage_Dyslipidemia_sd)
phenotypicage_Dyslipidemia_summarysd$sd <- sqrt(phenotypicage_Dyslipidemia_summarysd$se)


# Rename the columns
names(phenotypicage_Dyslipidemia_summary) <- c("age_deciles", "mean", "se", "sd")



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

phenotypicage_HeartDisease <- svyby(~Phenotypic_Age, ~factor(HeartDisease), design = nhanesDesign, svymean)
phenotypicage_HeartDisease_sd <- svyby(~Phenotypic_Age, ~factor(HeartDisease), design = nhanesDesign, svyvar)
phenotypicage_HeartDisease_range <- svyby(~Phenotypic_Age, ~HeartDisease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_HeartDisease_summary <- data.frame(phenotypicage_HeartDisease)
phenotypicage_HeartDisease_summarysd<-data.frame(phenotypicage_HeartDisease_sd)
phenotypicage_HeartDisease_summarysd$sd <- sqrt(phenotypicage_HeartDisease_summarysd$se)

# Rename the columns
names(phenotypicage_HeartDisease_summary) <- c("age_deciles", "mean", "se", "sd")



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

phenotypicage_Cancer <- svyby(~Phenotypic_Age, ~factor(mcq220), design = nhanesDesign, svymean)
phenotypicage_Cancer_sd <- svyby(~Phenotypic_Age, ~factor(mcq220), design = nhanesDesign, svyvar)
phenotypicage_Cancer_range <- svyby(~Phenotypic_Age, ~mcq220, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_Cancer_summary <- data.frame(phenotypicage_Cancer)
phenotypicage_Cancer_summarysd<-data.frame(phenotypicage_Cancer_sd)
phenotypicage_Cancer_summarysd$sd <- sqrt(phenotypicage_Cancer_summarysd$se)



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

phenotypicage_Any_Pulmonary_Disease <- svyby(~Phenotypic_Age, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svymean)
phenotypicage_Any_Pulmonary_Disease_sd <- svyby(~Phenotypic_Age, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svyvar)
phenotypicage_Any_Pulmonary_Disease_range <- svyby(~Phenotypic_Age, ~Any_Pulmonary_Disease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_Any_Pulmonary_Disease_summary <- data.frame(phenotypicage_Any_Pulmonary_Disease)
phenotypicage_Any_Pulmonary_Disease_summarysd<-data.frame(phenotypicage_Any_Pulmonary_Disease_sd)
phenotypicage_Any_Pulmonary_Disease_summarysd$sd <- sqrt(phenotypicage_Any_Pulmonary_Disease_summarysd$se)


# Rename the columns
names(phenotypicage_Any_Pulmonary_Disease_summary) <- c("age_deciles", "mean", "se", "sd")



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

phenotypicage_VIGOROUS_ACTIVITY <- svyby(~Phenotypic_Age, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svymean)
phenotypicage_VIGOROUS_ACTIVITY_sd <- svyby(~Phenotypic_Age, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svyvar)
phenotypicage_VIGOROUS_ACTIVITY_range <- svyby(~Phenotypic_Age, ~VIGOROUS_ACTIVITY, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_VIGOROUS_ACTIVITY_summary <- data.frame(phenotypicage_VIGOROUS_ACTIVITY)
phenotypicage_VIGOROUS_ACTIVITY_summarysd<-data.frame(phenotypicage_VIGOROUS_ACTIVITY_sd)
phenotypicage_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(phenotypicage_VIGOROUS_ACTIVITY_summarysd$se)
