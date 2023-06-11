library(readr)
file.choose()

setwd("/Users/mohammedalialvi/nhanes/data/sources/csv/2001-2002")

# Get a list of all CSV files in the folder
file_list <- list.files(pattern = "*.csv")

# Create an empty list to store the data frames
df_list <- list()
if (!all("seqn" %in% names(df_list))) {
  missing_cols <- names(df_list)[!("seqn" %in% names(df_list))]
  cat(paste0("Warning: missing 'seqn' column in ", paste(missing_cols, collapse = ", "), "\n"))
}

for (i in 1:length(file_list)) {
  file <- file_list[i]
  cat("Reading file", file, "\n")
  df <- tryCatch(read.csv(file), error = function(e) NULL)
  if (!is.null(df)) {
    df_list[[i]] <- df
  }
}
# check which data frames in the list have the "seqn" column
map_lgl(df_list, ~ "seqn" %in% names(.x))
names(df_list)
false_indices <- which(map_lgl(df_list, ~ "seqn" %in% names(.x)) == FALSE)
false_df_names <- names(df_list)[false_indices]
false_df_names
# output should show which data frame is missing the "seqn" column

df_names <- map_chr(df_list, ~ deparse(substitute(.x)))
df_names
# Merge all the data frames in the list into a single data frame
library(dplyr)
library(purrr)
merged_df <- reduce(df_list, full_join, by = "seqn")

#Deleted the 4 files without "seqn" variable in 2001-2002 NHANES manually

# identify columns with ".x" or ".y" suffix
dup_cols <- grep("\\.(x|y)$", names(merged_df), value = TRUE)
# remove ".x" or ".y" suffix from column names and identify unique names
unique_cols <- unique(gsub("\\.(x|y)$", "", dup_cols))
# loop through unique column names and keep only one column with that name
for (col_name in unique_cols) {
  # identify columns with this name and ".x" or ".y" suffix
  col_suffixes <- c(".x", ".y")
  col_names <- paste(col_name, col_suffixes, sep = "")
  dup_cols <- intersect(names(merged_df), col_names)
  
  # keep the first column and remove the others
  keep_col <- dup_cols[1]
  remove_cols <- dup_cols[-1]
  merged_df[[keep_col]] <- coalesce(merged_df[[keep_col]], merged_df[[remove_cols[1]]])
  merged_df <- merged_df[, !names(merged_df) %in% remove_cols]
}

# verify the resulting data frame
dim(merged_df)

library(dplyr)

merged_df2 <- distinct(merged_df, seqn, .keep_all = TRUE)
rm(merged_df)
write.csv(merged_df2,"NHANES_COMPLETE_2001_2002.csv")

install.packages("RNHANES")
library(RNHANES)
nhanes_load(year = "1999-2000")
download_nhanes_file(file_name, year, destination = tempdir(), cache = TRUE)




lab_files <- nhanes_data_files(components = "laboratory", year = "1999-2000")
lab_files

lab_files <- nhanes_data_files(components = "Demographics")
lab_files_1999_2000 <- lab_files[grep("1999-2000", lab_files$file_name), ]
nhanes_data_files(components = "all", destination = tempfile(),
                  cache = TRUE)

nhanes_load_data(file_name, year, destination = tempdir(),
                 demographics = FALSE, cache = TRUE, recode = FALSE,
                 recode_data = FALSE, recode_demographics = FALSE,
                 allow_duplicate_files = FALSE)
demgraphy_data_19992000<-nhanes_load_demography_data("1999-2000")

library(RNHANES)
nhanes19992000<-nhanes_load_data(file_name = "*",year = "2001-2002", destination = "/Users/mohammedalialvi/", recode = TRUE,
                                 cache=FALSE,demographics = TRUE)
library(haven)
library(foreign)


nhanes_demography_allyears<-nhanes_load_demography_data(c("1999-2000", "2011-2012"))

library(rvest)

# Scrape the webpage and extract file names
page <- readLines("https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&CycleBeginYear=1999")
file_names <- grep(".XPT", page, value = TRUE)
examination_files <- grep("LAB", file_names, value = TRUE, invert = TRUE)
examination_files <- grep("XT", examination_files, value = TRUE, invert = TRUE)

# Update URL for data files to the new link
examination_files <- gsub("/Nchs/Nhanes/", "https://wwwn.cdc.gov/Nchs/Nhanes/", examination_files)

# Load the data into R using nhanes_load_data function
exam_data <- nhanes_load_data(file_name = examination_files, year = "1999-2000", destination = tempdir())

nhanes_demography19992000<-nhanes_load_demography_data("1999-2000")

library(stringr)

# Set the URL for the Examination component of NHANES 1999-2000
url <- "https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&CycleBeginYear=1999"

# Scrape the webpage for all file links ending in .XPT
file_links <- url %>%
  read_html() %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset(".XPT$")

# Extract only the file names from the links
file_names <- file_links %>%
  str_extract("(?<=/)[^/]*$")

# Print the vector of file names
print(file_names)

library(readr)
library(RNHANES)
nhanes_demo_19992000<-nhanes_load_data("DEMO.XPT", "1999-2000",
                 cache = TRUE, recode = TRUE,demographics=TRUE,
                 recode_data = TRUE, recode_demographics = TRUE,
                 allow_duplicate_files = TRUE)

# Loop through each file and store as a separate dataframe
for (i in 1:length(nhanes_exam_19992000)) {
  # Get the file name without the extension
  df_name <- sub("\\.XPT$", "", names(nhanes_exam_19992000)[i])
  # Assign the data to a dataframe with the same name
  assign(df_name, nhanes_exam_19992000[[i]])
}

# Create a base data frame from the first data set
base_df <- nhanes_exam_19992000[[1]]

# Loop through the remaining data frames in the list and full join them onto the base
for (i in 2:length(nhanes_exam_19992000)) {
  base_df <- merge(base_df, nhanes_exam_19992000[[i]], by = "SEQN", all = TRUE)
}


# Set the URL for the Laboratory component of NHANES 1999-2000
url <- "https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Laboratory&CycleBeginYear=1999"

# Scrape the webpage for all file links ending in .XPT
file_links <- url %>%
  read_html() %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset(".XPT$")

# Extract only the file names from the links
file_names <- file_links %>%
  str_extract("(?<=/)[^/]*$")

# Print the vector of file names
print(file_names)

library(readr)

nhanes_labs_19992000<-nhanes_load_data(c("SSAFB_A.XPT", "LAB16.XPT", "SSAMH_A.XPT", "SSDFS_A.XPT", "SSANA_A.XPT", "SSANA2_A.XPT",
                                         "LAB06.XPT", "LAB05.XPT", "LAB13AM.XPT", "LAB13.XPT", "LAB25.XPT", "LAB11.XPT", "LAB17.XPT",
                                         "SSCYST_A.XPT", "SSCMV_A.XPT", "SSCMVG_A.XPT", "SSUCSH_A.XPT", "LAB28POC.XPT", "PH.XPT",
                                         "SSFOL_A.XPT",  "LAB10.XPT", "L02HPA_A.XPT", "L02HBS.XPT", "LAB02.XPT",
                                         "LAB09.XPT", "SSHSV1_A.XPT", "SSTROP_A.XPT", "LAB03.XPT", "LAB07.XPT", "LAB20.XPT",
                                         "LAB19.XPT", "LAB22.XPT", "LAB06HM.XPT", "SSOL_A.XPT", "SSMUMP_A.XPT", "SSNORO_A.XPT",
                                         "SSBNP_A.XPT", "SSPFC_A.XPT", "LAB26PP.XPT", "PHPYPA.XPT", "LAB10AM.XPT", "UC.XPT",
                                         "SSCHL_A.XPT", "LAB18.XPT", "TELO_A.XPT", "LAB18T4.XPT", "TFA_A.XPT", "SSTFR_A.XPT",
                                         "SSVARI_A.XPT", "LAB04.XPT", "LAB21.XPT","SSCARD_A.XPT")
                                       , "1999-2000",
                                       cache = TRUE, recode = TRUE,demographics=FALSE,
                                       recode_data = TRUE, recode_demographics = FALSE,
                                       allow_duplicate_files = TRUE)

# Loop through each file and store as a separate dataframe
for (i in 1:length(nhanes_labs_19992000)) {
  # Get the file name without the extension
  df_name_lab_19992000 <- sub("\\.XPT$", "", names(nhanes_labs_19992000)[i])
  # Assign the data to a dataframe with the same name
  assign(df_name_lab_19992000, nhanes_labs_19992000[[i]])
}

# Create a base data frame from the first data set
base_df_lab_19992000 <- nhanes_labs_19992000[[1]]

# Loop through the remaining data frames in the list and full join them onto the base
for (i in 2:length(nhanes_labs_19992000)) {
  base_df_lab_19992000 <- merge(base_df_lab_19992000, nhanes_labs_19992000[[i]], by = "SEQN", all = TRUE)
}

# The file SSCARD_A.xpt (which contains the albumin levels) was not 
## added in the file above because of an error so now loading it separately
SSCARD_A<-read_xpt("SSCARD_A_19992000.xpt")


# Now merging the file generated above with the
## SSCARD_A.xpt file which contains 
### Albumin lycated albumin, Beta-2 Microglobulin, Cystatin C (Surplus)


Final_LAB_19992000<-merge(base_df_lab_19992000,SSCARD_A,
                          by="SEQN",all.x=TRUE)

# The variable for albumin level in g/dl is LBDSALSI
summary(Final_LAB_19992000$LBDSALSI)

# The variable for creatinine in umol/L is LBDSCRSI
summary(Final_LAB_19992000$LBDSCRSI)

# The variable for serum glucose in mmol/L is LB2SGLSI
## this variable is not present in the 1999-2000 
### using a different variable for glucose (plasma glucose) SI (mmol/L) "LBXGLUSI"
summary(Final_LAB_19992000$LBXGLUSI)

# The variable for c-reactive protein (mg/dl) is LBXCRP
summary(Final_LAB_19992000$LBXCRP)

# The variable for lymphocyte% is LBXLYPCT
summary(Final_LAB_19992000$LBXLYPCT)

# The variable for mean cell volume (fl) is LBXMCVSI
summary(Final_LAB_19992000$LBXMCVSI)

# The variable for Red Cell Distribution Width (%) is LBXRDW
summary(Final_LAB_19992000$LBXRDW)

# The variable for Alkaline Phosphatase (U/L) is LBXSAPSI
summary(Final_LAB_19992000$LBXSAPSI)

# The variable for White blood cell count (1000 cells/uL) is LB2WBCSI
## This variable is not present in the 1999-2000 dataset, 
### so using a different variable (LBXWBCSI which is White blood cell count: SI)
summary(Final_LAB_19992000$LBXWBCSI)

# The variable for age in the demographic file is RIDAGEYR
summary(NHANES_19992000_Demographic$RIDAGEYR)

# Subsetting a dataset for demographic file with only SEQN and AGE
Subset_Demo_Age<-DEMO[, c("seqn","ridageyr")]

# Now subsetting the lab file including only the biomarkers
Subset_Lab_19992000<-Final_LAB_19992000[, c("LBDSALSI","LBDSCRSI","LBXGLUSI",
                                            "LBXCRP","LBXLYPCT","LBXMCVSI","LBXRDW",
                                            "LBXSAPSI","LBXWBCSI","SEQN")]
# Combining the two files now
Biomarker_NHANES_19992000<-merge(Subset_Demo_Age,NHANES_COMPLETE_1999_2000,by="seqn")
Biomarker_NHANES_19992000_CompleteCases<-complete.cases(Biomarker_NHANES_19992000)

Biomarker_NHANES_19992000[Biomarker_NHANES_19992000_CompleteCases,]
library(arsenal)
missing_values<-tableby(~RIDAGEYR+ LBDSALSI +LBDSCRSI +LBXGLUSI+ LBXCRP +  LBXLYPCT+ LBXMCVSI
                        +LBXRDW  + LBXSAPSI+ LBXWBCSI,data=Biomarker_NHANES_19992000)
write2word(missing_values,"missing_values.docx")

write.csv(Biomarker_NHANES_19992000,"Biomarker_NHANES_19992000.csv")

Biomarker_NHANES_20012002<-NHANES_COMPLETE_2001_2002[, c("seqn","lbdsalsi","lbdscrsi","lb2sglsi",
                                                         "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                         "lbdsapsi","lb2wbcsi","ridageyr")]
Biomarker_NHANES_20012002_CompleteCases<-complete.cases(Biomarker_NHANES_20012002)

Biomarker_NHANES_20012002[Biomarker_NHANES_20012002_CompleteCases,]











setwd("/Users/mohammedalialvi/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2017-2018")

# FILES 11-12 and 13-14 DO NOT HAVE CRP

# Get a list of all CSV files in the folder
file_list <- list.files(pattern = "*.csv")

# Create an empty list to store the data frames
df_list <- list()
if (!all("seqn" %in% names(df_list))) {
  missing_cols <- names(df_list)[!("seqn" %in% names(df_list))]
  cat(paste0("Warning: missing 'seqn' column in ", paste(missing_cols, collapse = ", "), "\n"))
}

for (i in 1:length(file_list)) {
  file <- file_list[i]
  cat("Reading file", file, "\n")
  df <- tryCatch(read.csv(file), error = function(e) NULL)
  if (!is.null(df)) {
    df_list[[i]] <- df
  }
}
library(dplyr)
library(purrr)
#DOXPOL_D,BFRPOL_D,PSTPOL_D,PBCD_D
# check which data frames in the list have the "seqn" column
map_lgl(df_list, ~ "seqn" %in% names(.x))
names(df_list)
false_indices <- which(map_lgl(df_list, ~ "seqn" %in% names(.x)) == FALSE)
false_df_names <- names(df_list)[false_indices]
false_df_names
# output should show which data frame is missing the "seqn" column

# Merge all the data frames in the list into a single data frame
merged_df <- reduce(df_list, full_join, by = "seqn")

#Deleted the 4 files without "seqn" variable in 2001-2002 NHANES manually


merged_df2 <- distinct(merged_df, seqn, .keep_all = TRUE)

Biomarker_NHANES_20032004<-merged_df2[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                                         "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                         "lbxsapsi","lbxwbcsi","ridageyr")]


merged_df3<-subset(merged_df2,!is.na(merged_df2$lbdsalsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbdscrsi))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbdsglsi))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxcrp))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxlypct))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxmcvsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxrdw))
merged_df10<-subset(merged_df9,!is.na(merged_df9$lbxsapsi))
merged_df11<-subset(merged_df10,!is.na(merged_df10$lbxwbcsi))
merged_df12<-subset(merged_df11,!is.na(merged_df11$ridageyr))

Biomarker_NHANES_200320042<-merged_df12[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                          "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                          "lbxsapsi","lbxwbcsi","ridageyr")]




Subset_Lab_19992000<-Final_LAB_19992000[, c("LBDSALSI","LBDSCRSI","LBXGLUSI",
                                            "LBXCRP","LBXLYPCT","LBXMCVSI","LBXRDW",
                                            "LBXSAPSI","LBXWBCSI","SEQN")]

merged_df3<-subset(merged_df2,!is.na(merged_df2$lbdsalsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbdscrsi))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbxglusi))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxcrp))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxlypct))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxmcvsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxrdw))
merged_df10<-subset(merged_df9,!is.na(merged_df9$lbxsapsi))
merged_df11<-subset(merged_df10,!is.na(merged_df10$lbxwbcsi))
merged_df12<-subset(merged_df11,!is.na(merged_df11$ridageyr.x))

Biomarker_NHANES_19992000<-merged_df12[, c("seqn","lbdsalsi","lbdscrsi","lbxglusi",
                                            "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                            "lbxsapsi","lbxwbcsi","ridageyr.x")]



Biomarker_NHANES_20012002<-NHANES_COMPLETE_2001_2002[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                                         "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                                         "lbdsapsi","lbxwbcsi","ridageyr")]
Biomarker_NHANES_20012002_CompleteCases<-complete.cases(Biomarker_NHANES_20012002)

Biomarker_NHANES_20012002[Biomarker_NHANES_20012002_CompleteCases,]

merged_df3<-subset(NHANES_COMPLETE_2001_2002,!is.na(NHANES_COMPLETE_2001_2002$lbdsalsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbdscrsi))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbdsglsi))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxcrp))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxlypct))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxmcvsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxrdw))
merged_df10<-subset(merged_df9,!is.na(merged_df9$lbdsapsi))
merged_df11<-subset(merged_df10,!is.na(merged_df10$lbxwbcsi))
merged_df12<-subset(merged_df11,!is.na(merged_df11$ridageyr))
Biomarker_NHANES_200120022<-merged_df12[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                            "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                            "lbdsapsi","lbxwbcsi","ridageyr")]
write.csv(Biomarker_NHANES_19992000,"Biomarker_NHANES_19992000_completecases.csv")
write.csv(Biomarker_NHANES_200120022,"Biomarker_NHANES_200120022_completecases.csv")
write.csv(Biomarker_NHANES_200320042,"Biomarker_NHANES_200320042_completecases.csv")

Biomarker_NHANES_20052006<-merged_df2[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                          "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                          "lbxsapsi","lbxwbcsi","ridageyr")]


merged_df3<-subset(Biomarker_NHANES_20052006,!is.na(Biomarker_NHANES_20052006$lbdsalsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbdscrsi))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbdsglsi))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxcrp))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxlypct))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxmcvsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxrdw))
merged_df10<-subset(merged_df9,!is.na(merged_df9$lbxsapsi))
merged_df11<-subset(merged_df10,!is.na(merged_df10$lbxwbcsi))
merged_df12<-subset(merged_df11,!is.na(merged_df11$ridageyr))
write.csv(merged_df12,"Biomarker_NHANES_20052006.csv")


Biomarker_NHANES_20072008<-merged_df2[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                          "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                          "lbxsapsi","lbxwbcsi","ridageyr")]


merged_df3<-subset(Biomarker_NHANES_20072008,!is.na(Biomarker_NHANES_20072008$lbdsalsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbdscrsi))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbdsglsi))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxcrp))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxlypct))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxmcvsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxrdw))
merged_df10<-subset(merged_df9,!is.na(merged_df9$lbxsapsi))
merged_df11<-subset(merged_df10,!is.na(merged_df10$lbxwbcsi))
merged_df12<-subset(merged_df11,!is.na(merged_df11$ridageyr))
write.csv(merged_df12,"Biomarker_NHANES_20072008.csv")

Biomarker_NHANES_20092010<-merged_df2[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                          "lbxcrp","lbxlypct","lbxmcvsi","lbxrdw",
                                          "lbxsapsi","lbxwbcsi","ridageyr")]


merged_df3<-subset(Biomarker_NHANES_20092010,!is.na(Biomarker_NHANES_20092010$lbdsalsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbdscrsi))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbdsglsi))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxcrp))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxlypct))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxmcvsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxrdw))
merged_df10<-subset(merged_df9,!is.na(merged_df9$lbxsapsi))
merged_df11<-subset(merged_df10,!is.na(merged_df10$lbxwbcsi))
merged_df12<-subset(merged_df11,!is.na(merged_df11$ridageyr))
write.csv(merged_df12,"Biomarker_NHANES_20092010.csv")


Biomarker_NHANES_20152016<-merged_df2[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                          "lbxhscrp","lbxlypct","lbxmcvsi","lbxrdw",
                                          "lbxsapsi","lbxwbcsi","ridageyr")]


merged_df3<-subset(Biomarker_NHANES_20152016,!is.na(Biomarker_NHANES_20152016$lbdsalsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbdscrsi))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbdsglsi))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxhscrp))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxlypct))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxmcvsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxrdw))
merged_df10<-subset(merged_df9,!is.na(merged_df9$lbxsapsi))
merged_df11<-subset(merged_df10,!is.na(merged_df10$lbxwbcsi))
merged_df12<-subset(merged_df11,!is.na(merged_df11$ridageyr))
write.csv(merged_df12,"Biomarker_NHANES_20152016.csv")

Biomarker_NHANES_20172018<-merged_df2[, c("seqn","lbdsalsi","lbdscrsi","lbdsglsi",
                                          "lbxhscrp","lbxlypct","lbxmcvsi","lbxrdw",
                                          "lbxsapsi","lbxwbcsi","ridageyr")]


merged_df3<-subset(Biomarker_NHANES_20172018,!is.na(Biomarker_NHANES_20172018$lbdsalsi))
merged_df4<-subset(merged_df3,!is.na(merged_df3$lbdscrsi))
merged_df5<-subset(merged_df4,!is.na(merged_df4$lbdsglsi))
merged_df6<-subset(merged_df5,!is.na(merged_df5$lbxhscrp))
merged_df7<-subset(merged_df6,!is.na(merged_df6$lbxlypct))
merged_df8<-subset(merged_df7,!is.na(merged_df7$lbxmcvsi))
merged_df9<-subset(merged_df8,!is.na(merged_df8$lbxrdw))
merged_df10<-subset(merged_df9,!is.na(merged_df9$lbxsapsi))
merged_df11<-subset(merged_df10,!is.na(merged_df10$lbxwbcsi))
merged_df12<-subset(merged_df11,!is.na(merged_df11$ridageyr))
write.csv(merged_df12,"Biomarker_NHANES_20172018.csv")


library(data.table)
Biomarker_NHANES_2001_2002 <- rename(Biomarker_NHANES_2001_2002, 
                                     `Terms_Creatinine...9` = `Terms_Creatinine`, 
                                     `Terms_Lymphocyte%` = `Terms_Lymphocyte%...23`)
Biomarker_All_Years<-rbindlist(list(Biomarker_NHANES_1999_2000,
                                    Biomarker_NHANES_2001_2002,
                                    Biomarker_NHANES_2003_2004,
                                    Biomarker_NHANES_20052006,
                                    Biomarker_NHANES_20072008,
                                    Biomarker_NHANES_20092010,
                                    Biomarker_NHANES_20152016,
                                    Biomarker_NHANES_20172018))


breaks <- seq(0, 130, by = 10)

# Use cut() function to create deciles
Biomarker_All_Years$age_deciles <- cut(Biomarker_All_Years$Age, breaks = breaks, labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "101-110", "111-120","121-130"))

breaks <- seq(-10, 130, by = 10)
Biomarker_All_Years$Phenotypical_Age_deciles <- cut(Biomarker_All_Years$Phenotypic_Age, breaks = breaks, labels = c("-10-0", "0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "101-110", "111-120","121-130"))

Biomarker_All_Years$Difference_Age_Phenotypic_Age<-Biomarker_All_Years$Age-Biomarker_All_Years$Phenotypic_Age

summary(Biomarker_All_Years$Difference_Age_Phenotypic_Age)

# View the new variable with deciles
summary(factor(Biomarker_All_Years$age_deciles))


#Making a subset where we are excluding those aged <18
subset_Biomarker_All_Years<-subset(Biomarker_All_Years,
                                   Biomarker_All_Years$Age>18)

write.xlsx(subset_Biomarker_All_Years,"subset_Biomarker_All_Years.xlsx")

# Externally in Excel, changed age_decile values "11-20" and "21-30" to "19-30"
subset_Biomarker_All_Years<-read.xlsx("subset_Biomarker_All_Years.xlsx")

summary(subset_Biomarker_All_Years$Phenotypic_Age)


# Creating a subset without missing values for phenotypic age
subset_Biomarker_All_Years2<-subset(subset_Biomarker_All_Years,
                                    !is.na(subset_Biomarker_All_Years$Phenotypic_Age))
# Creating deciles for difference in age & phenotypic age
breaks <- seq(-70, 30, by = 10)
subset_Biomarker_All_Years2$Difference_Deciles <- cut(subset_Biomarker_All_Years2$Difference_Age_Phenotypic_Age, breaks = breaks, labels = c("-70 - -60", "-60 - -50", "-50 - -40", 
                                                                                                                             "-40 - -30", "-30 - -20", "-20 - -10",
                                                                                                                             "-10 - 0", "0-10","10-20","20-30"))
#Creating a histogram
density <- density(subset_Biomarker_All_Years2$Phenotypic_Age)
q_2.5 <- quantile(subset_Biomarker_All_Years2$Phenotypic_Age, probs = 0.025, na.rm = TRUE)
q_97.5 <- quantile(subset_Biomarker_All_Years2$Phenotypic_Age, probs = 0.975, na.rm = TRUE)

ggplot(data = subset_Biomarker_All_Years2, aes(x = Phenotypic_Age)) +
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
mean_age <- mean(subset_Biomarker_All_Years2$Phenotypic_Age, na.rm = TRUE)
sd_age <- sd(subset_Biomarker_All_Years2$Phenotypic_Age, na.rm = TRUE)

ggplot(data = subset_Biomarker_All_Years2, aes(x = Phenotypic_Age)) +
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
density <- density(subset_Biomarker_All_Years2$Difference_Age_Phenotypic_Age)
q_2.5 <- quantile(subset_Biomarker_All_Years2$Difference_Age_Phenotypic_Age, probs = 0.025, na.rm = TRUE)
q_97.5 <- quantile(subset_Biomarker_All_Years2$Difference_Age_Phenotypic_Age, probs = 0.975, na.rm = TRUE)

ggplot(data = subset_Biomarker_All_Years2, aes(x = Difference_Age_Phenotypic_Age)) +
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
mean_age <- mean(subset_Biomarker_All_Years2$Difference_Age_Phenotypic_Age, na.rm = TRUE)
sd_age <- sd(subset_Biomarker_All_Years2$Difference_Age_Phenotypic_Age, na.rm = TRUE)

ggplot(data = subset_Biomarker_All_Years2, aes(x = Difference_Age_Phenotypic_Age)) +
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
ggplot(subset_Biomarker_All_Years2, aes(x = Phenotypic_Age)) +
  geom_density() +
  geom_vline(xintercept = quantile(subset_Biomarker_All_Years2$Phenotypic_Age, c(0.05, 0.95)), color = "red", linetype = "dashed") +
  xlab("Phenotypic Age") +
  ylab("Density") +
  ggtitle("Density plot of Phenotypic Age with Outlier Range")

# Load the ggplot2 package
library(ggplot2)
PRELIMINARY_1999_2004$deciles<-as.character(PRELIMINARY_1999_2004$deciles)

# Create a data frame with the deciles and their frequency
df2 <- data.frame(table(PRELIMINARY_1999_2004$age_deciles))

# Calculate the percentage of each frequency
df2$percent <- round(df2$Freq / sum(df2$Freq) * 100, 2)

# Create a bar chart using ggplot2 and add text annotations
ggplot(df2, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = paste0("n=", Freq, "\n", percent, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribution of Actual Age", x = "Deciles", y = "Frequency")



library(ggplot2)
subset_Biomarker_All_Years2$Phenotypical_Age_deciles <- factor(subset_Biomarker_All_Years2$Phenotypical_Age_deciles, 
                                                               levels = c("-10-0", "0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100", "101-110", "111-120", "121-130"))
# Create a data frame with the deciles and their frequency
df3 <- data.frame(table(subset_Biomarker_All_Years2$Phenotypical_Age_deciles))

# Calculate the percentage of each frequency
df3$percent <- round(df3$Freq / sum(df3$Freq) * 100, 2)

# Create a bar chart using ggplot2 and add text annotations
ggplot(df3, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = paste0("n=", Freq, "\n", percent, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribution of Phenotypic Age", x = "Deciles", y = "Frequency")


# Create a data frame with the deciles and their frequency
df4 <- data.frame(table(subset_Biomarker_All_Years2$age_deciles))

# Calculate the percentage of each frequency
df4$percent <- round(df4$Freq / sum(df4$Freq) * 100, 2)

# Create a bar chart using ggplot2 and add text annotations
ggplot(df4, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = paste0("n=", Freq, "\n", percent, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribution of Actual Age", x = "Deciles", y = "Frequency")




subset_Biomarker_All_Years2$Difference_Deciles <- cut(subset_Biomarker_All_Years2$Difference_Age_Phenotypic_Age, breaks = breaks, labels = c("-70 - -60", "-60 - -50", "-50 - -40", 
                                                                                                                                             "-40 - -30", "-30 - -20", "-20 - -10",
                                                                                                                                             "-10 - 0", "0-10","10-20","20-30"))

# Create a data frame with the deciles and their frequency
df5 <- data.frame(table(subset_Biomarker_All_Years2$Difference_Deciles))

# Calculate the percentage of each frequency
df5$percent <- round(df5$Freq / sum(df5$Freq) * 100, 2)

# Create a bar chart using ggplot2 and add text annotations
ggplot(df5, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = paste0("n=", Freq, "\n", percent, "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribution of Difference Between Actual Age & Phenotypic Age", x = "Deciles", y = "Frequency")



summary(PAQ$)
summary(factor(PAQ$pad200))
summary(PAQ_C$pad200)
summary(PAQ_D$pad200)

summary(PAQ_E$)
summary(factor(PAQ_F$paq605))
summary(PAQ_I$pad615)
summary(PAQ_F$pad615)


summary(PAQ$pad080)

summary(PAQ_F$pad645)


summary(factor(PAQ_F$paq655))

summary(factor(PAQ_F$))

summary(factor(PAQ$pa))
summary(factor(PAQ_F$pad590))
# Select common columns
..common_cols <- intersect(intersect(names(PAQ), names(PAQ_B)), intersect(names(PAQ_C), names(PAQ_D)))

# Combine datasets using rbindlist
Activity_20012002_to_20052006 <- rbindlist(list(PAQ_B[, ..common_cols], PAQ_C[, ..common_cols], PAQ_D[, ..common_cols]))

summary(Activity_19992000_to_20052006$pad200)

summary(Activity_20072008_to_20172018$paq605)


Biomarker_NHANES_20092010_Activity<-merge(Biomarker_NHANES_20092010,PAQ_F,by="seqn")

Biomarker_NHANES_20072008_Activity<-merge(Biomarker_NHANES_20072008,PAQ_E,by="seqn")

Biomarker_NHANES_2003_2004_activity<-merge(Biomarker_NHANES_2003_2004,PAQ_C,by="seqn")

Biomarker_NHANES_2005_2006_activity<-merge(Biomarker_NHANES_20052006,PAQ_D,by="seqn")

summary(Activ)


firstmerge_19992006<-rbindlist(list(Biomarker_NHANES_1999_2000_activity,
                                    Biomarker_NHANES_2001_2002_activity))
secondmerge_19992006<-rbindlist(list(firstmerge_19992006,
                                    Biomarker_NHANES_2003_2004_activity),fill=TRUE)
thirdmerge_19992006<-rbindlist(list(secondmerge_19992006,
                                    Biomarker_NHANES_2005_2006_activity),fill=TRUE)
summary(thirdmerge_19992006$pad200=="1" | thirdmerge_19992006$pad320=="1")

firstmerge_20072018<-rbindlist(list(Biomarker_NHANES_20072008_Activity,
                                    Biomarker_NHANES_20092010_Activity),fill=TRUE)
secondmerge_20072018<-rbindlist(list(firstmerge_20072018,
                                     Biomarker_NHANES_20152016_Activity),fill=TRUE)
thirdmerge_20072018<-rbindlist(list(secondmerge_20072018,
                                    Biomarker_NHANES_20172018_Activity),fill=TRUE)
summary(thirdmerge_20072018$paq650=="1" | thirdmerge_20072018$paq665=="1")

subset_thirdmerge19992006<-subset(thirdmerge_19992006,
                                  thirdmerge_19992006$seqn %in% NHANES_Biomarker_AllYears_with_Exclusions$seqn)
subset_thirdmerge20072018<-subset(thirdmerge_20072018,
                                  thirdmerge_20072018$seqn %in% NHANES_Biomarker_AllYears_with_Exclusions$seqn)

summary(subset_thirdmerge19992006$pad200=="1" | subset_thirdmerge19992006$pad320=="1")
summary(subset_thirdmerge20072018$paq650=="1" | subset_thirdmerge20072018$paq665=="1")

subset_thirdmerge19992006$VIGOROUS_ACTIVITY<-subset_thirdmerge19992006$pad200
subset_thirdmerge19992006Final<-subset_thirdmerge19992006[, c("seqn","VIGOROUS_ACTIVITY")]

subset_thirdmerge20072018$VIGOROUS_ACTIVITY<-subset_thirdmerge20072018$paq650
subset_thirdmerge20072018Final<-subset_thirdmerge20072018[, c("seqn","VIGOROUS_ACTIVITY")]

subset_thirdmergeall<-rbindlist(list(subset_thirdmerge19992006Final,
                                     subset_thirdmerge20072018Final))

all_demo_combined<-rbindlist(list(DEMO_19992000,
                                  DEMO20012002,
                                  DEMO_20032004,
                                  DEMO_20052006,
                                  DEMO_20072008,
                                  DEMO_20092010,
                                  DEMO_20152016,
                                  DEMO_20172018),fill=TRUE)

NHANES_Biomarker_AllYears_with_Exclusions1<-merge(NHANES_Biomarker_AllYears_with_Exclusions,
                                                  all_demo_combined,by="seqn" )
NHANES_Biomarker_AllYears_with_Exclusions2<-merge(NHANES_Biomarker_AllYears_with_Exclusions1,
                                                  subset_thirdmergeall,by="seqn")


summary(factor(NHANES_Biomarker_AllYears_with_Exclusions2$Year))

NHANES_Biomarker_AllYears_with_Exclusions2$AgeGroups<-as.factor(ifelse(NHANES_Biomarker_AllYears_with_Exclusions2$Age<40,"19-39",
                                                                       ifelse(NHANES_Biomarker_AllYears_with_Exclusions2$Age>39&
                                                                                NHANES_Biomarker_AllYears_with_Exclusions2$Age<60,"40-59",
                                                                              ifelse(NHANES_Biomarker_AllYears_with_Exclusions2$Age>59,"60+",NA))))
summary(NHANES_Biomarker_AllYears_with_Exclusions2$AgeGroups)
summary(factor(NHANES_Biomarker_AllYears_with_Exclusions2$ridreth1))


NHANES_Biomarker_AllYears_with_Exclusions2$RACE_RECODE<-as.factor(ifelse(NHANES_Biomarker_AllYears_with_Exclusions2$ridreth1=="1","Mexican American",
                                                                         ifelse(NHANES_Biomarker_AllYears_with_Exclusions2$ridreth1=="2","Other Hispanic",
                                                                                ifelse(NHANES_Biomarker_AllYears_with_Exclusions2$ridreth1=="3","Non-Hispanic White",
                                                                                       ifelse(NHANES_Biomarker_AllYears_with_Exclusions2$ridreth1=="4","Non-Hispanic Black",
                                                                                              "Other Race - Including Multi-Racial")))))
summary(NHANES_Biomarker_AllYears_with_Exclusions2$RACE_RECODE)



BMI_19992000<-BMX_19992000[, c("seqn","bmxbmi")]
BMI_20012002<-BMX_20012002[, c("seqn","bmxbmi")]
BMI_20032004<-BMX_20032004csv[, c("seqn","bmxbmi")]
BMI_20052006<-BMX_20052006[, c("seqn","bmxbmi")]
BMI_20072008<-BMX_20072008[, c("seqn","bmxbmi")]
BMI_20092010<-BMX_20092010[, c("seqn","bmxbmi")]
BMI_20152016<-BMX_20152016[, c("seqn","bmxbmi")]
BMI_20172018<-BMX_20172018[, c("seqn","bmxbmi")]

AllBMI<-rbindlist(list(BMI_19992000,
                       BMI_20012002,
                       BMI_20032004,
                       BMI_20052006,
                       BMI_20072008,
                       BMI_20092010,
                       BMI_20152016,
                       BMI_20172018))
NHANES_Biomarker_AllYears_with_Exclusions3<-merge(NHANES_Biomarker_AllYears_with_Exclusions2,
                                                  AllBMI,by="seqn")
summary(NHANES_Biomarker_AllYears_with_Exclusions3$bmxbmi)

# dmdeduc2 is the right variable for education
summary(factor(NHANES_Biomarker_AllYears_with_Exclusions3$dmdeduc2))


# variable for family income: INDFMINC
summary(factor(NHANES_Biomarker_AllYears_with_Exclusions3$indfminc))
summary(factor(NHANES_Biomarker_AllYears_with_Exclusions3$indfmin2))

# merging variables for family income (indfminc and indfmin2)
NHANES_Biomarker_AllYears_with_Exclusions3$FamilyIncomeRecode<-as.factor(ifelse(is.na(NHANES_Biomarker_AllYears_with_Exclusions3$indfminc),
                                                                                NHANES_Biomarker_AllYears_with_Exclusions3$indfmin2,NHANES_Biomarker_AllYears_with_Exclusions3$indfminc))
summary(factor(NHANES_Biomarker_AllYears_with_Exclusions3$FamilyIncomeRecode))


# Smoking status requires a lot of calculation
SMQ_C <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2003-2004/SMQ_C.csv")
SMQ_D <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2005-2006/SMQ_D.csv")
SMQ_E <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2007-2008/SMQ_E.csv")
SMQ_F <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2009-2010/SMQ_F.csv")
SMQ_I <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2015-2016/SMQ_I.csv")
SMQ_J <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2017-2018/SMQ_J.csv")

All_Smokers<-rbindlist(list(SMQ,
                            SMQ_B,
                            SMQ_C,
                            SMQ_D,
                            SMQ_E,
                            SMQ_F,
                            SMQ_I,
                            SMQ_J),fill=TRUE)
summary(factor(All_Smokers$smq020))

All_Smokers$CurrentFormerORNeverSmoker<-as.factor(ifelse(All_Smokers$smq020=="1","Current or Former Smoker",
                                                                                        ifelse(All_Smokers$smq020=="2","Never Smoker",NA)))
All_Smokers$CurrentFormerORNeverSmoker<-as.factor(ifelse(All_Smokers$smq020=="1","Current or Former Smoker",
                                                         ifelse(All_Smokers$smq020=="2","Never Smoker",NA)))
summary(All_Smokers$smq040=="3" & All_Smokers$CurrentFormerORNeverSmoker=="Current or Former Smoker")

All_Smokers$CurrentSmokingFrequency<-as.factor(ifelse(All_Smokers$smq040=="3","Not at all",
                                                      ifelse(All_Smokers$smq040=="1"|
                                                               All_Smokers$smq040=="2","Smoking",NA)))

All_Smokers$SmokingStatus<-as.factor(ifelse(All_Smokers$CurrentFormerORNeverSmoker=="Current or Former Smoker" &
                                                            All_Smokers$CurrentSmokingFrequency=="Smoking","Current Smoker",
                                            ifelse(All_Smokers$CurrentFormerORNeverSmoker=="Current or Former Smoker" &
                                                     All_Smokers$CurrentSmokingFrequency=="Not at all","Former Smoker","Never Smoker")))
summary(All_Smokers$SmokingStatus)
summary(All_Smokers$SmokingStatus)

NHANES_Biomarker_AllYears_with_Exclusions4<-merge(NHANES_Biomarker_AllYears_with_Exclusions3,
                                                  All_Smokers,by="seqn",all.x=TRUE)


# Alcohol consumption
ALQ <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/1999-2000/ALQ.csv")
ALQ_B <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2001-2002/ALQ_B.csv")
ALQ_C <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2003-2004/ALQ_C.csv")
ALQ_D <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2005-2006/ALQ_D.csv")
ALQ_E <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2007-2008/ALQ_E.csv")
ALQ_F <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2009-2010/ALQ_F.csv")
ALQ_I <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2015-2016/ALQ_I.csv")
ALQ_J <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2017-2018/ALQ_J.csv")

All_Alcohol<-rbindlist(list(ALQ,
                            ALQ_B,
                            ALQ_C,ALQ_D,ALQ_E,ALQ_F,ALQ_I,ALQ_J),fill=TRUE)

NHANES_Biomarker_AllYears_with_Exclusions5<-merge(NHANES_Biomarker_AllYears_with_Exclusions4,
                                                  All_Alcohol,by="seqn",all.x=TRUE)
> NHANES_Biomarker_AllYears_with_Exclusions12$Alcohol_HeavyDrinking<-as.factor(ifelse(is.na(NHANES_Biomarker_AllYears_with_Exclusions12$alq150),NHANES_Biomarker_AllYears_with_Exclusions12$alq151,NHANES_Biomarker_AllYears_with_Exclusions12$alq150))
> summary(factor(NHANES_Biomarker_AllYears_with_Exclusions12$Alcohol_HeavyDrinking))
1     2     7     9  NA's 
 4949 24061     2    19  8724 
> NHANES_Biomarker_AllYears_with_Exclusions12$Alcohol_AnyDrinking<-as.factor(ifelse(is.na(NHANES_Biomarker_AllYears_with_Exclusions12$alq100),NHANES_Biomarker_AllYears_with_Exclusions12$alq101,NHANES_Biomarker_AllYears_with_Exclusions12$alq100))
> summary(factor(NHANES_Biomarker_AllYears_with_Exclusions12$Alcohol_AnyDrinking))
    1     2     9  NA's 
17328  7473    17 12937 
> NHANES_Biomarker_AllYears_with_Exclusions12$Alcohol_Any_Categories<-as.factor(ifelse(NHANES_Biomarker_AllYears_with_Exclusions12$Alcohol_AnyDrinking=="1" & NHANES_Biomarker_AllYears_with_Exclusions12$Alcohol_HeavyDrinking=="1","Heavy Drinker", ifelse(NHANES_Biomarker_AllYears_with_Exclusions12$Alcohol_AnyDrinking=="1" & NHANES_Biomarker_AllYears_with_Exclusions12$Alcohol_HeavyDrinking=="2","Regular Drinker", "Not a Drinker")))
> summary(NHANES_Biomarker_AllYears_with_Exclusions12$Alcohol_Any_Categories)

#Diabetes
DIQ <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/1999-2000/DIQ.csv")
DIQ_B <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2001-2002/DIQ_B.csv")
DIQ_C <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2003-2004/DIQ_C.csv")
DIQ_D <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2005-2006/DIQ_D.csv")
DIQ_E <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2007-2008/DIQ_E.csv")
DIQ_F <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2009-2010/DIQ_F.csv")
DIQ_I <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2015-2016/DIQ_I.csv")
DIQ_J <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2017-2018/DIQ_J.csv")

GLU <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/1999-2000/GLU.csv")
GLU_B <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2001-2002/GLU_B.csv")
GLU_C <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2003-2004/GLU_C.csv")
GLU_D <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2005-2006/GLU_D.csv")
GLU_E <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2007-2008/GLU_E.csv")
GLU_F <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2009-2010/GLU_F.csv")
GLU_I <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2015-2016/GLU_I.csv")
GLU_J <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2017-2018/GLU_J.csv")


library(data.table)
All_Diabetes<-rbindlist(list(DIQ,
                             DIQ_B,DIQ_C,DIQ_D,DIQ_E,DIQ_F,DIQ_I,DIQ_J),fill = TRUE)
summary(factor(All_Diabetes$diq010))
All_Diabetes$Told_Had_Diabetes<-as.factor(ifelse(All_Diabetes$diq010=="1","Yes",
                                                 ifelse(All_Diabetes$diq010=="2","No",NA)))
NHANES_Biomarker_AllYears_with_Exclusions6<-merge(NHANES_Biomarker_AllYears_with_Exclusions5,
                                                  All_Diabetes,by="seqn",all.x=TRUE)

summary(NHANES_Biomarker_AllYears_with_Exclusions3$`Glucose_mmol/L`>7)

NHANES_Biomarker_AllYears_with_Exclusions3$Diabetes_Based_on_fastingGlucose<-as.factor(ifelse(NHANES_Biomarker_AllYears_with_Exclusions3$`Glucose_mmol/L`>7,"Yes","No"))

BPQ <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/1999-2000/BPQ.csv")
BPQ_B <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2001-2002/BPQ_B.csv")
BPQ_C <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2003-2004/BPQ_C.csv")
BPQ_D <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2005-2006/BPQ_D.csv")
BPQ_E <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2007-2008/BPQ_E.csv")
BPQ_F <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2009-2010/BPQ_F.csv")
BPQ_I <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2015-2016/BPQ_I.csv")
BPQ_J <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2017-2018/BPQ_J.csv")

BPX <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/1999-2000/BPX.csv")
BPX_B <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2001-2002/BPX_B.csv")
BPX_C <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2003-2004/BPX_C.csv")
BPX_D <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2005-2006/BPX_D.csv")
BPX_E <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2007-2008/BPX_E.csv")
BPX_F <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2009-2010/BPX_F.csv")
BPX_I <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2015-2016/BPX_I.csv")
BPX_J <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2017-2018/BPX_J.csv")

All_HTN<-rbindlist(list(BPQ,
                        BPQ_B,
                        BPQ_C,
                        BPQ_D,
                        BPQ_E,
                        BPQ_F,
                        BPQ_I,
                        BPQ_J),fill=TRUE)
summary(factor(All_HTN$bpq020))
NHANES_Biomarker_AllYears_with_Exclusions7<-merge(NHANES_Biomarker_AllYears_with_Exclusions6,
                                                  All_HTN,by="seqn",all.x=TRUE)

All_BPX<-rbindlist(list(BPX,
                        BPX_B,
                        BPX_C,
                        BPX_D,
                        BPX_E,
                        BPX_F,
                        BPX_I,
                        BPX_J),fill=TRUE)
summary(All_BPX$bpxsy1>140)
NHANES_Biomarker_AllYears_with_Exclusions8<-merge(NHANES_Biomarker_AllYears_with_Exclusions7,
                                                  All_BPX,by="seqn",all.x=TRUE)

All_BPX$HTN_Based_on_Systolic<-as.factor(ifelse(All_BPX$bpxsy1>140,"1","0"))
summary(All_BPX$HTN_Based_on_Systolic)

All_BPX$HTN_Based_on_Diastolic<-as.factor(ifelse(All_BPX$bpxdi1>90,"1","0"))
summary(All_BPX$HTN_Based_on_Diastolic)


All_BPX$HTNBasedonSyst_or_Dias<-as.factor(ifelse(All_BPX$HTN_Based_on_Systolic=="1"|
                                                   All_BPX$HTN_Based_on_Diastolic=="1","1","0"))
summary(All_BPX$HTNBasedonSyst_or_Dias)



#Cholesterol
LAB13 <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/1999-2000/LAB13.csv")
L13_B <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2001-2002/L13_B.csv")
L13_C <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2003-2004/L13_C.csv")
TCHOL_D <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2005-2006/TCHOL_D.csv")
TCHOL_E <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2007-2008/TCHOL_E.csv")
TCHOL_F <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2009-2010/TCHOL_F.csv")
TCHOL_I <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2015-2016/TCHOL_I.csv")
TCHOL_J <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2017-2018/TCHOL_J.csv")


All_Cholesterol<-rbindlist(list(LAB13,
                                L13_B,
                                L13_C,
                                TCHOL_D,
                                TCHOL_E,
                                TCHOL_F,
                                TCHOL_I,
                                TCHOL_J),fill=TRUE)
summary(All_Cholesterol$lbdtcsi>6)

All_Cholesterol$Dyslipidemia<-as.factor(ifelse(All_Cholesterol$lbdtcsi>6,"1","0"))
NHANES_Biomarker_AllYears_with_Exclusions9<-merge(NHANES_Biomarker_AllYears_with_Exclusions8,
                                                  All_Cholesterol,by="seqn",all.x=TRUE)

# Heart diseases
MCQ <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/1999-2000/MCQ.csv")
MCQ_B <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2001-2002/MCQ_B.csv")
MCQ_C <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2003-2004/MCQ_C.csv")
MCQ_D <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2005-2006/MCQ_D.csv")
MCQ_E <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2007-2008/MCQ_E.csv")
MCQ_F <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2009-2010/MCQ_F.csv")
MCQ_I <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2015-2016/MCQ_I.csv")
MCQ_J <- read.csv("~/Desktop/Freelance_Bellsant/nhanes/data/sources/csv/2017-2018/MCQ_J.csv")

All_Heart_Disease<-rbindlist(list(MCQ,
                                  MCQ_B,MCQ_C,MCQ_D,MCQ_E,MCQ_F,MCQ_I,MCQ_J),fill=TRUE)
NHANES_Biomarker_AllYears_with_Exclusions10<-merge(NHANES_Biomarker_AllYears_with_Exclusions9,
                                                   All_Heart_Disease,by="seqn",all.x=TRUE)

summary(factor(All_Heart_Disease$mcq160c))
summary(factor(All_Heart_Disease$mcq160b))
summary(factor(All_Heart_Disease$mcq160f))
N



#Cancer
summary(factor(All_Heart_Disease$mcq220))




# pulmonary
summary(factor(All_Heart_Disease$mcq010))
summary(factor(All_Heart_Disease$mcq160g))
summary(factor(All_Heart_Disease$mcq160k))


# Kidney Disease

All_KIQ<-rbindlist(list(KIQ,
                        KIQ_U_B,
                        KIQ_U_C,
                        KIQ_U_D,
                        KIQ_U_E,
                        KIQ_U_F,
                        KIQ_U_I,
                        KIQ_U_J),fill=TRUE)
summary(factor(All_KIQ$kiq022))
summary(factor(KIQ_U_F))
NHANES_Biomarker_AllYears_with_Exclusions11<-merge(NHANES_Biomarker_AllYears_with_Exclusions10,
                                                   All_KIQ,by="seqn",all.x=TRUE)

# Cancer 

summary(factor(All_Heart_Disease$mcq220))


summary(NHANES_Biomarker_AllYears_with_Exclusions11$Phenotypic_Age)
summary(NHANES_Biomarker_AllYears_with_Exclusions11$AgeGroups)

# Tables
library(arsenal)
# 1 Age groups
AgeGroupsTable<-tableby(AgeGroups~Phenotypic_Age,data=NHANES_Biomarker_AllYears_with_Exclusions11)
summary(AgeGroupsTable,text=TRUE)
write2word(AgeGroupsTable,"AgeGroupsTable.docx")

# 2 Age Groups 2
AgeGroups2Table<-tableby(age_deciles~Phenotypic_Age,data=NHANES_Biomarker_AllYears_with_Exclusions11)
write2word(AgeGroups2Table,"AgeGroups2Table.docx")

# 3 Gender
GenderTable<-tableby(riagendr~Phenotypic_Age,data=NHANES_Biomarker_AllYears_with_Exclusions11)
write2word(GenderTable,"GenderTable.docx")

# 4 Ethnicity
RaceRecodeTables<-tableby(RACE_RECODE~Phenotypic_Age,data=NHANES_Biomarker_AllYears_with_Exclusions11)
write2word(RaceRecodeTables,"RaceRecodeTables.docx")

# 5 BMI
NHANES_Biomarker_AllYears_with_Exclusions12$BMI_Groups<-as.factor(ifelse(NHANES_Biomarker_AllYears_with_Exclusions12$bmxbmi<20,"<20",
                                                                         ifelse(NHANES_Biomarker_AllYears_with_Exclusions12$bmxbmi>20&NHANES_Biomarker_AllYears_with_Exclusions12$bmxbmi<24.99,"20-25",
                                                                                ifelse(NHANES_Biomarker_AllYears_with_Exclusions12$bmxbmi>25&NHANES_Biomarker_AllYears_with_Exclusions12$bmxbmi<29.99,"25-30",
                                                                                       ifelse(NHANES_Biomarker_AllYears_with_Exclusions12$bmxbmi>30 &
                                                                                                NHANES_Biomarker_AllYears_with_Exclusions12$bmxbmi<34.99,"30-35",
                                                                                              ifelse(NHANES_Biomarker_AllYears_with_Exclusions12$bmxbmi>35 &
                                                                                                       NHANES_Biomarker_AllYears_with_Exclusions12$bmxbmi<39.99,"35-40",
                                                                                                     ifelse(NHANES_Biomarker_AllYears_with_Exclusions12$bmxbmi>40,"40 or higher",NA)))))))
BMI_Table<-tableby(BMI_Groups~Phenotypic_Age,data=NHANES_Biomarker_AllYears_with_Exclusions12)
write2word(BMI_Table,"BMI_Table.docx")
summary(factor(NHANES_Biomarker_AllYears_with_Exclusions12$BMI_Groups))
# Education
summary(factor(NHANES_Biomarker_AllYears_with_Exclusions11$dmdeduc))
Education_Table<-tableby(dmdeduc2~Phenotypic_Age,data=NHANES_Biomarker_AllYears_with_Exclusions11)
write2word(Education_Table,"Education_Table.docx")

#Income
INQ_E$familyincomeannual<-ifelse(INQ_E$ind235=="1","$ <5000",
                                 ifelse(INQ_E$ind235=="2" ,"5000-10000",
                                        ifelse(INQ_E$ind235=="3","10000-15000",
                                               ifelse(INQ_E$ind235=="4","15000-2000",
                                                      ifelse(INQ_E$ind235=="5","20000-25000",
                                                             ifelse(INQ_E$ind235=="6","25000-35000",
                                                                    ifelse(INQ_E$ind235=="7" ,"35000-45000",
                                                                           ifelse(INQ_E$ind235=="8","45000-55000",
                                                                                  ifelse(INQ_E$ind235=="9","55000-65000",
                                                                                         ifelse(INQ_E$ind235=="10","65000-75000",
                                                                                                ifelse(INQ_E$ind235=="11","75000-100000",
                                                                                                       ifelse(INQ_E$ind235=="12",">100000",NA))))))))))))


INQ_F$familyincomeannual<-ifelse(INQ_F$ind235=="1","$ <5000",
                                 ifelse(INQ_F$ind235=="2" ,"5000-10000",
                                        ifelse(INQ_F$ind235=="3","10000-15000",
                                               ifelse(INQ_F$ind235=="4","15000-2000",
                                                      ifelse(INQ_F$ind235=="5","20000-25000",
                                                             ifelse(INQ_F$ind235=="6","25000-35000",
                                                                    ifelse(INQ_F$ind235=="7" ,"35000-45000",
                                                                           ifelse(INQ_F$ind235=="8","45000-55000",
                                                                                  ifelse(INQ_F$ind235=="9","55000-65000",
                                                                                         ifelse(INQ_F$ind235=="10","65000-75000",
                                                                                                ifelse(INQ_F$ind235=="11","75000-100000",
                                                                                                       ifelse(INQ_F$ind235=="12",">100000",NA))))))))))))


INQ_I$familyincomeannual<-ifelse(INQ_I$ind235=="1","$ <5000",
                                 ifelse(INQ_I$ind235=="2" ,"5000-10000",
                                        ifelse(INQ_I$ind235=="3","10000-15000",
                                               ifelse(INQ_I$ind235=="4","15000-2000",
                                                      ifelse(INQ_I$ind235=="5","20000-25000",
                                                             ifelse(INQ_I$ind235=="6","25000-35000",
                                                                    ifelse(INQ_I$ind235=="7" ,"35000-45000",
                                                                           ifelse(INQ_I$ind235=="8","45000-55000",
                                                                                  ifelse(INQ_I$ind235=="9","55000-65000",
                                                                                         ifelse(INQ_I$ind235=="10","65000-75000",
                                                                                                ifelse(INQ_I$ind235=="11","75000-100000",
                                                                                                       ifelse(INQ_I$ind235=="12",">100000",NA))))))))))))


INQ_J$familyincomeannual<-ifelse(INQ_J$ind235=="1","$ <5000",
                                 ifelse(INQ_J$ind235=="2" ,"5000-10000",
                                        ifelse(INQ_J$ind235=="3","10000-15000",
                                               ifelse(INQ_J$ind235=="4","15000-2000",
                                                      ifelse(INQ_J$ind235=="5","20000-25000",
                                                             ifelse(INQ_J$ind235=="6","25000-35000",
                                                                    ifelse(INQ_J$ind235=="7" ,"35000-45000",
                                                                           ifelse(INQ_J$ind235=="8","45000-55000",
                                                                                  ifelse(INQ_J$ind235=="9","55000-65000",
                                                                                         ifelse(INQ_J$ind235=="10","65000-75000",
                                                                                                ifelse(INQ_J$ind235=="11","75000-100000",
                                                                                                       ifelse(INQ_J$ind235=="12",">100000",NA))))))))))))




onlyincome19992006$familyincomeannual<-ifelse(onlyincome19992006$indhhinc=="1","$ <5000",
                                 ifelse(onlyincome19992006$indhhinc=="2" ,"5000-10000",
                                        ifelse(onlyincome19992006$indhhinc=="3","10000-15000",
                                               ifelse(onlyincome19992006$indhhinc=="4","15000-2000",
                                                      ifelse(onlyincome19992006$indhhinc=="5","20000-25000",
                                                             ifelse(onlyincome19992006$indhhinc=="6","25000-35000",
                                                                    ifelse(onlyincome19992006$indhhinc=="7" ,"35000-45000",
                                                                           ifelse(onlyincome19992006$indhhinc=="8","45000-55000",
                                                                                  ifelse(onlyincome19992006$indhhinc=="9","55000-65000",
                                                                                         ifelse(onlyincome19992006$indhhinc=="10","65000-75000",">100000"))))))))))
summary(factor(onlyincome19992006$familyincomeannual))

onlyincome20072018<-rbindlist(list(INQ_E,
                                   INQ_F,
                                   INQ_I,
                                   INQ_J),fill = TRUE)
onlyincome20072018<-onlyincome20072018[, c("seqn","familyincomeannual")]
onlyincome19992006<-onlyincome19992006[, c("seqn","familyincomeannual")]

All_income<-rbindlist(list(onlyincome19992006,onlyincome20072018))

NHANES_Biomarker_AllYears_with_Exclusions12<-merge(NHANES_Biomarker_AllYears_with_Exclusions11,
                                                   All_income,by="seqn",all.x=TRUE)

Table_income<-tableby(familyincomeannual~Phenotypic_Age,data=NHANES_Biomarker_AllYears_with_Exclusions12)
summary(Table_income,text=TRUE)
write2word(Table_income,"Table_income.docx")

# Smoker
SmokingStatus_Table<-tableby(SmokingStatus~Phenotypic_Age,data=NHANES_Biomarker_AllYears_with_Exclusions12)
write2word(SmokingStatus_Table,"SmokingStatus_Table.docx")

# Alcohol
Alcohol_Table<-tableby(Alcohol_Any_Categories~Phenotypic_Age,
                       data=NHANES_Biomarker_AllYears_with_Exclusions12)
write2word(Alcohol_Table,"Alcohol_Table.docx")

# Diabetes
summary(NHANES_Biomarker_AllYears_with_Exclusions12$Told_Had_Diabetes)
Diabetes_Table<-tableby(Told_Had_Diabetes~Phenotypic_Age,data=NHANES_Biomarker_AllYears_with_Exclusions12)
write2word(Diabetes_Table,"Diabetes_Table.docx")

# HTN
summary(factor(NHANES_Biomarker_AllYears_with_Exclusions12$HTNBasedonSyst_or_Dias))
Hypertension_table<-tableby(HTNBasedonSyst_or_Dias~Phenotypic_Age,
                            data=NHANES_Biomarker_AllYears_with_Exclusions12)
write2word(Hypertension_table,"Hypertension_table.docx")

# Dyslipidemia 
Dyslipidemia_table<-tableby(Dyslipidemia~Phenotypic_Age,
                            data=NHANES_Biomarker_AllYears_with_Exclusions12)
write2word(Dyslipidemia_table,"Dyslipidemia_table.docx")

# CKD
KidneyDiseaseTable<-tableby(kiq022~Phenotypic_Age,
                            data=NHANES_Biomarker_AllYears_with_Exclusions12)

# HeartDisease
NHANES_Biomarker_AllYears_with_Exclusions12$HeartDisease<-as.factor(ifelse(NHANES_Biomarker_AllYears_with_Exclusions12$mcq160b=="1" | NHANES_Biomarker_AllYears_with_Exclusions12$mcq160c=="1" | NHANES_Biomarker_AllYears_with_Exclusions12$mcq160f=="1","1","0"))

Table_CardiacDisease<-tableby(HeartDisease~Phenotypic_Age,data=NHANES_Biomarker_AllYears_with_Exclusions12)
write2word(Table_CardiacDisease,"Table_CardiacDisease.docx")

# Cancer
Cancer_table<-tableby(mcq220~Phenotypic_Age,
                      data=NHANES_Biomarker_AllYears_with_Exclusions12)
write2word(Cancer_table,"Cancer_table.docx")

# Pulmonary
NHANES_Biomarker_AllYears_with_Exclusions12$Any_Pulmonary_Disease<-as.factor(ifelse(NHANES_Biomarker_AllYears_with_Exclusions12$mcq010=="1"|
                                                                                      NHANES_Biomarker_AllYears_with_Exclusions12$mcq160g=="1"|
                                                                                      NHANES_Biomarker_AllYears_with_Exclusions12$mcq160k=="1","1","0"))
Pulmonary_table<-tableby(Any_Pulmonary_Disease~Phenotypic_Age,
                         data=NHANES_Biomarker_AllYears_with_Exclusions12)
write2word(Pulmonary_table,"Pulmonary_table.docx")

# Vigorous Activity
table_vigorous_activity<-tableby(VIGOROUS_ACTIVITY~Phenotypic_Age,data=NHANES_Biomarker_AllYears_with_Exclusions12)
write2word(table_vigorous_activity,"table_vigorous_activity.docx")



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
                          data    = NHANES_Biomarker_AllYears_with_Exclusions12)

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
phenotypicage_agegroups_summarysd$sd <- sqrt(phenotypicage_agegroups_summarysd$Phenotypic_Age)

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
Difference_Age_Phenotypic_Age_agegroups_summarysd$sd <- sqrt(Difference_Age_Phenotypic_Age_agegroups_summarysd$Difference_Age_Phenotypic_Age)

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
phenotypicage_agegroups_summarysd2$sd <- sqrt(phenotypicage_agegroups_summarysd2$Phenotypic_Age)

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
Difference_Age_Phenotypic_Age_agegroups_summarysd2$sd <- sqrt(Difference_Age_Phenotypic_Age_agegroups_summarysd2$Difference_Age_Phenotypic_Age)

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
phenotypicage_race_summarysd$sd <- sqrt(phenotypicage_race_summarysd$Phenotypic_Age)

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
Difference_Age_Phenotypic_Age_race_summarysd$sd <- sqrt(Difference_Age_Phenotypic_Age_race_summarysd$Difference_Age_Phenotypic_Age)


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
phenotypicage_sex_summarysd$sd <- sqrt(phenotypicage_sex_summarysd$Phenotypic_Age)

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
Difference_Age_Phenotypic_Age_sex_summarysd$sd <- sqrt(Difference_Age_Phenotypic_Age_sex_summarysd$Difference_Age_Phenotypic_Age)



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
phenotypicage_BMI_Groups_summarysd$sd <- sqrt(phenotypicage_BMI_Groups_summarysd$Phenotypic_Age)

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
Difference_Age_Phenotypic_Age_BMI_Groups_summarysd$sd <- sqrt(Difference_Age_Phenotypic_Age_BMI_Groups_summarysd$Difference_Age_Phenotypic_Age)


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
phenotypicage_Education_summarysd$sd <- sqrt(phenotypicage_Education_summarysd$Phenotypic_Age)

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
Difference_Age_Phenotypic_Age_BMI_Groups_summarysd$sd <- sqrt(Difference_Age_Phenotypic_Age_BMI_Groups_summarysd$Difference_Age_Phenotypic_Age)



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
phenotypicage_familyincomeannual <- svyby(~Phenotypic_Age, ~factor(FamilyIncomeRecode), design = nhanesDesign, svymean)
phenotypicage_familyincomeannual_sd <- svyby(~Phenotypic_Age, ~factor(FamilyIncomeRecode), design = nhanesDesign, svyvar)
phenotypicage_familyincomeannual_range <- svyby(~Phenotypic_Age, ~FamilyIncomeRecode, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_familyincomeannual_summary <- data.frame(phenotypicage_familyincomeannual)
phenotypicage_familyincomeannual_summarysd<-data.frame(phenotypicage_familyincomeannual_sd)
phenotypicage_familyincomeannual_summarysd$sd <- sqrt(phenotypicage_familyincomeannual_summarysd$Phenotypic_Age)

# Rename the columns
names(phenotypicage_familyincomeannual_summary) <- c("age_deciles", "mean", "se", "sd")

# View the results
print(phenotypicage_familyincomeannual_summary)



differenceage_familyincomeannual <- svyby(~Difference_Age_Phenotypic_Age, ~factor(FamilyIncomeRecode), design = nhanesDesign, svymean)
differenceage_familyincomeannual_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(FamilyIncomeRecode), design = nhanesDesign, svyvar)
differenceage_familyincomeannual_range <- svyby(~Difference_Age_Phenotypic_Age, ~FamilyIncomeRecode, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
differenceage_familyincomeannual_summary <- data.frame(differenceage_familyincomeannual)
differenceage_familyincomeannual_summarysd<-data.frame(differenceage_familyincomeannual_sd)
differenceage_familyincomeannual_summarysd$sd <- sqrt(differenceage_familyincomeannual_sd$Difference_Age_Phenotypic_Age)

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
phenotypicage_Alcohol_summarysd$sd <- sqrt(phenotypicage_Alcohol_summarysd$Phenotypic_Age)


differenceage_Alcohol <- svyby(~Difference_Age_Phenotypic_Age, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svymean)
differenceage_Alcohol_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(Alcohol_Any_Categories), design = nhanesDesign, svyvar)
differenceage_Alcohol_range <- svyby(~Difference_Age_Phenotypic_Age, ~Alcohol_Any_Categories, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
phenotypicage_Alcohol_summary <- data.frame(differenceage_Alcohol)
phenotypicage_Alcohol_summarysd<-data.frame(differenceage_Alcohol_sd)
phenotypicage_Alcohol_summarysd$sd <- sqrt(differenceage_Alcohol_sd$Difference_Age_Phenotypic_Age)

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
phenotypicage_SmokingStatus_summarysd$sd <- sqrt(phenotypicage_SmokingStatus_summarysd$Phenotypic_Age)


Difference_SmokingStatus <- svyby(~Difference_Age_Phenotypic_Age, ~factor(SmokingStatus), design = nhanesDesign, svymean)
Difference_SmokingStatus_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(SmokingStatus), design = nhanesDesign, svyvar)
Difference_SmokingStatus_range <- svyby(~Difference_Age_Phenotypic_Age, ~SmokingStatus, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_SmokingStatus_summary <- data.frame(Difference_SmokingStatus)
Difference_SmokingStatus_summarysd<-data.frame(Difference_SmokingStatus_sd)
Difference_SmokingStatus_summarysd$sd <- sqrt(Difference_SmokingStatus_summarysd$Difference_Age_Phenotypic_Age)


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
phenotypicage_Diabetes_summarysd$sd <- sqrt(phenotypicage_Diabetes_summarysd$Phenotypic_Age)

Difference_Diabetes <- svyby(~Difference_Age_Phenotypic_Age, ~factor(Told_Had_Diabetes), design = nhanesDesign, svymean)
Difference_Diabetes_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(Told_Had_Diabetes), design = nhanesDesign, svyvar)
Difference_Diabetes_range <- svyby(~Difference_Age_Phenotypic_Age, ~Told_Had_Diabetes, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_Diabetes_summary <- data.frame(Difference_Diabetes)
Difference_Diabetes_summarysd<-data.frame(Difference_Diabetes_sd)
Difference_Diabetes_summarysd$sd <- sqrt(Difference_Diabetes_summarysd$Difference_Age_Phenotypic_Age)

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
phenotypicage_Hypertension_summarysd$sd <- sqrt(phenotypicage_Hypertension_summarysd$Phenotypic_Age)

Difference_Hypertension <- svyby(~Difference_Age_Phenotypic_Age, ~factor(HTNBasedonSyst_or_Dias), design = nhanesDesign, svymean)
Difference_Hypertension_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(HTNBasedonSyst_or_Dias), design = nhanesDesign, svyvar)
Difference_Hypertension_range <- svyby(~Difference_Age_Phenotypic_Age, ~HTNBasedonSyst_or_Dias, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_Hypertension_summary <- data.frame(Difference_Hypertension)
Difference_Hypertension_summarysd<-data.frame(Difference_Hypertension_sd)
Difference_Hypertension_summarysd$sd <- sqrt(Difference_Hypertension_summarysd$Difference_Age_Phenotypic_Age)

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
phenotypicage_Dyslipidemia_summarysd$sd <- sqrt(phenotypicage_Dyslipidemia_summarysd$Phenotypic_Age)

Difference_Dyslipidemia <- svyby(~Difference_Age_Phenotypic_Age, ~factor(Dyslipidemia), design = nhanesDesign, svymean)
Difference_Dyslipidemia_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(Dyslipidemia), design = nhanesDesign, svyvar)
Difference_Dyslipidemia_range <- svyby(~Difference_Age_Phenotypic_Age, ~Dyslipidemia, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_Dyslipidemia_summary <- data.frame(Difference_Dyslipidemia)
Difference_Dyslipidemia_summarysd<-data.frame(Difference_Dyslipidemia_sd)
Difference_Dyslipidemia_summarysd$sd <- sqrt(Difference_Dyslipidemia_summarysd$Difference_Age_Phenotypic_Age)


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
phenotypicage_HeartDisease_summarysd$sd <- sqrt(phenotypicage_HeartDisease_summarysd$Phenotypic_Age)

Difference_HeartDisease <- svyby(~Difference_Age_Phenotypic_Age, ~factor(HeartDisease), design = nhanesDesign, svymean)
Difference_HeartDisease_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(HeartDisease), design = nhanesDesign, svyvar)
Difference_HeartDisease_range <- svyby(~Difference_Age_Phenotypic_Age, ~HeartDisease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_HeartDisease_summary <- data.frame(Difference_HeartDisease)
Difference_HeartDisease_summarysd<-data.frame(Difference_HeartDisease_sd)
Difference_HeartDisease_summarysd$sd <- sqrt(Difference_HeartDisease_summarysd$Difference_Age_Phenotypic_Age)



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
phenotypicage_Cancer_summarysd$sd <- sqrt(phenotypicage_Cancer_summarysd$Phenotypic_Age)


Difference_Cancer <- svyby(~Difference_Age_Phenotypic_Age, ~factor(mcq220), design = nhanesDesign, svymean)
Difference_Cancer_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(mcq220), design = nhanesDesign, svyvar)
Difference_Cancer_range <- svyby(~Difference_Age_Phenotypic_Age, ~mcq220, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_Cancer_summary <- data.frame(Difference_Cancer)
Difference_Cancer_summarysd<-data.frame(Difference_Cancer_sd)
Difference_Cancer_summarysd$sd <- sqrt(Difference_Cancer_summarysd$Difference_Age_Phenotypic_Age)

# Rename the columns
names(phenotypicage_Cancer_summary) <- c("age_deciles", "mean", "se", "sd")




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
phenotypicage_Any_Pulmonary_Disease_summarysd$sd <- sqrt(phenotypicage_Any_Pulmonary_Disease_summarysd$Phenotypic_Age)

Difference_Any_Pulmonary_Disease <- svyby(~Difference_Age_Phenotypic_Age, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svymean)
Difference_Any_Pulmonary_Disease_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(Any_Pulmonary_Disease), design = nhanesDesign, svyvar)
Difference_Any_Pulmonary_Disease_range <- svyby(~Difference_Age_Phenotypic_Age, ~Any_Pulmonary_Disease, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_Any_Pulmonary_Disease_summary <- data.frame(Difference_Any_Pulmonary_Disease)
Difference_Any_Pulmonary_Disease_summarysd<-data.frame(Difference_Any_Pulmonary_Disease_sd)
Difference_Any_Pulmonary_Disease_summarysd$sd <- sqrt(Difference_Any_Pulmonary_Disease_summarysd$Difference_Age_Phenotypic_Age)

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
phenotypicage_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(phenotypicage_VIGOROUS_ACTIVITY_summarysd$Phenotypic_Age)


Difference_VIGOROUS_ACTIVITY <- svyby(~Difference_Age_Phenotypic_Age, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svymean)
Difference_VIGOROUS_ACTIVITY_sd <- svyby(~Difference_Age_Phenotypic_Age, ~factor(VIGOROUS_ACTIVITY), design = nhanesDesign, svyvar)
Difference_VIGOROUS_ACTIVITY_range <- svyby(~Difference_Age_Phenotypic_Age, ~VIGOROUS_ACTIVITY, design = nhanesDesign, FUN = svyquantile, quantiles = c(0, 1))

# Combine the mean and standard deviation into a data frame
Difference_VIGOROUS_ACTIVITY_summary <- data.frame(Difference_VIGOROUS_ACTIVITY)
Difference_VIGOROUS_ACTIVITY_summarysd<-data.frame(Difference_VIGOROUS_ACTIVITY_sd)
Difference_VIGOROUS_ACTIVITY_summarysd$sd <- sqrt(Difference_VIGOROUS_ACTIVITY_summarysd$Difference_Age_Phenotypic_Age)


# Rename the columns
names(phenotypicage_VIGOROUS_ACTIVITY_summary) <- c("age_deciles", "mean", "se", "sd")



# View the results
print(phenotypicage_Dyslipidemia_summary)
group_variable <- "SMOKING"
svymean(~HEIGHT3 | SMOKING, nhanesDesign)

svymean(~Phenotypic_Age,
        nhanesDesign,
        na.rm = TRUE)




