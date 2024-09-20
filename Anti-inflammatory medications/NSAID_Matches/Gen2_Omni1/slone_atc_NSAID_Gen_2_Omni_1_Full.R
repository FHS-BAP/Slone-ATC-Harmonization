# ******************************************************************************************************************************************
# Introduction to Gen 2/Omni 1 Exam 8-10 NSAIDs drug usage ATC/Slone conversion source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: September 2024
# 
# 
# The purpose of this R code is to convert the NSAID ATC codes in the Gen 2/Omni 1 Exam 8-10 ATC datasets
# to Slone codes using a conversion file. Only ATC codes regarding thyroid-related usage are to be converted here.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  NSAIDs ATC/Slone information 
# ATC3_NSAID_Edited.xlsx.xlsx
# 
# 2)  Full ATC medication info for patients
# vr_meds_ex03_7_0535.sas7bdat (Omni 1 Exam 3)
# vr_meds_ex08_1_0280_v1.sas7bdat (Gen 2 Exam 8)
# vr_meds_ex09_1b_0879.sas7bdat (Combined Gen 2 Exam 9 and Omni 1 Exam 4)
# vr_meds_ex10_1b_1198.sas7bdat (Combined Gen 2 Exam 10 and Omni 1 Exam 5)
# 
# 
# *Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#setwd("/path/goes/here")
library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality
library(readxl) #reading excel files

#Read ATC/Slone match list
#This is a specific sheet containing NSAIDs-related values only.
#All the values, including the multi-values and replacement values, are in here.
NSAID_a_s <- read_excel("ATC3_NSAID_Edited.xlsx")
NSAID_a_s[, c(3:5, 7:9)][is.na(NSAID_a_s[, c(3:5, 7:9)])] <- ""
#for any datasets that just go up to atc_3, to prevent potential duplication
NSAID_a_s_4_removed <- NSAID_a_s[NSAID_a_s$atc_cod4 == "", ]



#### Exam 8 ####
#medications from Gen 3 Exam 8
meds_08_1 <- read_sas("vr_meds_ex08_1_0280_v1.sas7bdat")
#four columns of ATC values
cod_08_1 <- meds_08_1[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#medications from Omni 1 Exam 3
meds_03_7 <- read_sas("vr_meds_ex03_7_0535.sas7bdat")
#four columns of ATC values
cod_03_7 <- meds_03_7[,c("ID", "IDTYPE", "MEDNAME", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Combine medication sections into one data frame
colnames(cod_03_7) <- colnames(cod_08_1)
cod_08_1b <- do.call("rbind", list(cod_08_1, cod_03_7))
#create framid column
cod_08_1b$framid <- with(cod_08_1b, ifelse(idtype == 1, 80000 + id, 
                                           ifelse(idtype == 7, 700000 + id, id)))

#relocate framid column
cod_08_1b <- cod_08_1b %>% relocate(framid, .after = idtype)

#Join ATC and Slone
NSAID_08_1b_joined <- inner_join(cod_08_1b, NSAID_a_s, by = c("medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))

#remove older atc columns
NSAID_08_1b_corrected <- NSAID_08_1b_joined[ , !(names(NSAID_08_1b_joined) %in% c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))]

#rename to correct columns to just atc
colnames(NSAID_08_1b_corrected)[5:8] <- c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")

#reorder rows
NSAID_08_1b_ordered <- NSAID_08_1b_corrected %>% arrange(framid)

#add exam number
NSAID_08_1b_ordered$exam_num <- 8
NSAID_08_1b_final <- NSAID_08_1b_ordered %>% relocate(exam_num, .before = id)


#### Exam 9 ####
#medications from Gen 2 Exam 9 and Omni 1 Exam 4
meds_09_1b <- read_sas("vr_meds_ex09_1b_0879.sas7bdat")
#There are four columns of ATC values
cod_09_1b <- meds_09_1b[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#create framid column
cod_09_1b$framid <- with(cod_09_1b, ifelse(idtype == 1, 80000 + id, 
                                           ifelse(idtype == 7, 700000 + id, id)))

#relocate framid column
cod_09_1b <- cod_09_1b %>% relocate(framid, .after = idtype)

#Join ATC and Slone
NSAID_09_1b_joined <- inner_join(cod_09_1b, NSAID_a_s, by = c("medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))

#remove older atc columns
NSAID_09_1b_corrected <- NSAID_09_1b_joined[ , !(names(NSAID_09_1b_joined) %in% c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))]

#rename to correct columns to just atc
colnames(NSAID_09_1b_corrected)[5:8] <- c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")

#reorder rows
NSAID_09_1b_ordered <- NSAID_09_1b_corrected %>% arrange(framid)

#add exam number
NSAID_09_1b_ordered$exam_num <- 9
NSAID_09_1b_final <- NSAID_09_1b_ordered %>% relocate(exam_num, .before = id)

#### Exam 10 ####
#medications from Gen 2 Exam 10 and Omni 1 Exam 5
meds_10_1b <- read_sas("vr_meds_ex10_1b_1198.sas7bdat")
#There are three columns of ATC values
cod_10_1b <- meds_10_1b[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3")]

#create framid column
cod_10_1b$framid <- with(cod_10_1b, ifelse(idtype == 1, 80000 + id, 
                                           ifelse(idtype == 7, 700000 + id, id)))

#relocate framid column
cod_10_1b <- cod_10_1b %>% relocate(framid, .after = idtype)

#Join ATC and Slone
NSAID_10_1b_joined <- inner_join(cod_10_1b, NSAID_a_s_4_removed, by = c("medname", "atc_cod1", "atc_cod2", "atc_cod3"))

#remove older atc columns
NSAID_10_1b_corrected <- NSAID_10_1b_joined[ , !(names(NSAID_10_1b_joined) %in% c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))]

#rename to correct columns to just atc
colnames(NSAID_10_1b_corrected)[5:8] <- c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")

#reorder rows
NSAID_10_1b_ordered <- NSAID_10_1b_corrected %>% arrange(framid)

#add exam number
NSAID_10_1b_ordered$exam_num <- 10
NSAID_10_1b_final <- NSAID_10_1b_ordered %>% relocate(exam_num, .before = id)


#### Final Section ####
NSAID_full_final <- bind_rows(NSAID_08_1b_final, NSAID_09_1b_final, NSAID_10_1b_final)
#write final CSV to file
write.csv(NSAID_full_final, file = "Slone_ATC_NSAID_Gen_2_Omni_1_Full.csv", row.names = FALSE)


