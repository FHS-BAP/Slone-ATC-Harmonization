# ******************************************************************************************************************************************
# Introduction to Gen 1 Exam 28-32 Salicylates drug usage ATC/Slone conversion source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: September 2024
# 
# 
# The purpose of this R code is to convert the salicylate ATC codes in the Gen 1 Exam 28-32 ATC datasets
# to Slone codes using a conversion file. Only ATC codes regarding thyroid-related usage are to be converted here.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Salicylates ATC/Slone information 
# ATC3_Salicylate_Edited.xlsx
# 
# 2)  Full ATC medication info for patients
# vr_meds_ex28_0_0441.sas7bdat (ATC Info Exam 28)
# vr_meds_ex31_0_0763.sas7bdat (ATC Info Exams 29/30/31)
# vr_meds_ex32_0_0880.sas7bdat (ATC Info Exam 32)
# 
# 
# *Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#setwd("/path/goes/here")
library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality
library(readxl) #reading excel files

#Read ATC/Slone match list
#This is a specific sheet containing salicylates-related values only.
#All the values, including the multi-values and replacement values, are in here.
salicylate_a_s <- read_excel("ATC3_Salicylate_Edited.xlsx")
salicylate_a_s[, c(3:5, 7:9)][is.na(salicylate_a_s[, c(3:5, 7:9)])] <- ""
#for any datasets that just go up to atc_3, to prevent potential duplication
salicylate_a_s_4_removed <- salicylate_a_s[salicylate_a_s$atc_cod4 == "", ]


#### Exam 28 ####

#medications from Gen 1 exam 28
meds_28_0 <- read_sas("vr_meds_ex28_0_0441.sas7bdat")
#There are four columns of ATC values
cod_28_0 <- meds_28_0[,c("ID", "IDTYPE", "MEDNAME", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]
colnames(cod_28_0)[1:3] <- c("id", "idtype", "medname")

#create framid column
cod_28_0$framid <- cod_28_0$id

#relocate framid column
cod_28_0 <- cod_28_0 %>% relocate(framid, .after = idtype)

#Join ATC and Slone
salicylate_28_0_joined <- inner_join(cod_28_0, salicylate_a_s, by = c("medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))

#remove older atc columns
salicylate_28_0_corrected <- salicylate_28_0_joined[ , !(names(salicylate_28_0_joined) %in% c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))]

#rename to correct columns to just atc
colnames(salicylate_28_0_corrected)[5:8] <- c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")

#reorder rows
salicylate_28_0_ordered <- salicylate_28_0_corrected %>% arrange(framid)

#add exam number
salicylate_28_0_ordered$exam_num <- 28
salicylate_28_0_final <- salicylate_28_0_ordered %>% relocate(exam_num, .before = id)


#### Exam 29 ####
#medications from exam 29
#split 29/30/31 medications file
meds_29_30_31 <- read_sas("vr_meds_ex31_0_0763.sas7bdat")
meds_29_0 <- subset(meds_29_30_31, exam == 29)

#There are four columns of ATC values
cod_29_0 <- meds_29_0[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#create framid column
cod_29_0$framid <- cod_29_0$id

#relocate framid column
cod_29_0 <- cod_29_0 %>% relocate(framid, .after = idtype)

#Join ATC and Slone
salicylate_29_0_joined <- inner_join(cod_29_0, salicylate_a_s, by = c("medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))

#remove older atc columns
salicylate_29_0_corrected <- salicylate_29_0_joined[ , !(names(salicylate_29_0_joined) %in% c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))]

#rename to correct columns to just atc
colnames(salicylate_29_0_corrected)[5:8] <- c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")

#reorder rows
salicylate_29_0_ordered <- salicylate_29_0_corrected %>% arrange(framid)

#add exam number
salicylate_29_0_ordered$exam_num <- 29
salicylate_29_0_final <- salicylate_29_0_ordered %>% relocate(exam_num, .before = id)


#### Exam 30 ####
#medications from exam 30
#split 29/30/31 medications file
meds_30_0 <- subset(meds_29_30_31, exam == 30)

#There are four columns of ATC values
cod_30_0 <- meds_30_0[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#create framid column
cod_30_0$framid <- cod_30_0$id

#relocate framid column
cod_30_0 <- cod_30_0 %>% relocate(framid, .after = idtype)

#Join ATC and Slone
salicylate_30_0_joined <- inner_join(cod_30_0, salicylate_a_s, by = c("medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))

#remove older atc columns
salicylate_30_0_corrected <- salicylate_30_0_joined[ , !(names(salicylate_30_0_joined) %in% c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))]

#rename to correct columns to just atc
colnames(salicylate_30_0_corrected)[5:8] <- c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")

#reorder rows
salicylate_30_0_ordered <- salicylate_30_0_corrected %>% arrange(framid)

#add exam number
salicylate_30_0_ordered$exam_num <- 30
salicylate_30_0_final <- salicylate_30_0_ordered %>% relocate(exam_num, .before = id)


#### Exam 31 ####
#medications from exam 31
#split 29/30/31 medications file
meds_31_0 <- subset(meds_29_30_31, exam == 31)

#There are four columns of ATC values
cod_31_0 <- meds_31_0[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#create framid column
cod_31_0$framid <- cod_31_0$id

#relocate framid column
cod_31_0 <- cod_31_0 %>% relocate(framid, .after = idtype)

#Join ATC and Slone
salicylate_31_0_joined <- inner_join(cod_31_0, salicylate_a_s, by = c("medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))

#remove older atc columns
salicylate_31_0_corrected <- salicylate_31_0_joined[ , !(names(salicylate_31_0_joined) %in% c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))]

#rename to correct columns to just atc
colnames(salicylate_31_0_corrected)[5:8] <- c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")

#reorder rows
salicylate_31_0_ordered <- salicylate_31_0_corrected %>% arrange(framid)

#add exam number
salicylate_31_0_ordered$exam_num <- 31
salicylate_31_0_final <- salicylate_31_0_ordered %>% relocate(exam_num, .before = id)


#### Exam 32 ####

#medications from Gen 1 exam 32
meds_32_0 <- read_sas("vr_meds_ex32_0_0880.sas7bdat")
#There are four columns of ATC values
cod_32_0 <- meds_32_0[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#create framid column
cod_32_0$framid <- cod_32_0$id

#relocate framid column
cod_32_0 <- cod_32_0 %>% relocate(framid, .after = idtype)

#Join ATC and Slone
salicylate_32_0_joined <- inner_join(cod_32_0, salicylate_a_s, by = c("medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))

#remove older atc columns
salicylate_32_0_corrected <- salicylate_32_0_joined[ , !(names(salicylate_32_0_joined) %in% c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))]

#rename to correct columns to just atc
colnames(salicylate_32_0_corrected)[5:8] <- c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")

#reorder rows
salicylate_32_0_ordered <- salicylate_32_0_corrected %>% arrange(framid)

#add exam number
salicylate_32_0_ordered$exam_num <- 32
salicylate_32_0_final <- salicylate_32_0_ordered %>% relocate(exam_num, .before = id)


#### Final Section ####

#bind data frames for each exam
salicylate_full_final <- bind_rows(salicylate_28_0_final, salicylate_29_0_final, salicylate_30_0_final,
                                   salicylate_31_0_final, salicylate_32_0_final)
#write final CSV to file
write.csv(salicylate_full_final, file = "Slone_ATC_Salicylate_Gen_1_Full.csv", row.names = FALSE)


