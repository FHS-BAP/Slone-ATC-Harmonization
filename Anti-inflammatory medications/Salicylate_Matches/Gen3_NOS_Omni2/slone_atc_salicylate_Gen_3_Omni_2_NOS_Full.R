# ******************************************************************************************************************************************
# Introduction to Gen 3/Omni 2/NOS Exam 1-3 Salicylates drug usage ATC/Slone conversion source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: September 2024
# 
# 
# The purpose of this R code is to convert the salicylate ATC codes in the Gen 3/Omni 2/NOS Exam 3 Exam 1-3 ATC datasets
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
# vr_meds_ex01_3_0242.sas7bdat (Gen 3 Exam 1)
# vr_meds_ex01_3b_0825_v1.sas7bdat (Combined NOS Exam 1 and Omni 2 Exam 1)
# vr_meds_2011_m_0675.sas7bdat (Combined Gen 3 Exam 2, NOS Exam 2, Omni 2 Exam 2)
# vr_meds_ex03_3b_1071_v1.sas7bdat (Combined Gen 3 Exam 3, NOS Exam 3, Omni 2 Exam 3)
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



#### Exam 1 ####

#medications from Gen 3 Exam 1
meds_01_3 <- read_sas("vr_meds_ex01_3_0242.sas7bdat")
#four columns of ATC values
cod_01_3 <- meds_01_3[,c("ID", "IDTYPE", "MEDNAME", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#medications from NOS Exam 1 and Omni 2 Exam 1
meds_01_2_72 <- read_sas("vr_meds_ex01_3b_0825_v1.sas7bdat")
#four columns of ATC values
cod_01_2_72 <- meds_01_2_72[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Combine medication sections into one data frame
colnames(cod_01_3) <- colnames(cod_01_2_72)
cod_01_3b <- do.call("rbind", list(cod_01_3, cod_01_2_72))
#create framid column
cod_01_3b$framid <- with(cod_01_3b, ifelse(idtype == 3, 30000 + id, 
                                           ifelse(idtype == 2, 20000 + id, 
                                                  ifelse(idtype == 72, 720000 + id, id))))

#relocate framid column
cod_01_3b <- cod_01_3b %>% relocate(framid, .after = idtype)

#Join ATC and Slone
salicylate_01_3b_joined <- inner_join(cod_01_3b, salicylate_a_s, by = c("medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))

#remove older atc columns
salicylate_01_3b_corrected <- salicylate_01_3b_joined[ , !(names(salicylate_01_3b_joined) %in% c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))]

#rename to correct columns to just atc
colnames(salicylate_01_3b_corrected)[5:8] <- c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")

#reorder rows
salicylate_01_3b_ordered <- salicylate_01_3b_corrected %>% arrange(framid)

#add exam number
salicylate_01_3b_ordered$exam_num <- 1
salicylate_01_3b_final <- salicylate_01_3b_ordered %>% relocate(exam_num, .before = id)


#### Exam 2 ####

#medications from Gen 3 Exam 2, NOS Exam 2 and Omni 2 Exam 2
meds_02_3b <- read_sas("vr_meds_2011_m_0675.sas7bdat")
#There are four columns of ATC values
cod_02_3b <- meds_02_3b[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#create framid column
cod_02_3b$framid <- with(cod_02_3b, ifelse(idtype == 3, 30000 + id, 
                                           ifelse(idtype == 2, 20000 + id, 
                                                  ifelse(idtype == 72, 720000 + id, id))))

#relocate framid column
cod_02_3b <- cod_02_3b %>% relocate(framid, .after = idtype)

#Join ATC and Slone
salicylate_02_3b_joined <- inner_join(cod_02_3b, salicylate_a_s, by = c("medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))

#remove older atc columns
salicylate_02_3b_corrected <- salicylate_02_3b_joined[ , !(names(salicylate_02_3b_joined) %in% c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))]

#rename to correct columns to just atc
colnames(salicylate_02_3b_corrected)[5:8] <- c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")

#reorder rows
salicylate_02_3b_ordered <- salicylate_02_3b_corrected %>% arrange(framid)

#add exam number
salicylate_02_3b_ordered$exam_num <- 2
salicylate_02_3b_final <- salicylate_02_3b_ordered %>% relocate(exam_num, .before = id)



#### Exam 3 ####

#medications from Gen 3 Exam 3, NOS Exam 3 and Omni 2 Exam 3
meds_03_3b <- read_sas("vr_meds_ex03_3b_1071_v1.sas7bdat")
#There are three columns of ATC values
cod_03_3b <- meds_03_3b[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3")]

#create framid column
cod_03_3b$framid <- with(cod_03_3b, ifelse(idtype == 3, 30000 + id, 
                                           ifelse(idtype == 2, 20000 + id, 
                                                  ifelse(idtype == 72, 720000 + id, id))))

#relocate framid column
cod_03_3b <- cod_03_3b %>% relocate(framid, .after = idtype)

#Join ATC and Slone
salicylate_03_3b_joined <- inner_join(cod_03_3b, salicylate_a_s_4_removed, by = c("medname", "atc_cod1", "atc_cod2", "atc_cod3"))

#remove older atc columns
salicylate_03_3b_corrected <- salicylate_03_3b_joined[ , !(names(salicylate_03_3b_joined) %in% c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))]

#rename to correct columns to just atc
colnames(salicylate_03_3b_corrected)[5:8] <- c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")

#reorder rows
salicylate_03_3b_ordered <- salicylate_03_3b_corrected %>% arrange(framid)

#add exam number
salicylate_03_3b_ordered$exam_num <- 3
salicylate_03_3b_final <- salicylate_03_3b_ordered %>% relocate(exam_num, .before = id)


#### Final Section ####

#bind data frames for each exam
salicylate_full_final <- bind_rows(salicylate_01_3b_final, salicylate_02_3b_final, salicylate_03_3b_final)
#write final CSV to file
write.csv(salicylate_full_final, file = "Slone_ATC_Salicylate_Gen_3_Omni_2_NOS_Full.csv", row.names = FALSE)

