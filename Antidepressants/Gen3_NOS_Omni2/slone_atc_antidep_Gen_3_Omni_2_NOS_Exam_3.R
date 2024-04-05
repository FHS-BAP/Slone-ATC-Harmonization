# ******************************************************************************************************************************************
# Introduction to Gen 3/Omni 2/NOS Exam 3 Antidepressants drug usage ATC/Slone conversion source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: April 2023
# 
# 
# The purpose of this R code is to convert the antidepressants-related ATC codes in the Gen 3/Omni 2/NOS Exam 3 Exam 3 ATC dataset
# to Slone codes using a conversion file. Only ATC codes regarding antidepressant usage are to be converted here.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Antidepressants ATC/Slone information 
# ATC_Codes1_with_edits.xlsx
# 
# 2)  Full ATC medication info for patients
# vr_meds_ex03_3b_1071_v1.sas7bdat (Combined Gen 3 Exam 3, NOS Exam 3, Omni 2 Exam 3)
# 
# 
# *Provide the location of input and output datasets for setwd() before you run the R code.
# setwd("/path/goes/here")

#Set working directory for all input and output files
#setwd("/path/goes/here")
library(haven) #library for reading .sas7bdat files
library(tidyverse) #improves R data functionality
library(readxl) #reading excel files

#Read ATC/Slone match list
#This is a specific sheet containing Antidepressants values only, starting with N06.
#It has also had duplicates removed so there is only one Slone code for each ATC value.
#The rows with multiple values are in the N06_Antidep_Duplicated_Values sheet.
antidep_atc_slone <- read_excel("ATC_Codes1.xlsx", sheet = "N06_Antidep")
#values of antidepressant ATC codes only
antidep_atc_list <- antidep_atc_slone$ATC_Code

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

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
antidep_filtered_03_3b <- cod_03_3b %>% filter_all(any_vars(. %in% antidep_atc_list))


#Match the three ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
antidep_atc_slone_1 <- antidep_atc_slone #create separate columns for merge
colnames(antidep_atc_slone_1) <- c("atc_cod1", "MEDICATION1", "SloneCode1" ,"Slone_Label1") #rename for uniqueness
antidep_03_3b_join_1 <- left_join(antidep_filtered_03_3b, antidep_atc_slone_1, by = "atc_cod1") #carry out join

#match second ATC column
antidep_atc_slone_2 <- antidep_atc_slone #create separate columns for merge
colnames(antidep_atc_slone_2) <- c("atc_cod2", "MEDICATION2", "SloneCode2" ,"Slone_Label2") #rename for uniqueness
antidep_03_3b_join_2 <- left_join(antidep_03_3b_join_1, antidep_atc_slone_2, by = "atc_cod2") #carry out merge

#match third ATC column
antidep_atc_slone_3 <- antidep_atc_slone #create separate columns for merge
colnames(antidep_atc_slone_3) <- c("atc_cod3", "MEDICATION3", "SloneCode3" ,"Slone_Label3") #rename for uniqueness
antidep_03_3b_join_3 <- left_join(antidep_03_3b_join_2, antidep_atc_slone_3, by = "atc_cod3") #carry out merge


#write final CSV to file
write.csv(antidep_03_3b_join_3, file = "Slone_ATC_Antidepressants_Gen_3_Omni_2_NOS_Exam_3.csv", row.names = FALSE)

