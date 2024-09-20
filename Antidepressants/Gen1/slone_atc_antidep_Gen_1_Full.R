# ******************************************************************************************************************************************
# Introduction to Gen 1 Exam 28-32 Antidepressants drug usage ATC/Slone conversion source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: May 2024
# 
# 
# The purpose of this R code is to convert the antidepressants ATC codes in the Gen 1 Exam 28-32 ATC datasets
# to Slone codes using a conversion file. Only ATC codes regarding antidepressants usage are to be converted here.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Antidepressants ATC/Slone information 
# Antidep_For_Match.xlsx
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
#This is a specific sheet containing antidepressants values only.
#It has also had duplicates removed so there is only one Slone code for each ATC value.
#The rows with multiple values are in the Reassign_atc_cod sheet.
antidep_a_s_single <- read_excel("Antidep_For_Match.xlsx", sheet = "Antidep_Single")
antidep_a_s_multiple <- read_excel("Antidep_For_Match.xlsx", sheet = "Antidep_Multiple")
#values of antidepressants ATC codes only
antidep_atc_list <- antidep_a_s_single$ATC_Code

#columns for merge, renamed for uniqueness
antidep_a_s_single_1 <- setNames(antidep_a_s_single, c("atc_cod1", "MEDICATION1", "SloneCode1" ,"Slone_Label1"))
antidep_a_s_single_2 <- setNames(antidep_a_s_single, c("atc_cod2", "MEDICATION2", "SloneCode2" ,"Slone_Label2"))
antidep_a_s_single_3 <- setNames(antidep_a_s_single, c("atc_cod3", "MEDICATION3", "SloneCode3" ,"Slone_Label3"))
antidep_a_s_single_4 <- setNames(antidep_a_s_single, c("atc_cod4", "MEDICATION4", "SloneCode4" ,"Slone_Label4"))

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

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
antidep_filtered_28_0 <- cod_28_0 %>% filter_all(any_vars(. %in% antidep_atc_list))

#rows with only one ATC code
antidep_single_28_0 <- antidep_filtered_28_0[nchar(antidep_filtered_28_0$atc_cod2) == 0 
                                             & nchar(antidep_filtered_28_0$atc_cod3) == 0
                                             & nchar(antidep_filtered_28_0$atc_cod4) == 0, ]

#rows with multiple ATC codes
antidep_multiple_28_0 <- antidep_filtered_28_0[nchar(antidep_filtered_28_0$atc_cod2) > 0 
                                               | nchar(antidep_filtered_28_0$atc_cod3) > 0
                                               | nchar(antidep_filtered_28_0$atc_cod4) > 0, ]

#Match multiple ATCs to one Slone
antidep_28_0_join_multiple <- inner_join(antidep_multiple_28_0, antidep_a_s_multiple, by = c("atc_cod1", "atc_cod2"))

#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
antidep_28_0_join_1 <- left_join(antidep_single_28_0, antidep_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
antidep_28_0_join_2 <- left_join(antidep_28_0_join_1, antidep_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
antidep_28_0_join_3 <- left_join(antidep_28_0_join_2, antidep_a_s_single_3, by = "atc_cod3") #carry out merge
#match fourth ATC column
antidep_28_0_join_4 <- left_join(antidep_28_0_join_3, antidep_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
antidep_28_0_bind <- bind_rows(antidep_28_0_join_4, antidep_28_0_join_multiple)

#reorder rows
antidep_28_0_ordered <- antidep_28_0_bind %>% arrange(framid)

#add exam number
antidep_28_0_ordered$exam_num <- 28
antidep_28_0_final <- antidep_28_0_ordered %>% relocate(exam_num, .before = id)


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

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
antidep_filtered_29_0 <- cod_29_0 %>% filter_all(any_vars(. %in% antidep_atc_list))

#rows with only one ATC code
antidep_single_29_0 <- antidep_filtered_29_0[nchar(antidep_filtered_29_0$atc_cod2) == 0 
                                             & nchar(antidep_filtered_29_0$atc_cod3) == 0
                                             & nchar(antidep_filtered_29_0$atc_cod4) == 0, ]

#rows with multiple ATC codes
antidep_multiple_29_0 <- antidep_filtered_29_0[nchar(antidep_filtered_29_0$atc_cod2) > 0 
                                               | nchar(antidep_filtered_29_0$atc_cod3) > 0
                                               | nchar(antidep_filtered_29_0$atc_cod4) > 0, ]

#Match multiple ATCs to one Slone
antidep_29_0_join_multiple <- inner_join(antidep_multiple_29_0, antidep_a_s_multiple, by = c("atc_cod1", "atc_cod2"))

#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
antidep_29_0_join_1 <- left_join(antidep_single_29_0, antidep_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
antidep_29_0_join_2 <- left_join(antidep_29_0_join_1, antidep_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
antidep_29_0_join_3 <- left_join(antidep_29_0_join_2, antidep_a_s_single_3, by = "atc_cod3") #carry out merge
#match fourth ATC column
antidep_29_0_join_4 <- left_join(antidep_29_0_join_3, antidep_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
antidep_29_0_bind <- bind_rows(antidep_29_0_join_4, antidep_29_0_join_multiple)

#reorder rows
antidep_29_0_ordered <- antidep_29_0_bind %>% arrange(framid)

#add exam number
antidep_29_0_ordered$exam_num <- 29
antidep_29_0_final <- antidep_29_0_ordered %>% relocate(exam_num, .before = id)


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

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
antidep_filtered_30_0 <- cod_30_0 %>% filter_all(any_vars(. %in% antidep_atc_list))

#rows with only one ATC code
antidep_single_30_0 <- antidep_filtered_30_0[nchar(antidep_filtered_30_0$atc_cod2) == 0 
                                             & nchar(antidep_filtered_30_0$atc_cod3) == 0
                                             & nchar(antidep_filtered_30_0$atc_cod4) == 0, ]

#rows with multiple ATC codes
antidep_multiple_30_0 <- antidep_filtered_30_0[nchar(antidep_filtered_30_0$atc_cod2) > 0 
                                               | nchar(antidep_filtered_30_0$atc_cod3) > 0
                                               | nchar(antidep_filtered_30_0$atc_cod4) > 0, ]

#Match multiple ATCs to one Slone
antidep_30_0_join_multiple <- inner_join(antidep_multiple_30_0, antidep_a_s_multiple, by = c("atc_cod1", "atc_cod2"))

#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
antidep_30_0_join_1 <- left_join(antidep_single_30_0, antidep_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
antidep_30_0_join_2 <- left_join(antidep_30_0_join_1, antidep_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
antidep_30_0_join_3 <- left_join(antidep_30_0_join_2, antidep_a_s_single_3, by = "atc_cod3") #carry out merge
#match fourth ATC column
antidep_30_0_join_4 <- left_join(antidep_30_0_join_3, antidep_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
antidep_30_0_bind <- bind_rows(antidep_30_0_join_4, antidep_30_0_join_multiple)

#reorder rows
antidep_30_0_ordered <- antidep_30_0_bind %>% arrange(framid)

#add exam number
antidep_30_0_ordered$exam_num <- 30
antidep_30_0_final <- antidep_30_0_ordered %>% relocate(exam_num, .before = id)


#### Exam 31 ####
#medications from exam 31
#split 29/31/31 medications file
meds_31_0 <- subset(meds_29_30_31, exam == 31)

#There are four columns of ATC values
cod_31_0 <- meds_31_0[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#create framid column
cod_31_0$framid <- cod_31_0$id
#relocate framid column
cod_31_0 <- cod_31_0 %>% relocate(framid, .after = idtype)

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
antidep_filtered_31_0 <- cod_31_0 %>% filter_all(any_vars(. %in% antidep_atc_list))

#rows with only one ATC code
antidep_single_31_0 <- antidep_filtered_31_0[nchar(antidep_filtered_31_0$atc_cod2) == 0 
                                             & nchar(antidep_filtered_31_0$atc_cod3) == 0
                                             & nchar(antidep_filtered_31_0$atc_cod4) == 0, ]

#rows with multiple ATC codes
antidep_multiple_31_0 <- antidep_filtered_31_0[nchar(antidep_filtered_31_0$atc_cod2) > 0 
                                               | nchar(antidep_filtered_31_0$atc_cod3) > 0
                                               | nchar(antidep_filtered_31_0$atc_cod4) > 0, ]

#Match multiple ATCs to one Slone
antidep_31_0_join_multiple <- inner_join(antidep_multiple_31_0, antidep_a_s_multiple, by = c("atc_cod1", "atc_cod2"))

#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
antidep_31_0_join_1 <- left_join(antidep_single_31_0, antidep_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
antidep_31_0_join_2 <- left_join(antidep_31_0_join_1, antidep_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
antidep_31_0_join_3 <- left_join(antidep_31_0_join_2, antidep_a_s_single_3, by = "atc_cod3") #carry out merge
#match fourth ATC column
antidep_31_0_join_4 <- left_join(antidep_31_0_join_3, antidep_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
antidep_31_0_bind <- bind_rows(antidep_31_0_join_4, antidep_31_0_join_multiple)

#reorder rows
antidep_31_0_ordered <- antidep_31_0_bind %>% arrange(framid)

#add exam number
antidep_31_0_ordered$exam_num <- 31
antidep_31_0_final <- antidep_31_0_ordered %>% relocate(exam_num, .before = id)


#### Exam 32 ####

#medications from Gen 1 exam 32
meds_32_0 <- read_sas("vr_meds_ex32_0_0880.sas7bdat")
#There are four columns of ATC values
cod_32_0 <- meds_32_0[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#create framid column
cod_32_0$framid <- cod_32_0$id
#relocate framid column
cod_32_0 <- cod_32_0 %>% relocate(framid, .after = idtype)

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
antidep_filtered_32_0 <- cod_32_0 %>% filter_all(any_vars(. %in% antidep_atc_list))

#rows with only one ATC code
antidep_single_32_0 <- antidep_filtered_32_0[nchar(antidep_filtered_32_0$atc_cod2) == 0 
                                             & nchar(antidep_filtered_32_0$atc_cod3) == 0
                                             & nchar(antidep_filtered_32_0$atc_cod4) == 0, ]

#rows with multiple ATC codes
antidep_multiple_32_0 <- antidep_filtered_32_0[nchar(antidep_filtered_32_0$atc_cod2) > 0 
                                               | nchar(antidep_filtered_32_0$atc_cod3) > 0
                                               | nchar(antidep_filtered_32_0$atc_cod4) > 0, ]

#Match multiple ATCs to one Slone
antidep_32_0_join_multiple <- inner_join(antidep_multiple_32_0, antidep_a_s_multiple, by = c("atc_cod1", "atc_cod2"))

#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
antidep_32_0_join_1 <- left_join(antidep_single_32_0, antidep_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
antidep_32_0_join_2 <- left_join(antidep_32_0_join_1, antidep_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
antidep_32_0_join_3 <- left_join(antidep_32_0_join_2, antidep_a_s_single_3, by = "atc_cod3") #carry out merge
#match fourth ATC column
antidep_32_0_join_4 <- left_join(antidep_32_0_join_3, antidep_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
antidep_32_0_bind <- bind_rows(antidep_32_0_join_4, antidep_32_0_join_multiple)

#reorder rows
antidep_32_0_ordered <- antidep_32_0_bind %>% arrange(framid)

#add exam number
antidep_32_0_ordered$exam_num <- 32
antidep_32_0_final <- antidep_32_0_ordered %>% relocate(exam_num, .before = id)


#### Final Section ####

#bind data frames for each exam
antidep_full_final <- bind_rows(antidep_28_0_final, antidep_29_0_final, antidep_30_0_final,
                              antidep_31_0_final, antidep_32_0_final)
#write final CSV to file
write.csv(antidep_full_final, file = "Slone_ATC_Antidep_Gen_1_Full.csv", row.names = FALSE)


