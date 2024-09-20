# ******************************************************************************************************************************************
# Introduction to Gen 3/Omni 2/NOS Exam 1-3 Thyroid-related drug usage ATC/Slone conversion source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: July 2024
# 
# 
# The purpose of this R code is to convert the thyroid-related ATC codes in the Gen 3/Omni 2/NOS Exam 3 Exam 1-3 ATC datasets
# to Slone codes using a conversion file. Only ATC codes regarding thyroid-related usage are to be converted here.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Thyroid-related ATC/Slone information 
# ATC4_Thyroid_Slone_Revised.xlsx
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
#This is a specific sheet containing thyroid-related values only.
#It has also had duplicates removed so there is only one Slone code for each ATC value.
#The rows with multiple values are in the Reassign_atc_cod sheet.
thyroid_a_s_single <- read_excel("ATC4_Thyroid_Slone_Revised.xlsx", sheet = "ATC4_Thyroid")
thyroid_a_s_multiple <- read_excel("ATC4_Thyroid_Slone_Revised.xlsx", sheet = "ATC4_Thyroid2")
#values of thyroid-related ATC codes only
thyroid_atc_list <- thyroid_a_s_single$ATC_Code
#values for replacement
thyroid_a_s_replace <- read_excel("ATC4_Thyroid_Slone_Revised.xlsx", sheet = "ATC4_Thyroid_Replacement")
thyroid_a_s_replace[is.na(thyroid_a_s_replace)] <- ""

#columns for merge, renamed for uniqueness
thyroid_a_s_single_1 <- setNames(thyroid_a_s_single, c("atc_cod1", "MEDICATION1", "SloneCode1" ,"Slone_Label1"))
thyroid_a_s_single_2 <- setNames(thyroid_a_s_single, c("atc_cod2", "MEDICATION2", "SloneCode2" ,"Slone_Label2"))
thyroid_a_s_single_3 <- setNames(thyroid_a_s_single, c("atc_cod3", "MEDICATION3", "SloneCode3" ,"Slone_Label3"))
thyroid_a_s_single_4 <- setNames(thyroid_a_s_single, c("atc_cod4", "MEDICATION4", "SloneCode4" ,"Slone_Label4"))


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

#Join old and replacement
joined_data_28_0 <- left_join(cod_28_0, thyroid_a_s_replace, by = c("medname", "atc_cod1", "atc_cod2"))

#Update certain columns by using coalesce function. Where the new column values exist, they replace the old ones
#This also leaves the ones we don't want replaced alone
joined_data_28_0$medname <- coalesce(joined_data_28_0$New_medname, joined_data_28_0$medname)
joined_data_28_0$atc_cod1 <- coalesce(joined_data_28_0$New_atc_cod1, joined_data_28_0$atc_cod1)
joined_data_28_0$atc_cod2 <- coalesce(joined_data_28_0$New_atc_cod2, joined_data_28_0$atc_cod2)

#Select the updated columns along with the necessary unchanged columns
cod_28_0_updated <- joined_data_28_0[,c("id", "idtype", "framid", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
thyroid_filtered_28_0 <- cod_28_0_updated %>% filter_all(any_vars(. %in% thyroid_atc_list))

#rows with only one ATC code
thyroid_single_28_0 <- thyroid_filtered_28_0[nchar(thyroid_filtered_28_0$atc_cod2) == 0 
                                               & nchar(thyroid_filtered_28_0$atc_cod3) == 0
                                               & nchar(thyroid_filtered_28_0$atc_cod4) == 0, ]

#rows with mutiple ATC codes
thyroid_multiple_28_0 <- thyroid_filtered_28_0[nchar(thyroid_filtered_28_0$atc_cod2) > 0 
                                                 | nchar(thyroid_filtered_28_0$atc_cod3) > 0
                                                 | nchar(thyroid_filtered_28_0$atc_cod4) > 0, ]


#Match multiple ATCs to one Slone
thyroid_28_0_join_multiple <- inner_join(thyroid_multiple_28_0, thyroid_a_s_multiple, by = c("atc_cod1", "atc_cod2"))


#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
thyroid_28_0_join_1 <- left_join(thyroid_single_28_0, thyroid_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
thyroid_28_0_join_2 <- left_join(thyroid_28_0_join_1, thyroid_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
thyroid_28_0_join_3 <- left_join(thyroid_28_0_join_2, thyroid_a_s_single_3, by = "atc_cod3") #carry out merge
#match four ATC column
thyroid_28_0_join_4 <- left_join(thyroid_28_0_join_3, thyroid_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
thyroid_28_0_bind <- bind_rows(thyroid_28_0_join_4, thyroid_28_0_join_multiple)

#reorder rows
thyroid_28_0_ordered <- thyroid_28_0_bind %>% arrange(framid)

#add exam number
thyroid_28_0_ordered$exam_num <- 28
thyroid_28_0_final <- thyroid_28_0_ordered %>% relocate(exam_num, .before = id)


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

#Join old and replacement
joined_data_29_0 <- left_join(cod_29_0, thyroid_a_s_replace, by = c("medname", "atc_cod1", "atc_cod2"))

#Update certain columns by using coalesce function. Where the new column values exist, they replace the old ones
#This also leaves the ones we don't want replaced alone
joined_data_29_0$medname <- coalesce(joined_data_29_0$New_medname, joined_data_29_0$medname)
joined_data_29_0$atc_cod1 <- coalesce(joined_data_29_0$New_atc_cod1, joined_data_29_0$atc_cod1)
joined_data_29_0$atc_cod2 <- coalesce(joined_data_29_0$New_atc_cod2, joined_data_29_0$atc_cod2)

#Select the updated columns along with the necessary unchanged columns
cod_29_0_updated <- joined_data_29_0[,c("id", "idtype", "framid", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
thyroid_filtered_29_0 <- cod_29_0_updated %>% filter_all(any_vars(. %in% thyroid_atc_list))

#rows with only one ATC code
thyroid_single_29_0 <- thyroid_filtered_29_0[nchar(thyroid_filtered_29_0$atc_cod2) == 0 
                                               & nchar(thyroid_filtered_29_0$atc_cod3) == 0
                                               & nchar(thyroid_filtered_29_0$atc_cod4) == 0, ]

#rows with mutiple ATC codes
thyroid_multiple_29_0 <- thyroid_filtered_29_0[nchar(thyroid_filtered_29_0$atc_cod2) > 0 
                                                 | nchar(thyroid_filtered_29_0$atc_cod3) > 0
                                                 | nchar(thyroid_filtered_29_0$atc_cod4) > 0, ]


#Match multiple ATCs to one Slone
thyroid_29_0_join_multiple <- inner_join(thyroid_multiple_29_0, thyroid_a_s_multiple, by = c("atc_cod1", "atc_cod2"))


#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
thyroid_29_0_join_1 <- left_join(thyroid_single_29_0, thyroid_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
thyroid_29_0_join_2 <- left_join(thyroid_29_0_join_1, thyroid_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
thyroid_29_0_join_3 <- left_join(thyroid_29_0_join_2, thyroid_a_s_single_3, by = "atc_cod3") #carry out merge
#match four ATC column
thyroid_29_0_join_4 <- left_join(thyroid_29_0_join_3, thyroid_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
thyroid_29_0_bind <- bind_rows(thyroid_29_0_join_4, thyroid_29_0_join_multiple)

#reorder rows
thyroid_29_0_ordered <- thyroid_29_0_bind %>% arrange(framid)

#add exam number
thyroid_29_0_ordered$exam_num <- 29
thyroid_29_0_final <- thyroid_29_0_ordered %>% relocate(exam_num, .before = id)


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

#Join old and replacement
joined_data_30_0 <- left_join(cod_30_0, thyroid_a_s_replace, by = c("medname", "atc_cod1", "atc_cod2"))

#Update certain columns by using coalesce function. Where the new column values exist, they replace the old ones
#This also leaves the ones we don't want replaced alone
joined_data_30_0$medname <- coalesce(joined_data_30_0$New_medname, joined_data_30_0$medname)
joined_data_30_0$atc_cod1 <- coalesce(joined_data_30_0$New_atc_cod1, joined_data_30_0$atc_cod1)
joined_data_30_0$atc_cod2 <- coalesce(joined_data_30_0$New_atc_cod2, joined_data_30_0$atc_cod2)

#Select the updated columns along with the necessary unchanged columns
cod_30_0_updated <- joined_data_30_0[,c("id", "idtype", "framid", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
thyroid_filtered_30_0 <- cod_30_0_updated %>% filter_all(any_vars(. %in% thyroid_atc_list))

#rows with only one ATC code
thyroid_single_30_0 <- thyroid_filtered_30_0[nchar(thyroid_filtered_30_0$atc_cod2) == 0 
                                               & nchar(thyroid_filtered_30_0$atc_cod3) == 0
                                               & nchar(thyroid_filtered_30_0$atc_cod4) == 0, ]

#rows with mutiple ATC codes
thyroid_multiple_30_0 <- thyroid_filtered_30_0[nchar(thyroid_filtered_30_0$atc_cod2) > 0 
                                                 | nchar(thyroid_filtered_30_0$atc_cod3) > 0
                                                 | nchar(thyroid_filtered_30_0$atc_cod4) > 0, ]


#Match multiple ATCs to one Slone
thyroid_30_0_join_multiple <- inner_join(thyroid_multiple_30_0, thyroid_a_s_multiple, by = c("atc_cod1", "atc_cod2"))


#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
thyroid_30_0_join_1 <- left_join(thyroid_single_30_0, thyroid_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
thyroid_30_0_join_2 <- left_join(thyroid_30_0_join_1, thyroid_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
thyroid_30_0_join_3 <- left_join(thyroid_30_0_join_2, thyroid_a_s_single_3, by = "atc_cod3") #carry out merge
#match four ATC column
thyroid_30_0_join_4 <- left_join(thyroid_30_0_join_3, thyroid_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
thyroid_30_0_bind <- bind_rows(thyroid_30_0_join_4, thyroid_30_0_join_multiple)

#reorder rows
thyroid_30_0_ordered <- thyroid_30_0_bind %>% arrange(framid)

#add exam number
thyroid_30_0_ordered$exam_num <- 30
thyroid_30_0_final <- thyroid_30_0_ordered %>% relocate(exam_num, .before = id)


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

#Join old and replacement
joined_data_31_0 <- left_join(cod_31_0, thyroid_a_s_replace, by = c("medname", "atc_cod1", "atc_cod2"))

#Update certain columns by using coalesce function. Where the new column values exist, they replace the old ones
#This also leaves the ones we don't want replaced alone
joined_data_31_0$medname <- coalesce(joined_data_31_0$New_medname, joined_data_31_0$medname)
joined_data_31_0$atc_cod1 <- coalesce(joined_data_31_0$New_atc_cod1, joined_data_31_0$atc_cod1)
joined_data_31_0$atc_cod2 <- coalesce(joined_data_31_0$New_atc_cod2, joined_data_31_0$atc_cod2)

#Select the updated columns along with the necessary unchanged columns
cod_31_0_updated <- joined_data_31_0[,c("id", "idtype", "framid", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
thyroid_filtered_31_0 <- cod_31_0_updated %>% filter_all(any_vars(. %in% thyroid_atc_list))

#rows with only one ATC code
thyroid_single_31_0 <- thyroid_filtered_31_0[nchar(thyroid_filtered_31_0$atc_cod2) == 0 
                                               & nchar(thyroid_filtered_31_0$atc_cod3) == 0
                                               & nchar(thyroid_filtered_31_0$atc_cod4) == 0, ]

#rows with mutiple ATC codes
thyroid_multiple_31_0 <- thyroid_filtered_31_0[nchar(thyroid_filtered_31_0$atc_cod2) > 0 
                                                 | nchar(thyroid_filtered_31_0$atc_cod3) > 0
                                                 | nchar(thyroid_filtered_31_0$atc_cod4) > 0, ]


#Match multiple ATCs to one Slone
thyroid_31_0_join_multiple <- inner_join(thyroid_multiple_31_0, thyroid_a_s_multiple, by = c("atc_cod1", "atc_cod2"))


#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
thyroid_31_0_join_1 <- left_join(thyroid_single_31_0, thyroid_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
thyroid_31_0_join_2 <- left_join(thyroid_31_0_join_1, thyroid_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
thyroid_31_0_join_3 <- left_join(thyroid_31_0_join_2, thyroid_a_s_single_3, by = "atc_cod3") #carry out merge
#match four ATC column
thyroid_31_0_join_4 <- left_join(thyroid_31_0_join_3, thyroid_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
thyroid_31_0_bind <- bind_rows(thyroid_31_0_join_4, thyroid_31_0_join_multiple)

#reorder rows
thyroid_31_0_ordered <- thyroid_31_0_bind %>% arrange(framid)

#add exam number
thyroid_31_0_ordered$exam_num <- 31
thyroid_31_0_final <- thyroid_31_0_ordered %>% relocate(exam_num, .before = id)



#### Exam 32 ####

#medications from Gen 1 exam 32
meds_32_0 <- read_sas("vr_meds_ex32_0_0880.sas7bdat")
#There are four columns of ATC values
cod_32_0 <- meds_32_0[,c("id", "idtype", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#create framid column
cod_32_0$framid <- cod_32_0$id

#relocate framid column
cod_32_0 <- cod_32_0 %>% relocate(framid, .after = idtype)

#Join old and replacement
joined_data_32_0 <- left_join(cod_32_0, thyroid_a_s_replace, by = c("medname", "atc_cod1", "atc_cod2"))

#Update certain columns by using coalesce function. Where the new column values exist, they replace the old ones
#This also leaves the ones we don't want replaced alone
joined_data_32_0$medname <- coalesce(joined_data_32_0$New_medname, joined_data_32_0$medname)
joined_data_32_0$atc_cod1 <- coalesce(joined_data_32_0$New_atc_cod1, joined_data_32_0$atc_cod1)
joined_data_32_0$atc_cod2 <- coalesce(joined_data_32_0$New_atc_cod2, joined_data_32_0$atc_cod2)

#Select the updated columns along with the necessary unchanged columns
cod_32_0_updated <- joined_data_32_0[,c("id", "idtype", "framid", "medname", "atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4")]

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
thyroid_filtered_32_0 <- cod_32_0_updated %>% filter_all(any_vars(. %in% thyroid_atc_list))

#rows with only one ATC code
thyroid_single_32_0 <- thyroid_filtered_32_0[nchar(thyroid_filtered_32_0$atc_cod2) == 0 
                                               & nchar(thyroid_filtered_32_0$atc_cod3) == 0
                                               & nchar(thyroid_filtered_32_0$atc_cod4) == 0, ]

#rows with mutiple ATC codes
thyroid_multiple_32_0 <- thyroid_filtered_32_0[nchar(thyroid_filtered_32_0$atc_cod2) > 0 
                                                 | nchar(thyroid_filtered_32_0$atc_cod3) > 0
                                                 | nchar(thyroid_filtered_32_0$atc_cod4) > 0, ]


#Match multiple ATCs to one Slone
thyroid_32_0_join_multiple <- inner_join(thyroid_multiple_32_0, thyroid_a_s_multiple, by = c("atc_cod1", "atc_cod2"))


#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
thyroid_32_0_join_1 <- left_join(thyroid_single_32_0, thyroid_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
thyroid_32_0_join_2 <- left_join(thyroid_32_0_join_1, thyroid_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
thyroid_32_0_join_3 <- left_join(thyroid_32_0_join_2, thyroid_a_s_single_3, by = "atc_cod3") #carry out merge
#match four ATC column
thyroid_32_0_join_4 <- left_join(thyroid_32_0_join_3, thyroid_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
thyroid_32_0_bind <- bind_rows(thyroid_32_0_join_4, thyroid_32_0_join_multiple)

#reorder rows
thyroid_32_0_ordered <- thyroid_32_0_bind %>% arrange(framid)

#add exam number
thyroid_32_0_ordered$exam_num <- 32
thyroid_32_0_final <- thyroid_32_0_ordered %>% relocate(exam_num, .before = id)

#### Final Section ####

#bind data frames for each exam
thyroid_full_final <- bind_rows(thyroid_28_0_final, thyroid_29_0_final, thyroid_30_0_final,
                                thyroid_31_0_final, thyroid_32_0_final)
#write final CSV to file
write.csv(thyroid_full_final, file = "Slone_ATC_Thyroid_Gen_1_Full.csv", row.names = FALSE)


