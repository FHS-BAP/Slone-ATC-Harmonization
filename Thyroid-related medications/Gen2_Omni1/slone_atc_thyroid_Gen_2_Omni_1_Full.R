# ******************************************************************************************************************************************
# Introduction to Gen 2/Omni 1 Exam 8-10 Thyroid-related drug usage ATC/Slone conversion source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: April 2024
# 
# 
# The purpose of this R code is to convert the thyroid-related ATC codes in the Gen 2/Omni 1 Exam 8-10 ATC datasets
# to Slone codes using a conversion file. Only ATC codes regarding thyroid-related usage are to be converted here.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Thyroid-related ATC/Slone information 
# ATC_Codes1_with_edits.xlsx
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
#This is a specific sheet containing thyroid-related values only.
#It has also had duplicates removed so there is only one Slone code for each ATC value.
#The rows with multiple values are in the Reassign_atc_cod sheet.
thyroid_a_s_single <- read_excel("ATC4_Thyroid_Slone.xlsx", sheet = "ATC4_Thyroid")
thyroid_a_s_multiple <- read_excel("ATC4_Thyroid_Slone.xlsx", sheet = "ATC4_Thyroid2")
#values of thyroid-related ATC codes only
thyroid_atc_list <- thyroid_a_s_single$ATC_Code

#columns for merge, renamed for uniqueness
thyroid_a_s_single_1 <- setNames(thyroid_a_s_single, c("atc_cod1", "MEDICATION1", "SloneCode1" ,"Slone_Label1"))
thyroid_a_s_single_2 <- setNames(thyroid_a_s_single, c("atc_cod2", "MEDICATION2", "SloneCode2" ,"Slone_Label2"))
thyroid_a_s_single_3 <- setNames(thyroid_a_s_single, c("atc_cod3", "MEDICATION3", "SloneCode3" ,"Slone_Label3"))
thyroid_a_s_single_4 <- setNames(thyroid_a_s_single, c("atc_cod4", "MEDICATION4", "SloneCode4" ,"Slone_Label4"))

#Specific drugs with multiple ATC - specifying them is okay as long as no PII given
thyroid_a_s_armour <-  thyroid_a_s_multiple[2,] #Armour Thyroid
thyroid_a_s_ltyro <-  thyroid_a_s_multiple[3,] #Thyroid & L-Tyrosine


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

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
thyroid_filtered_08_1b <- cod_08_1b %>% filter_all(any_vars(. %in% thyroid_atc_list))

#rows with only one ATC code
thyroid_single_08_1b <- thyroid_filtered_08_1b[nchar(thyroid_filtered_08_1b$atc_cod2) == 0 
                                               & nchar(thyroid_filtered_08_1b$atc_cod3) == 0
                                               & nchar(thyroid_filtered_08_1b$atc_cod4) == 0, ]

#rows with mutiple ATC codes
thyroid_multiple_08_1b <- thyroid_filtered_08_1b[nchar(thyroid_filtered_08_1b$atc_cod2) > 0 
                                                 | nchar(thyroid_filtered_08_1b$atc_cod3) > 0
                                                 | nchar(thyroid_filtered_08_1b$atc_cod4) > 0, ]


#Match multiple ATCs to one Slone - specific drugs mentioned above
thyroid_08_1b_join_armour <- inner_join(thyroid_multiple_08_1b, thyroid_a_s_armour, by = c("atc_cod1", "atc_cod2")) #armour thyroid
thyroid_08_1b_join_ltyro <- inner_join(thyroid_multiple_08_1b, thyroid_a_s_ltyro, by = c("atc_cod1", "atc_cod2")) #Thyroid & L-Tyrosine

thyroid_08_1b_join_multiple <- bind_rows(thyroid_08_1b_join_armour, thyroid_08_1b_join_ltyro)

#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
thyroid_08_1b_join_1 <- left_join(thyroid_single_08_1b, thyroid_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
thyroid_08_1b_join_2 <- left_join(thyroid_08_1b_join_1, thyroid_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
thyroid_08_1b_join_3 <- left_join(thyroid_08_1b_join_2, thyroid_a_s_single_3, by = "atc_cod3") #carry out merge
#match four ATC column
thyroid_08_1b_join_4 <- left_join(thyroid_08_1b_join_3, thyroid_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
thyroid_08_1b_bind <- bind_rows(thyroid_08_1b_join_4, thyroid_08_1b_join_multiple)

#reorder rows
thyroid_08_1b_ordered <- thyroid_08_1b_bind %>% arrange(framid)

#add exam number
thyroid_08_1b_ordered$exam_num <- 8
thyroid_08_1b_final <- thyroid_08_1b_ordered %>% relocate(exam_num, .before = id)


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

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
thyroid_filtered_09_1b <- cod_09_1b %>% filter_all(any_vars(. %in% thyroid_atc_list))

#rows with only one ATC code
thyroid_single_09_1b <- thyroid_filtered_09_1b[nchar(thyroid_filtered_09_1b$atc_cod2) == 0 
                                               & nchar(thyroid_filtered_09_1b$atc_cod3) == 0
                                               & nchar(thyroid_filtered_09_1b$atc_cod4) == 0, ]

#rows with mutiple ATC codes
thyroid_multiple_09_1b <- thyroid_filtered_09_1b[nchar(thyroid_filtered_09_1b$atc_cod2) > 0 
                                                 | nchar(thyroid_filtered_09_1b$atc_cod3) > 0
                                                 | nchar(thyroid_filtered_09_1b$atc_cod4) > 0, ]


#Match multiple ATCs to one Slone - specific drugs mentioned above
thyroid_09_1b_join_multiple <- left_join(thyroid_multiple_09_1b, thyroid_a_s_armour, by = c("atc_cod1", "atc_cod2")) #armour thyroid


#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
thyroid_09_1b_join_1 <- left_join(thyroid_single_09_1b, thyroid_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
thyroid_09_1b_join_2 <- left_join(thyroid_09_1b_join_1, thyroid_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
thyroid_09_1b_join_3 <- left_join(thyroid_09_1b_join_2, thyroid_a_s_single_3, by = "atc_cod3") #carry out merge
#match four ATC column
thyroid_09_1b_join_4 <- left_join(thyroid_09_1b_join_3, thyroid_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
thyroid_09_1b_bind <- bind_rows(thyroid_09_1b_join_4, thyroid_09_1b_join_multiple)

#reorder rows
thyroid_09_1b_ordered <- thyroid_09_1b_bind %>% arrange(framid)

#add exam number
thyroid_09_1b_ordered$exam_num <- 9
thyroid_09_1b_final <- thyroid_09_1b_ordered %>% relocate(exam_num, .before = id)


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

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
thyroid_filtered_10_1b <- cod_10_1b %>% filter_all(any_vars(. %in% thyroid_atc_list))

#rows with only one ATC code
thyroid_single_10_1b <- thyroid_filtered_10_1b[nchar(thyroid_filtered_10_1b$atc_cod2) == 0 
                                               & nchar(thyroid_filtered_10_1b$atc_cod3) == 0, ]

#rows with mutiple ATC codes
thyroid_multiple_10_1b <- thyroid_filtered_10_1b[nchar(thyroid_filtered_10_1b$atc_cod2) > 0 
                                                 | nchar(thyroid_filtered_10_1b$atc_cod3) > 0, ]


#Match multiple ATCs to one Slone - specific drugs mentioned above
thyroid_10_1b_join_multiple <- left_join(thyroid_multiple_10_1b, thyroid_a_s_armour, by = c("atc_cod1", "atc_cod2")) #armour thyroid


#Match the three ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
thyroid_10_1b_join_1 <- left_join(thyroid_single_10_1b, thyroid_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
thyroid_10_1b_join_2 <- left_join(thyroid_10_1b_join_1, thyroid_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
thyroid_10_1b_join_3 <- left_join(thyroid_10_1b_join_2, thyroid_a_s_single_3, by = "atc_cod3") #carry out merge

#bind single atc and multiple atc rows together
thyroid_10_1b_bind <- bind_rows(thyroid_10_1b_join_3, thyroid_10_1b_join_multiple)

#reorder rows
thyroid_10_1b_ordered <- thyroid_10_1b_bind %>% arrange(framid)

#add exam number
thyroid_10_1b_ordered$exam_num <- 10
thyroid_10_1b_final <- thyroid_10_1b_ordered %>% relocate(exam_num, .before = id)


#### Final Section ####
thyroid_full_final <- bind_rows(thyroid_08_1b_final, thyroid_09_1b_final, thyroid_10_1b_final)
#write final CSV to file
write.csv(thyroid_full_final, file = "Slone_ATC_Thyroid_Gen_2_Omni_1_Full.csv", row.names = FALSE)



