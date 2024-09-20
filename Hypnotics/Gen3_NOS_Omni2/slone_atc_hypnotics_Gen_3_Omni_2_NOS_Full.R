# ******************************************************************************************************************************************
# Introduction to Gen 3/Omni 2/NOS Exam 1-3 Hypnotics drug usage ATC/Slone conversion source code
# ******************************************************************************************************************************************
#   
# Created by Michael Cummings
# Last updated: May 2024
# 
# 
# The purpose of this R code is to convert the hypnotics ATC codes in the Gen 3/Omni 2/NOS Exam 3 Exam 1-3 ATC datasets
# to Slone codes using a conversion file. Only ATC codes regarding hypnotics usage are to be converted here.
# 
# Please ensure you have these listed datasets to run this R code optimally. It is highly recommended to have them in the same location.
# 
# Generic names are used for these datasets within this R code.
# Tip: You can copy and paste this R code onto a Word document and use the "find and replace" function to customize your dataset names
# 
# 1)  Hypnotics ATC/Slone information 
# Hypno_For_Match.xlsx
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
#This is a specific sheet containing hypnotics values only.
#It has also had duplicates removed so there is only one Slone code for each ATC value.
#The rows with multiple values are in the Reassign_atc_cod sheet.
hypno_a_s_single <- read_excel("Hypno_For_Match.xlsx", sheet = "Hypno_Single")
hypno_a_s_multiple <- read_excel("Hypno_For_Match.xlsx", sheet = "Hypno_Multiple")
#blank cell replacement
hypno_a_s_multiple[is.na(hypno_a_s_multiple)] <- ""
#values of hypnotics ATC codes only
hypno_atc_list <- hypno_a_s_single$ATC_Code

#columns for merge, renamed for uniqueness
hypno_a_s_single_1 <- setNames(hypno_a_s_single, c("atc_cod1", "MEDICATION1", "SloneCode1" ,"Slone_Label1"))
hypno_a_s_single_2 <- setNames(hypno_a_s_single, c("atc_cod2", "MEDICATION2", "SloneCode2" ,"Slone_Label2"))
hypno_a_s_single_3 <- setNames(hypno_a_s_single, c("atc_cod3", "MEDICATION3", "SloneCode3" ,"Slone_Label3"))
hypno_a_s_single_4 <- setNames(hypno_a_s_single, c("atc_cod4", "MEDICATION4", "SloneCode4" ,"Slone_Label4"))

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

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
hypno_filtered_01_3b <- cod_01_3b %>% filter_all(any_vars(. %in% hypno_atc_list))

#rows with only one ATC code
hypno_single_01_3b <- hypno_filtered_01_3b[nchar(hypno_filtered_01_3b$atc_cod2) == 0 
                                           & nchar(hypno_filtered_01_3b$atc_cod3) == 0
                                           & nchar(hypno_filtered_01_3b$atc_cod4) == 0, ]

#rows with multiple ATC codes
hypno_multiple_01_3b <- hypno_filtered_01_3b[nchar(hypno_filtered_01_3b$atc_cod2) > 0 
                                             | nchar(hypno_filtered_01_3b$atc_cod3) > 0
                                             | nchar(hypno_filtered_01_3b$atc_cod4) > 0, ]

#Match multiple ATCs to one Slone
hypno_01_3b_join_multiple <- inner_join(hypno_multiple_01_3b, hypno_a_s_multiple, by = c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))

#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
hypno_01_3b_join_1 <- left_join(hypno_single_01_3b, hypno_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
hypno_01_3b_join_2 <- left_join(hypno_01_3b_join_1, hypno_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
hypno_01_3b_join_3 <- left_join(hypno_01_3b_join_2, hypno_a_s_single_3, by = "atc_cod3") #carry out merge
#match fourth ATC column
hypno_01_3b_join_4 <- left_join(hypno_01_3b_join_3, hypno_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
hypno_01_3b_bind <- bind_rows(hypno_01_3b_join_4, hypno_01_3b_join_multiple)

#reorder rows
hypno_01_3b_ordered <- hypno_01_3b_bind %>% arrange(framid)

#add exam number
hypno_01_3b_ordered$exam_num <- 1
hypno_01_3b_final <- hypno_01_3b_ordered %>% relocate(exam_num, .before = id)


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

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
hypno_filtered_02_3b <- cod_02_3b %>% filter_all(any_vars(. %in% hypno_atc_list))

#rows with only one ATC code
hypno_single_02_3b <- hypno_filtered_02_3b[nchar(hypno_filtered_02_3b$atc_cod2) == 0 
                                           & nchar(hypno_filtered_02_3b$atc_cod3) == 0
                                           & nchar(hypno_filtered_02_3b$atc_cod4) == 0, ]

#rows with multiple ATC codes
hypno_multiple_02_3b <- hypno_filtered_02_3b[nchar(hypno_filtered_02_3b$atc_cod2) > 0 
                                             | nchar(hypno_filtered_02_3b$atc_cod3) > 0
                                             | nchar(hypno_filtered_02_3b$atc_cod4) > 0, ]

#Match multiple ATCs to one Slone
hypno_02_3b_join_multiple <- inner_join(hypno_multiple_02_3b, hypno_a_s_multiple, by = c("atc_cod1", "atc_cod2", "atc_cod3", "atc_cod4"))

#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
hypno_02_3b_join_1 <- left_join(hypno_single_02_3b, hypno_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
hypno_02_3b_join_2 <- left_join(hypno_02_3b_join_1, hypno_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
hypno_02_3b_join_3 <- left_join(hypno_02_3b_join_2, hypno_a_s_single_3, by = "atc_cod3") #carry out merge
#match fourth ATC column
hypno_02_3b_join_4 <- left_join(hypno_02_3b_join_3, hypno_a_s_single_4, by = "atc_cod4") #carry out merge

#bind single atc and multiple atc rows together
hypno_02_3b_bind <- bind_rows(hypno_02_3b_join_4, hypno_02_3b_join_multiple)

#reorder rows
hypno_02_3b_ordered <- hypno_02_3b_bind %>% arrange(framid)

#add exam number
hypno_02_3b_ordered$exam_num <- 2
hypno_02_3b_final <- hypno_02_3b_ordered %>% relocate(exam_num, .before = id)


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

#Select the rows of individual ATC data containing the specific ATC numbers in the lists for drugs of this type
hypno_filtered_03_3b <- cod_03_3b %>% filter_all(any_vars(. %in% hypno_atc_list))

#rows with only one ATC code
hypno_single_03_3b <- hypno_filtered_03_3b[nchar(hypno_filtered_03_3b$atc_cod2) == 0 
                                           & nchar(hypno_filtered_03_3b$atc_cod3) == 0, ]

#rows with multiple ATC codes
hypno_multiple_03_3b <- hypno_filtered_03_3b[nchar(hypno_filtered_03_3b$atc_cod2) > 0 
                                             | nchar(hypno_filtered_03_3b$atc_cod3) > 0, ]

#Match multiple ATCs to one Slone
hypno_03_3b_join_multiple <- inner_join(hypno_multiple_03_3b, hypno_a_s_multiple, by = c("atc_cod1", "atc_cod2", "atc_cod3"))

#Match the four ATC columns
#In rows without data for that column or with values not in the drug category the merge will go through but produce a blank result
#match first ATC column
hypno_03_3b_join_1 <- left_join(hypno_single_03_3b, hypno_a_s_single_1, by = "atc_cod1") #carry out merge
#match second ATC column
hypno_03_3b_join_2 <- left_join(hypno_03_3b_join_1, hypno_a_s_single_2, by = "atc_cod2") #carry out merge
#match third ATC column
hypno_03_3b_join_3 <- left_join(hypno_03_3b_join_2, hypno_a_s_single_3, by = "atc_cod3") #carry out merge

#bind single atc and multiple atc rows together
hypno_03_3b_bind <- bind_rows(hypno_03_3b_join_3, hypno_03_3b_join_multiple)

#reorder rows
hypno_03_3b_ordered <- hypno_03_3b_bind %>% arrange(framid)

#add exam number
hypno_03_3b_ordered$exam_num <- 3
hypno_03_3b_final <- hypno_03_3b_ordered %>% relocate(exam_num, .before = id)


#### Final Section ####

#bind data frames for each exam
hypno_full_final <- bind_rows(hypno_01_3b_final, hypno_02_3b_final, hypno_03_3b_final)
#write final CSV to file
write.csv(hypno_full_final, file = "Slone_ATC_Hypno_Gen_3_Omni_2_NOS_Full.csv", row.names = FALSE)

