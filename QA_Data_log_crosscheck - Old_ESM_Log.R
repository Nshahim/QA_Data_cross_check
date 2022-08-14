rm(list=ls())
library(dplyr)
library(readxl)
`%notin%` <- Negate(`%in%`)
options(scipen = 999)

#log -------------------
# #online log
googlesheets4::gs4_deauth()
cleaned_link <- ""
online_log <- googlesheets4::read_sheet(cleaned_link, sheet = "cleaning_log", col_types = "c")
online_log_yes <- online_log %>% filter(changed %in% "Yes")
online_log_no <- online_log %>% filter(changed %in% "No")
additional_log <- googlesheets4::read_sheet(cleaned_link, sheet = "Additional_log", col_types = "c")

#QA log
qa_log <- read_excel("input/QA correction Log/ESM_Dataset_QA_Log_Cumulative.xlsx")

qa_log <- qa_log %>% 
  rename(dataset_name=DATASET,
         sheet_name=TAB,
         uuid=KEY,
         question=COLUMN,
         old_value=ORIGINAL_VALUE,
         new_value=CORRECT_VALUE,
         week=REVISED_WEEK_NUM2) %>% 
  mutate(dataset_name = case_when(
    dataset_name %in% "MARKETSERV" ~ "CPI_Market_Services_Dataset",
    dataset_name %in% "MSFOOD" ~ "CPI_Market_FI_Dataset",
    dataset_name %in% "MSNONFOOD" ~ "CPI_Market_NFI_Dataset",
    dataset_name %in% "BANK" ~ "CPI_Bank_Dataset",
    dataset_name %in% "BCTT" ~ "CPI_Border_Count_of_Transport_Traffic_Dataset",
    dataset_name %in% "MSINFEX" ~ "CPI_Market_IME_Hawala_Dataset",
    dataset_name %in% "BANK OPERATIONALITY" ~ "CPI_Bank_Operationality_Status_Dataset",
    dataset_name %in% "MARKETSERV" ~ "CPI_Market_Services_Dataset",
    TRUE ~ dataset_name
  ), `DM Comment` = "Based on integrity correction log", changed="Yes", question_type="", Item_name="") %>% 
  select(question, question_type, Item_name, old_value, new_value, uuid, changed, 
         dataset_name, sheet_name, week, `DM Comment`, CORRECTION_TYPE)

#Check to see if spellings are the same 
unique(qa_log$CORRECTION_TYPE)

qa_log_outlier <- qa_log %>% 
  filter(CORRECTION_TYPE %in% "Outlier")

# qa_log_fixed <- qa_log %>% 
#   filter(CORRECTION_TYPE %in% c("Incorrect value", "Missing value", "Superfluous value", "Value corrected", "missing value", "Outlier") & question %notin% "[OUTLIER]")

qa_log_fixed <- qa_log %>% 
  filter(question %notin% "[OUTLIER]")

new_log <- anti_join(qa_log_fixed, online_log, by=c("uuid", "question", "new_value", "changed"))
new_log <- anti_join(new_log, additional_log, by=c("uuid", "question", "new_value", "changed"))

writexl::write_xlsx(new_log, paste0("output/Integrity_correction_log/integrity_correction_log_",lubridate::today(),".xlsx"))
writexl::write_xlsx(qa_log_outlier, paste0("output/Integrity_correction_log/integrity_correction_log_Outliers_",lubridate::today(),".xlsx"))



#Cleaning Cleaning Log
duplicated_logs <- janitor::get_dupes(online_log, c("uuid", "question"))
