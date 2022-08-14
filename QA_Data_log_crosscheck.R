rm(list=ls())
library(dplyr)
library(readxl)
`%notin%` <- Negate(`%in%`)
options(scipen = 999)

#Log -------------------
# #online log
googlesheets4::gs4_deauth()
cleaned_link <- ""
online_log <- googlesheets4::read_sheet(cleaned_link, sheet = "cleaning_log", col_types = "c")
# online_log_yes <- online_log %>% filter(changed %in% "Yes")
# online_log_no <- online_log %>% filter(changed %in% "No")
additional_log <- googlesheets4::read_sheet(cleaned_link, sheet = "Additional_log", col_types = "c")
rejection_log <- googlesheets4::read_sheet(cleaned_link, sheet = "rejection_log", col_types = "c") 

#QA log
qa_log_path <- "input/QA correction Log/ECON_WK26-2022_ECON_PMT.xlsx"
qa_log_main <- read_excel(qa_log_path, sheet = "MAIN")
qa_log_price <- read_excel(qa_log_path, sheet = "PRICES")

#Should be ERROR or VEIFIED
unique(qa_log_main$`PMT: Response (ERROR or VERIFIED)`)

qa_log_main <- qa_log_main %>% 
  filter(`PMT: Response (ERROR or VERIFIED)` %in% "ERROR") %>% 
  rename(dataset_name=DATASET,
         sheet_name=TAB,
         uuid=KEY,
         question=`PMT: Corrected Column`,
         old_value=`FLAGGED VALUE`,
         new_value=`PMT: Corrected Value`,
         week=WEEK,
         CORRECTION_TYPE= `QA COMMENT`,
         QA_comment = `PMT: Comment`) %>% 
  mutate(`DM Comment` = "Based on integrity correction log", changed="Yes", question_type="", Item_name="") %>% 
  select(question, question_type, Item_name, old_value, new_value, uuid, changed, 
         dataset_name, sheet_name, week, `DM Comment`, CORRECTION_TYPE, QA_comment)

#Should be ERROR or VEIFIED
unique(qa_log_price$`PMT: Response (ERROR or VERIFIED)`)
qa_log_price <- qa_log_price %>% 
  filter(`PMT: Response (ERROR or VERIFIED)` %in% "ERROR") %>% 
  rename(dataset_name=DATASET,
         sheet_name=TAB,
         Item_name=choice,
         uuid=KEY,
         question=`PMT: Corrected Column`,
         new_value=`PMT: Corrected Value`,
         week=WEEK,
         CORRECTION_TYPE= `QA COMMENT`,
         QA_comment = `PMT: Comment`) %>% 
  mutate(old_value = case_when(
    question %in% c("Unit_FI", "Unit_NFI")  ~ Unit,
    question %in% c("Unit_Amount_FI", "Unit_NFI_Amount") ~ as.character(Unit_Amount),
    TRUE ~ as.character(Price)
  ),
  `DM Comment` = "Based on integrity correction log", changed="Yes", question_type="") %>% 
  select(question, question_type, Item_name, old_value, new_value, uuid, changed, 
         dataset_name, sheet_name, week, `DM Comment`, CORRECTION_TYPE, QA_comment)

qa_log <- rbind(qa_log_main, qa_log_price)

qa_log <- qa_log %>% 
  mutate(dataset_name = case_when(
    dataset_name %in% "MARKETSERV" ~ "CPI_Market_Services_Dataset",
    dataset_name %in% "BANK" ~ "CPI_Bank_Dataset",
    dataset_name %in% "BCTT" ~ "CPI_Border_Count_of_Transport_Traffic_Dataset",
    #all new weeks are IME V2
    dataset_name %in% "MSINFEX" ~ "CPI_Market_IME_Hawala_Dataset_v2",
    dataset_name %in% "BANK OPERATIONALITY" ~ "CPI_Bank_Operationality_Status_Dataset",
    dataset_name %in% c("GOV_SALARIES", "GOV_EMP") ~ "CPI_Government_Employee_Salary_Dataset",
    dataset_name %in% "MSFOOD" ~ "CPI_Market_FI_Dataset",
    dataset_name %in% "MSNONFOOD" ~ "CPI_Market_NFI_Dataset",
    dataset_name %in% "NONFOOD" ~ "CPI_Market_NFI_Dataset",
    TRUE ~ dataset_name
  ),
  new_value = case_when(
    new_value %in% "(blank)" ~ as.character(NA),
    TRUE ~ as.character(new_value)
  ),
  week = paste0("W",week))
#Dataset names should be according to our name conventions
unique(qa_log$dataset_name)

#Check to see if spellings are the same 
unique(qa_log$CORRECTION_TYPE)

# qa_log_outlier <- qa_log %>% 
#   filter(new_value %in% c("[OUTLIER REMOVED]", "Fixed value for all question in column G"))

qa_log_fixed <- qa_log %>% 
  filter(new_value %notin% c("[OUTLIER REMOVED]", "Fixed value for all question in column G"))

new_log <- anti_join(qa_log_fixed, online_log, by=c("uuid", "question", "new_value", "changed"))
new_log <- anti_join(new_log, additional_log, by=c("uuid", "question", "new_value", "changed"))
new_log <- anti_join(new_log, rejection_log, by=c("uuid", "sheet_name", "week"))

output_path <- paste0("output/Integrity_correction_log/integrity_correction_log_W26_",lubridate::today(),".xlsx")
writexl::write_xlsx(new_log, output_path)
# writexl::write_xlsx(qa_log_outlier, paste0("output/Integrity_correction_log/integrity_correction_log_Outliers_",lubridate::today(),".xlsx"))