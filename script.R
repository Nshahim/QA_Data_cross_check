rm(list=ls())
library(tidyverse)
library(readxl)
library(stringr)
library(varhandle)
`%notin%` <- Negate(`%in%`)
options(scipen = 999)
source("R/function.R")

#log -------------------
cleaning_log_link <- ""
cleaned_link <- cleaning_log_link
online_log <- googlesheets4::read_sheet(cleaned_link, sheet = "cleaning_log", col_types = "c")

# read all data files --------------------------------------------------
raw_path <- "input/raw_data/"
# weeks <- list.files(raw_path)
weeks <- c("W16")
raw_data <- list()
for(week in weeks){
  print(week)
  
  for(file in list.files(paste0(raw_path, week), pattern = "*.xlsx")){
    file_name <- paste0(raw_path, week,"/", file)
    print(file_name)
    raw_data[[week]][[str_remove(file, ".xlsx")]] <- read_excel_func(file_name) 
    
    # if(file %in% "CPI_Border_Count_of_Transport_Traffic_Dataset.xlsx"){
    #   raw_data[[week]][[str_remove(file, ".xlsx")]][["Traffic_count"]] <- convert_ton_kg(raw_data[[week]][[str_remove(file, ".xlsx")]][["Traffic_count"]])
    # }
  }
}

revised_path <- "input/QAed_data/"
revised_data <- list()
for(week in weeks){
  print(week)
  
  for(file in list.files(paste0(revised_path, week), pattern = "*.xlsx")){
    file_name <- paste0(revised_path, week,"/", file)
    print(file_name)
    
    revised_data[[week]][[str_remove(file, ".xlsx")]] <- read_excel_func(file_name) 
  }
}


#print different column names -------------------------------------
for(week in ls(raw_data)){
  print(week)
  for(dataset in ls(raw_data[[week]])){
    for(sheet in ls(raw_data[[week]][[dataset]])){
      diff_cols <- dplyr::setdiff(names(raw_data[[week]][[dataset]][[sheet]]), names(revised_data[[week]][[dataset]][[sheet]]))
      if(length(diff_cols) > 0){
        cat("Following columns are missing in [",dataset, "] in sheet[", sheet, "]:\n")
        # print(paste0("Following columns are missing in [",dataset, "] sheet ", sheet))
        print(diff_cols)
      }
      
    }
  }
}

# convert date columns to character format in main sheet of each form in every week --------------------------------------------------
date_columns <- c("SubmissionDate", "Starttime", "Endtime", "Date_And_Time")
for (weeks in names(raw_data)) {
  print(glue::glue("# week: {weeks}"))
  for(dataset in names(raw_data[[weeks]])){
    raw_data[[weeks]][[dataset]][[1]] <- dates_to_character(raw_data[[weeks]][[dataset]][[1]], all_of(date_columns))
  }
}

w47_log <- generate_log(raw_data[["W47"]], revised_data[["W47"]])
w48_log <- generate_log(raw_data[["W48"]], revised_data[["W48"]])
w49_log <- generate_log(raw_data[["W49"]], revised_data[["W49"]])
w50_log <- generate_log(raw_data[["W50"]], revised_data[["W50"]])
w51_log <- generate_log(raw_data[["W51"]], revised_data[["W51"]])
w52_log <- generate_log(raw_data[["W52"]], revised_data[["W52"]])
w1_log <- generate_log(raw_data[["W1"]], revised_data[["W1"]])
w2_log <- generate_log(raw_data[["W2"]], revised_data[["W2"]])
w3_log <- generate_log(raw_data[["W3"]], revised_data[["W3"]])
w4_log <- generate_log(raw_data[["W4"]], revised_data[["W4"]])
w5_log <- generate_log(raw_data[["W5"]], revised_data[["W5"]])
w6_log <- generate_log(raw_data[["W6"]], revised_data[["W6"]])
w7_log <- generate_log(raw_data[["W7"]], revised_data[["W7"]])
w8_log <- generate_log(raw_data[["W8"]], revised_data[["W8"]])
w9_log <- generate_log(raw_data[["W9"]], revised_data[["W9"]])
w10_log <- generate_log(raw_data[["W10"]], revised_data[["W10"]])
w11_log <- generate_log(raw_data[["W11"]], revised_data[["W11"]])
w12_log <- generate_log(raw_data[["W12"]], revised_data[["W12"]])
w13_log <- generate_log(raw_data[["W13"]], revised_data[["W13"]])
w15_log <- generate_log(raw_data[["W15"]], revised_data[["W15"]])
w16_log <- generate_log(raw_data[["W16"]], revised_data[["W16"]])


CPI_weekly_log <- list(
  w47_log=w47_log,
  w48_log=w48_log,
  w49_log=w49_log,
  w50_log=w50_log,
  w51_log=w51_log,
  w52_log=w52_log,
  w1_log=w1_log,
  w2_log=w2_log,
  w3_log=w3_log,
  w4_log=w4_log,
  w5_log=w5_log,
  w6_log=w6_log,
  w7_log=w7_log
)

writexl::write_xlsx(w15_log, paste0("output/QA_revised_log/CPI_QA_revised_log_W15_",lubridate::today(),".xlsx"))

#Test --------------------------
