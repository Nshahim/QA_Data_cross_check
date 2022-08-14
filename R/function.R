read_excel_func <- function(file) {
  file %>% 
    excel_sheets() %>%
    set_names() %>%
    map(read_excel, path = file)
}

dates_to_character <- function(df, ...) {
  mutate(df,
         across(.cols = c(...),
                function(x){
                  x = as.character(x)
                  x = format(x, format= "%Y-%m-%d %I:%M:%S")
                }
         ))
}

generate_log <- function(raw_weekly, revised_weekly){
  log <- data.frame(incon_uuid="", incon_col="", raw_val="", cleaned_val="", dataset_name="", sheet_name="")

  for(dataset in names(raw_weekly)){
    sheet_log <- data.frame()
    for(sheet in names(raw_weekly[[dataset]])){
      raw_sheet <- raw_weekly[[dataset]][[sheet]]
      revised_sheet <- revised_weekly[[dataset]][[sheet]]
      
      sheet_log <- rbind(sheet_log, 
                         cross_check(raw_sheet, revised_sheet, sheet) %>% mutate(dataset_name=dataset, sheet_name=sheet))

    }
    log <- rbind(log, sheet_log)
  }
  log <- log[-1,] %>% 
    mutate(issue = case_when(
      incon_uuid %notin% online_log$uuid ~ "Not logged yet",
      incon_uuid %in% online_log$uuid & incon_col %in% online_log$question[online_log$uuid %in% incon_uuid] ~ "Already Logged!",
      incon_uuid %in% online_log$uuid & incon_col %in% online_log$question[online_log$uuid %in% incon_uuid] & 
        cleaned_val %notin% online_log$new_value[online_log$uuid %in% incon_uuid] ~ "Logged but different new value!",
      incon_uuid %in% online_log$uuid[online_log$changed %in% "No"] ~ "Logged but changed is 'No'",
      TRUE ~ NA_character_
    )) %>% 
    filter(!(sheet_name %in% "subrespondents_available_data" & is.na(raw_val) & cleaned_val %in% "1"))
  
  return(log)
}


cross_check <- function(raw_data, revised_data, sheet){
  cols_list <- names(revised_data)[names(revised_data) %in% names(raw_data) & names(revised_data) %notin% c("KEY", "PARENT_KEY", "KEY_Main", "SET-OF-Currency_Exchangers")]
  
  incon_uuid <- c()
  incon_col <- c()
  cleaned_val <- c()
  raw_val <- c()
  for(col_name in cols_list){
    for(i in 1:nrow(revised_data)){
      uuid <- revised_data$KEY[i]
      revised_val <- revised_data[[col_name]][i]
      old_val <- raw_data[[col_name]][raw_data$KEY %in% uuid]
      
      if(length(old_val) == 0 | length(revised_val) == 0){
        print("The following index did not work:")
        cat("sheet:", sheet,"\n", "Question:", col_name,"\n","KEY:",uuid,"\n","Old_value:", old_val,"\n","Revised_value:", revised_val, "\n")
      } else {
        if(old_val %notin% revised_val){
          
          if(check.numeric(old_val) & check.numeric(revised_val)){
            if(as.numeric(old_val) %notin% as.numeric(revised_val)){
              incon_uuid <- c(incon_uuid, uuid)
              incon_col <- c(incon_col, col_name)
              cleaned_val <- c(cleaned_val, revised_val)
              raw_val <- c(raw_val, old_val)
            }
          } else {
            incon_uuid <- c(incon_uuid, uuid)
            incon_col <- c(incon_col, col_name)
            cleaned_val <- c(cleaned_val, revised_val)
            raw_val <- c(raw_val, old_val)
          }
        }
      }
    }
  }
  
  log <- data.frame(incon_uuid, incon_col, raw_val, cleaned_val)
  date_columns <- c("SubmissionDate", "Starttime", "Endtime", "Date_And_Time")
  log <- log %>% filter(raw_val %notin% "9999" & cleaned_val %notin% "I don't know",
                        raw_val %notin% "8888" & cleaned_val %notin% "Not applicable/we don't have this service",
                        raw_val %notin% "7777" & cleaned_val %notin% "Unlimited")
  # incon_col %notin% date_columns
  return(log)
}

#QA data converted all ton to KG in Traffic count sheet, 
#This function will do the same on raw data
convert_ton_kg <- function(data){
  
  for(col_i in 1:length(data)){
    for(row_i in 1:nrow(data)){
      col_val <- data[[col_i]][row_i]
      
      if(col_val %in% "Ton"){
        
        data[[col_i]][row_i] <- "Kilogram (KG)"
        # print(data[[col_i-1]][row_i])
        data[[col_i-1]][row_i] <- as.numeric(data[[col_i-1]][row_i])*1000
      }
    }
  }
  return(data)
}
