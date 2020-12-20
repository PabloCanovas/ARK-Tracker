
### Check duplicated info (from weekends)

Check_duplicated_files <- function(remove_duplicates = FALSE, files_path = "Daily_states/"){
  
  all_files <- list.files(files_path)
  
  all_dates <- all_files %>% 
    map_chr(
      ~ str_split(.x, "_") %>% 
        first() %>% 
        first()) %>% 
    unique() %>% 
    ymd()
  
  right_dates <- all_dates %>% discard(~ wday(.x) %in% c(1,2))
  wrong_dates <- all_dates %>% keep(~ wday(.x) %in% c(1,2)) 
  
  files_to_remove <- c()
  
  for(d in seq_along(wrong_dates)){
    
    if(wday(wrong_dates[d]) == 1 & wrong_dates[d] %>% add(days(-1)) %in% right_dates){
      message(glue('** Duplicated: {wrong_dates[d]} file contains same info as {wrong_dates[d] %>% add(days(-1))} file'))
      files_to_remove <- append(files_to_remove, wrong_dates[d])
    }
    
    if(wday(wrong_dates[d]) == 2 & wrong_dates[d] %>% add(days(-2)) %in% right_dates){
      message(glue('** Duplicated: {wrong_dates[d]} file contains same info as {wrong_dates[d] %>% add(days(-2))} file'))
      files_to_remove <- append(files_to_remove, wrong_dates[d])
    }
  }
  
  files_to_remove <- files_to_remove %>% format("%Y%m%d") %>% map(~ list.files(files_path, pattern = .x)) %>% flatten_chr()
  
  if(remove_duplicates){
    message('Deleting duplicated files...')
    files_to_remove %>% map(~ file.remove(glue('{files_path}{.x}')))
  }
}

