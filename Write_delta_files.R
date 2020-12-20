
 ### Get deltas between consecutive files

Write_delta_files <- function(write_to = "Daily_deltas/", funds = "all", states_path = "Daily_states/"){
  
  all_files <- list.files(states_path, full.names = T)
  
  all_data <- suppressMessages(suppressWarnings(
    all_files %>% 
      map_dfr(~ read_csv(.x)) %>% 
      na.omit() %>% 
      select(-cusip) %>% 
      janitor::clean_names() %>% 
      rename(weight = weight_percent) %>% 
      mutate(date = mdy(date))
  ))
  
  dates <- all_data$date %>% unique()
  
  for(d in seq_along(dates)){
    
    if(d == 1) next
    
    date_str <- dates[d] %>% format('%Y%m%d')
    
    df1 <- all_data %>% filter(date == dates[d-1])
    df2 <- all_data %>% filter(date == dates[d])
    
    delta <- full_join(df2, df1, by = c("fund", "ticker", "company")) %>% 
      mutate(notes = if_else(is.na(date.x), "Baja", NA_character_), 
             notes = if_else(is.na(date.y), "Alta", notes),
             delta_weight = weight.x - weight.y,                                      
             delta_shares = shares.x - shares.y,
             delta_value = market_value.x - market_value.y) %>% 
      # Corregimos los que han desaparecido y las nuevas entradas
      mutate(delta_weight = case_when(is.na(date.x) ~ -weight.y, 
                                      is.na(delta_weight) ~ weight.x,
                                      TRUE ~ delta_weight),
             delta_shares = case_when(is.na(date.x) ~ -shares.y, 
                                      is.na(delta_shares) ~ shares.x,
                                      TRUE ~ delta_shares),
             delta_value = case_when(is.na(date.x) ~ -market_value.y, 
                                     is.na(delta_value) ~ market_value.x,
                                     TRUE ~ delta_value)) %>% 
      select(date = date.x, fund, ticker, company, contains("delta"), notes) %>% 
      fill(date, .direction = "downup")
    
    write_csv(delta, glue("{write_to}Delta_{date_str}.csv"))
  }
}