
Get_last_trading_day <- function(date){
  
  if(wday(date, week_start = 1) <= 5){
    return(date)
  } else {
    Get_last_trading_day(date %>% add(days(-1)))
  }
}



Get_state <- function(date, funds = "all"){
  
  date_str <- date %>% format("%Y%m%d")
  
  # if(tolower(funds) == "all") funds <- c("ARKK", "ARKQ", "ARKW","ARKG","ARKF")
  if(tolower(funds) == "all") funds <- c("ARKK", "ARKF")
  
  file_names <- funds %>% map_chr(~ glue("{date_str}_{.x}.csv")) %>% paste0("Daily_reports/", .)
  
  state_df <- file_names %>%
    map_dfr(~ read_csv(.x)) %>%
    na.omit() %>%
    janitor::clean_names() %>%
    select(-cusip) %>%
    group_by(date, fund, company, ticker) %>%
    summarise(shares = sum(shares),
              market_value = sum(market_value)) %>%
    ungroup() %>% 
    mutate(date = mdy(date))
  
  return(state_df)
}

# Get_global_state <- function(date, funds = "all"){
#   
#   date_str <- date %>% format("%Y%m%d")
#   
#   if(tolower(funds) == "all") funds <- c("ARKK", "ARKQ", "ARKW","ARKG","ARKF")
#   
#   file_names <- funds %>% map_chr(~ glue("{date_str}_{.x}.csv")) %>% paste0("Daily_reports/", .)
#   
#   state_df <- file_names %>%
#     map_dfr(~ read_csv(.x)) %>%
#     na.omit() %>%
#     janitor::clean_names() %>%
#     select(-cusip) %>%
#     group_by(date, company, ticker) %>%
#     summarise(shares = sum(shares),
#               market_value = sum(market_value)) %>%
#     ungroup() %>% 
#     mutate(date = mdy(date))
#   
#   return(state_df)
# }

Get_altas_bajas <- function(today, previous_day, funds){
  
  current_state <- Get_state(today, funds)
  previous_state <- Get_state(previous_day, funds)
  
  previous_state <- previous_state %>% filter(ticker != "BABA")
  current_state <- current_state %>% filter(ticker != "FB")
  
  altas <- anti_join(current_state, previous_state, by = c("ticker", "fund"))       # Que informacion quiero traerme? (columnas)
  bajas <- anti_join(previous_state, current_state, by = c("ticker", "fund"))
  
  
  return(list(altas = altas, bajas = bajas))
  
}

Get_previous_date <- function(date, days_back){
  date %>%
    add_with_rollback(days(-as.numeric(days_back))) %>%
    Get_last_trading_day()
}


Get_delta <- function(current_state, previous_state){
  
  current_state %>% 
    select(-date) %>% 
    full_join(previous_state %>% select(-date), by = c("company", "ticker")) %>% 
    mutate(shares = shares.x - shares.y, 
           market_value = market_value.x - market_value.y) %>% 
    select(-shares.x, -shares.y, -market_value.x, -market_value.y)
  
}