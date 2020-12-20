
v_str_detect <- function(strings, patterns){
  # vectorized version of str_detect
  patterns %>% map(function(pattern) strings %>% keep(~ str_detect(.x, pattern))) %>% flatten_chr()
}

Get_last_trading_day <- function(date){
  
  # los días los medimos por el día del archivo
  if(wday(date, week_start = 1) %>% between(2,6)){
    return(date)
  } else {
    Get_last_trading_day(date %>% add(days(-1)))
  }
}

Get_previous_date <- function(date, days_back){
  
  date %>%
    add_with_rollback(days(-as.numeric(days_back))) %>%
    Get_last_trading_day()
}


Get_files_to_read <- function(date = NULL, funds = "all", states_path = "Daily_states/"){
  
  if(length(funds) == 1 & tolower(funds[1]) == "all") funds <- c("ARKK", "ARKQ", "ARKW", "ARKG", "ARKF")
  
  if(is.null(date)){
    file_names <- list.files(states_path, full.names = T) %>% v_str_detect(funds)
  } else {
    date_str <- if_else(is.Date(date), format(date, "%Y%m%d"), as.character(date))
    file_names <- funds %>% map_chr(~ glue("{states_path}{date_str}_{.x}.csv"))
  }
  return(file_names)
}


Get_states <- function(date = NULL, funds = "all"){
  
  states_df <- Get_files_to_read(date, funds) %>%
    map_dfr(~ suppressMessages(suppressWarnings(read_csv(.x)))) %>%
    na.omit() %>%
    distinct() %>% 
    select(-cusip) %>%
    janitor::clean_names() %>%
    rename(weight = weight_percent) 
  
  
  if(funds == "all"){
    aggro_states_df <- states_df %>% 
      group_by(date, company, ticker) %>% 
      summarise(shares = sum(shares), 
                market_value = sum(market_value)) %>%
      ungroup() %>% 
      group_by(date) %>% 
      mutate(weight = round(market_value/sum(market_value)*100, 2),
             fund = "AGGR") %>% 
      ungroup() %>% 
      relocate(fund, .after = date)
    
    states_df <- bind_rows(states_df, aggro_states_df)
  }
  
  
  states_df <- states_df %>% 
    mutate(date = mdy(date), 
           M_shares = shares/1e6, 
           close_price = market_value/shares)
  
  dates_df <- states_df %>% pull(date) %>% unique() %>% enframe(name = "trading_day", value = "date")
  
  states_df <- left_join(dates_df, states_df, by = "date")
  
  return(states_df)
}

Plot_stock <- function(df, stock, quantity, mode = "aggregated"){
  
  if(quantity == "M_shares"){
    quantity <- "shares" 
    quantity_label <- "Shares (Millions)"
  } else if(quantity == "weight"){
    quantity_label <- quantity
  }
  
  df <- df %>% filter(ticker == stock) %>% select(-shares) %>% rename(shares = M_shares)
  
  
  
  max_quantity <- max(df %>% pull(quantity), na.rm = T)
  max_price <-  max(df$close_price, na.rm = T)
  large <- max(max_quantity, max_price)
  small <- min(max_quantity, max_price)
  # scale_ratio <- large / small * .8
  scale_ratio <- small / large * .8
  
  if(mode == "aggregated"){
    
    df %>% 
      filter(fund == "AGGR") %>% 
      ggplot() + 
      aes(x = trading_day) + 
      aes_string(y = quantity) + 
      geom_col(fill = "darkorange", alpha = 0.75) + 
      geom_line(aes(trading_day, close_price * scale_ratio),
                col = "darkgreen", key_glyph = "timeseries", size = 2, alpha = 0.6) + 
      geom_point(aes(trading_day, close_price * scale_ratio),
                 col = "darkgreen", size = 5, alpha = 0.6) + 
      theme_minimal() +
      scale_color_tableau() +
      scale_y_continuous(limits = c(0, NA), sec.axis = sec_axis(~ . / scale_ratio, name = "Stock price",
                                                                labels = scales::dollar)) + 
      labs(title = glue('Price and {quantity} evolution'), x = "Trading day", y = quantity_label) + 
      theme(axis.title.y.right = element_text(color = "darkgreen", face = "bold", size = 16), 
            axis.text.y.right = element_text(color = "darkgreen", size = 12)) + 
      theme(axis.title.x = element_text(size = 16), 
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 16, face = "bold"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = 24, face = "bold", hjust = 0.5))
    
  } else if(mode == "by_fund"){
    
    # Arreglar gráfica desagregada por fondos
    
    
    df %>% 
      filter(fund != "AGGR") %>% 
      ggplot(aes(x = trading_day, col = fund)) + 
      geom_line(aes_string(y = quantity)) + 
      geom_point(aes_string(y = quantity)) + 
      geom_line(aes(trading_day, close_price * scale_ratio), col = "darkgreen", key_glyph = "timeseries") +
      geom_point(aes(trading_day, close_price * scale_ratio), col = "darkgreen", show.legend = F) +
      theme_minimal() + 
      scale_color_tableau() +
      # scale_y_continuous(sec.axis = sec_axis(~ . / scale_ratio, name = "$ Price")) + 
      # theme(axis.title.y.right = element_text(color = "darkgreen"))
      NULL
  }
}


Plot_fund_wide <- function(df, fund, quantity){
  
  p <- df %>% 
    filter(fund == !!fund) %>% 
    ggplot() + 
    aes(x = trading_day) + 
    aes_string(y = quantity) + 
    geom_col(width = 1, fill = "darkorange", alpha = .8) +
    facet_wrap(~ticker, nrow = 1, strip.position = "bottom") + 
    theme_minimal() +
    labs(y = quantity, title = fund) +  
    scale_color_tableau() + 
    theme(axis.title.x = element_blank(), 
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 16, face = "bold"),
          strip.text = element_text(size = 12, face = "bold", angle = 50, hjust = 0.2, vjust = 0.2), 
          plot.title = element_text(size = 24, face = "bold", hjust = 0.5))
  
  # if(quantity == "M_shares") p <- p + labs(y = "Shares (Millions)") 
  
  return(p)
}



Plot_fund_long <- function(df, fund, quantity, scale_free){
  
  # Intento de hacer un grid con las stocks más importantes arriba y las menos abajo
  
  # dates_df <- enframe(unique(df$date), name = "trading_day", value = "date")
  # 
  # top_weight_df <- df %>% 
  #   filter(date == max(date)) %>% 
  #   mutate(top_weight = if_else(weight > median(weight), TRUE, FALSE)) %>% 
  #   select(ticker, top_weight)
  # 
  # df <- left_join(dates_df, df, by = "date") %>% 
  #   select(-date) %>% 
  #   filter(fund == !!fund) %>% 
  #   left_join(top_weight_df, by = "ticker") %>% 
  #   # group_by(trading_day) %>% 
  #   # top_n(20, weight) %>% 
  #   mutate(k_shares = shares/1e3) 
  # 
  # df %>% 
  #   ggplot() + 
  #   aes(x = trading_day) + 
  #   aes_string(y = quantity) + 
  #   geom_col(fill = "darkorange", alpha = .8) +
  #   facet_grid(top_weight~ticker) + 
  #   theme_minimal() +
  #   scale_color_tableau() + 
  #   theme(axis.title.x = element_blank(), panel.grid = element_blank(),
  #         axis.text.x = element_blank(),
  #         strip.text = element_text(size = 16, face = "bold"))
  
  dates_df <- enframe(unique(df$date), name = "trading_day", value = "date")
  
  df <- left_join(dates_df, df, by = "date") %>% 
    select(-date) %>% 
    filter(fund == !!fund) %>% 
    group_by(trading_day) %>% 
    top_n(20, weight) %>% 
    mutate(k_shares = shares/1e3) %>% 
    group_by(trading_day, ticker) %>% 
    summarise(k_shares = sum(k_shares)) %>% 
    ungroup()
  
  # n_stocks <- n_distinct(df$ticker)
  
  p <- df %>% 
    ggplot() + 
    aes(x = trading_day) + 
    aes_string(y = quantity) + 
    geom_col(fill = "darkorange", alpha = .8)
  
  
  if(scale_free){
    p <- p + facet_wrap(~ticker, ncol = 1, scales = "free_y")
  } else {
    p <- p + facet_wrap(~ticker, ncol = 1)
  }
  
  p <- p + 
    theme_minimal() +
    scale_color_tableau() + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          strip.text = element_text(size = 16, face = "bold"))
  
  return(p)
}


# Get_prices <- function(symbol, begin = ymd(20200901), end = today()){
#   
#   symbol %>% 
#     tq_get(get  = "stock.prices",
#            from = begin, 
#            to   = end) %>% 
#     # mutate(close_lag1 = lag(close, 1)) %>% 
#     # arrange(symbol, date) %>% 
#     select(symbol, date, open, close)
# }


Get_altas_bajas <- function(today, previous_day, funds){
  
  current_state <- Get_states(today, funds) #%>% filter(fund != "AGGR")
  previous_state <- Get_states(previous_day, funds) #%>% filter(fund != "AGGR")
  
  altas <- anti_join(current_state, previous_state, by = c("ticker", "fund"))       # Que informacion quiero traerme? (columnas)
  bajas <- anti_join(previous_state, current_state, by = c("ticker", "fund"))
  
  
  return(list(altas = altas, bajas = bajas))
}



Get_delta <- function(current_state, previous_state, arrange_by = NULL, only_trades){
  
  delta_df <- current_state %>%
    select(-date, -trading_day) %>%
    full_join(previous_state %>% select(-date, -trading_day), by = c("fund", "company", "ticker")) %>%
    mutate(
      delta_shares = case_when(is.na(shares.y) ~ shares.x,
                               is.na(shares.x) ~ -shares.y,
                               TRUE ~ shares.x - shares.y),
      delta_pct_shares = case_when(is.na(shares.y) ~ 99999,
                                   is.na(shares.x) ~ -99999, 
                                   TRUE ~ round((shares.x - shares.y)/shares.y*100, 2),),
      
      delta_price = round(close_price.x - close_price.y, 2),
      delta_pct_price = round((close_price.x - close_price.y) / close_price.y*100, 2), 
      price = round(close_price.x, 2),
      
      delta_weight = case_when(is.na(weight.y) ~ weight.x,                               # .x es el current,
                               is.na(weight.x) ~ -weight.y,                              # .y es el previous
                               TRUE ~ round(weight.x - weight.y, 2)),
      delta_pct_weight = case_when(is.na(weight.y) ~ 99999,
                                   is.na(weight.x) ~ -99999, 
                                   TRUE ~ round((weight.x - weight.y) / weight.y*100, 2)),
      weight = weight.x
    ) %>%
    select(-shares.x, -shares.y, -M_shares.x, -M_shares.y,
           -market_value.x, -market_value.y, -weight.x, -weight.y, 
           -close_price.x, -close_price.y) 
  
  
  if(only_trades)
    delta_df <- delta_df %>% filter(delta_shares != 0)
  
  # Funciona tb si lo pasamos desde fuera desde la variable del selector (input$delta_quantity)
  if(!is.null(arrange_by))
    delta_df <- delta_df %>% arrange(desc(!!sym(arrange_by)))
  
  return(delta_df)
}

