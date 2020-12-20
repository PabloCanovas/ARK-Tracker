

setwd("C:/Users/Pablo/Documents/RWork/Investing/ARK/")

library(tidyverse)
library(lubridate)
library(glue)
library(magrittr)

today <- today() %>% format("%Y%m%d")

### Define funds ----------------------------------

urls <- list(
  ARKK = "https://ark-funds.com/wp-content/fundsiteliterature/csv/ARK_INNOVATION_ETF_ARKK_HOLDINGS.csv",
  ARKQ = "https://ark-funds.com/wp-content/fundsiteliterature/csv/ARK_AUTONOMOUS_TECHNOLOGY_&_ROBOTICS_ETF_ARKQ_HOLDINGS.csv",
  ARKW = "https://ark-funds.com/wp-content/fundsiteliterature/csv/ARK_NEXT_GENERATION_INTERNET_ETF_ARKW_HOLDINGS.csv",
  ARKG = "https://ark-funds.com/wp-content/fundsiteliterature/csv/ARK_GENOMIC_REVOLUTION_MULTISECTOR_ETF_ARKG_HOLDINGS.csv",
  ARKF = "https://ark-funds.com/wp-content/fundsiteliterature/csv/ARK_FINTECH_INNOVATION_ETF_ARKF_HOLDINGS.csv"
)


### Download files ----------------------------------

message("Downloading files...\n ")

imap(urls,
     ~ {
       download.file(.x, glue("Daily_states/{today}_{.y}.csv"))
       Sys.sleep(max(0, 3 + rnorm(1, 5, 5)))
     })




# Check files have been downloaded and, if not, send email to notify.











### Read funds files ----------------------------------

# file_names <- names(urls) %>% map_chr(~ glue("{today}_{.x}.csv")) %>% paste0("Daily_states/", .)
# 
# 
# today_state <- file_names %>%
#   map_dfr(~ read_csv(.x)) %>% 
#   na.omit() %>% 
#   janitor::clean_names() %>% 
#   select(-cusip) %>% 
#   group_by(date, company, ticker) %>% 
#   summarise(shares = sum(shares),
#             market_value = sum(market_value)) %>% 
#   mutate(date = mdy(date))
# 
# df <- read_csv("ARK_funds.csv") %>% 
#   select(-cusip) %>% 
#   group_by(date, company, ticker) %>% 
#   summarise(shares = sum(shares),
#             market_value = sum(market_value)) %>% 
#   mutate(date = mdy(date))
# 
# 
# today_state %>% 
#   mutate(date = date %>% add(days(-1))) %>%
#   full_join(df %>% filter(date == max(date)), by = c("date", "company", "ticker")) %>% 
#   mutate(shares = shares.x - shares.y, 
#          market_value = market_value.x - market_value.y) %>% 
#   select(-shares.x, -shares.y, -market_value.x, -market_value.y)
# 
# 
# write_csv(today_state, "ARK_funds.csv")
# 


