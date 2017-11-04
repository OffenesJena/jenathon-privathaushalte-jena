library(readr)
library(tidyr)
library(dplyr)
library(stringr)



# Load data ---------------------------------------------------------------
load_hh <- function(x) {
  read_csv(
    paste0("data/", x, ".csv"),
    col_types = cols(
      statistischer_Bezirk_Name = col_character(),
      statistischer_Bezirk_Nr = col_character(),
      `31.12.2006` = col_integer(),
      `31.12.2007` = col_integer(),
      `31.12.2008` = col_integer(),
      `31.12.2009` = col_integer(),
      `31.12.2010` = col_integer(),
      `31.12.2011` = col_integer(),
      `31.12.2012` = col_integer(),
      `31.12.2013` = col_integer(),
      `31.12.2014` = col_integer(),
      `31.12.2015` = col_integer(),
      `31.12.2016` = col_integer(),
      `31.12.2017` = col_integer()
    )
  ) %>% 
    mutate(file = x)
}


# Combine
hh_raw <- hh_priv <- load_hh("hh_priv") %>% 
  bind_rows(load_hh("hh_priv_1")) %>% 
  bind_rows(hh_priv_1_60plus <- load_hh("hh_priv_1_60plus"))

# Tidy
hh <- hh_raw %>% 
  gather(
    key = date, 
    value = cnt, 
    -statistischer_Bezirk_Name, -statistischer_Bezirk_Nr, -file) %>% 
  arrange(statistischer_Bezirk_Name) %>% 
  mutate(
    grp = case_when(
      file == "hh_priv"          ~ "sum",
      file == "hh_priv_1"        ~ "1pers",
      file == "hh_priv_1_60plus" ~ "1pers_60plus",
      TRUE                       ~ NA_character_),
    year = str_sub(date, 7, 10)) %>% 
  rename(reg_name = statistischer_Bezirk_Name,
         reg_id   = statistischer_Bezirk_Nr) %>% 
  select(reg_name, reg_id, grp, year, cnt) %>% 
  # 2017 is not yet available
  filter(year != 2017)



# Transform data ----------------------------------------------------------
hh
