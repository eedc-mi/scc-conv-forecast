library(tidyverse)
library(lubridate)
library(stringr)
library(here)

setwd(here())

data_path <- file.path(
  "V:",
  "Economic Intelligence",
  "Shaw Conference Centre",
  "Projects",
  "Convention Forecast",
  "data"
)

tib <- read_csv(file.path(data_path, "events.csv"))
to_exclude <- read_csv(file.path(data_path, "to_exclude.csv"))

fix_name <- function(string) {
  new_name <- unlist(strsplit(string, "-"))[1]
  new_name <- trimws(tolower(new_name))
  new_name <- str_replace_all(new_name, " ", "_")
  new_name <- str_replace_all(new_name, "/", "_")
  new_name <- str_replace_all(new_name, "\\(", "")
  new_name <- str_replace_all(new_name, "\\)", "")
  new_name <- str_replace_all(new_name, "\\?", "")
  
  return(new_name)
}

names(tib) <- sapply(names(tib), fix_name)

tib <- tib %>%
  mutate(date_cancelled = substr(cancelled_on, 1, 8)) %>%
  select(-cancelled_on) %>%
  mutate_at(vars(contains("date")), ymd) %>%
  filter(! grepl(" test ", post_as, ignore.case = TRUE)) %>%
  mutate(status_code = as.integer(str_extract(status, "\\d+"))) %>%
  filter(! event_id %in% to_exclude$event_id) %>%
  mutate(
    is_definite = (status_code >= 30 & status_code < 80),
    is_cancelled = status_code %in% c(80, 82, 85),
    is_open = status_code %in% c(28, 29),
    lead_time = as.integer(start_date - date_booked),
    start_year = as.integer(year(start_date)),
    adj_date_definite = case_when(
      is.na(date_definite) & is_definite ~ date_booked,
      ! is.na(date_definite) ~ date_definite, 
      TRUE ~ as.Date(NA)),
    adj_date_cancelled = case_when(
      is.na(date_cancelled) & is_cancelled ~ date_booked,
      ! is.na(date_cancelled) ~ date_cancelled,
      TRUE ~ as.Date(NA)),
    booked_quarter = quarter(date_booked, with_year = TRUE),
    cancelled_quarter = quarter(date_cancelled, with_year = TRUE),
    def_quarter = quarter(adj_date_definite, with_year = TRUE),
    booked_days_to_year_start = as.integer(
      date_booked - ymd(paste(start_year, "12", "31", sep = "-"))),
    def_days_to_year_start = as.integer(
      adj_date_definite - ymd(paste(start_year, "12", "31", sep = "-"))),
    canc_days_to_year_start = as.integer(
      adj_date_cancelled - ymd(paste(start_year, "12", "31", sep = "-"))),
    booked_days_to_year_start = as.integer(
      date_booked - ymd(paste(start_year, "01", "01", sep = "-"))),
    def_days_to_year_start = as.integer(
      adj_date_definite - ymd(paste(start_year, "01", "01", sep = "-"))),
    canc_days_to_year_start = as.integer(
      adj_date_cancelled - ymd(paste(start_year, "01", "01", sep = "-"))))

if (file.exists("data.csv")) file.remove("data.csv")
write_csv(tib, "data.csv")
