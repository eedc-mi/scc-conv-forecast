library(tidyverse)
library(lubridate)
library(stringr)
library(here)

setwd(here())

tib <- read_csv("data.csv")

# data with incorrect created dates filtered out
tibLeadTime <- tib %>%
  filter(date_booked != ymd("15-01-31"))

tib %>%
  group_by(year(start_date)) %>%
  count()
