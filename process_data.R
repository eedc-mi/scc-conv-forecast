library(tidyverse)
library(lubridate)
library(stringr)
library(here)

setwd(here())

dataPath <- file.path(
  "V:",
  "Economic Intelligence",
  "Shaw Conference Centre",
  "Projects",
  "Convention Forecast",
  "data"
)

tib <- read_csv(file.path(dataPath, "events.csv"))

fixName <- function(string) {
  newName <- unlist(strsplit(string, "-"))[1]
  newName <- trimws(tolower(newName))
  newName <- str_replace_all(newName, " ", "_")
  newName <- str_replace_all(newName, "/", "_")
  newName <- str_replace_all(newName, "\\(", "")
  newName <- str_replace_all(newName, "\\)", "")
  newName <- str_replace_all(newName, "\\?", "")
  
  return(newName)
}

names(tib) <- sapply(names(tib), fixName)

tib <- tib %>%
  mutate_at(vars(contains("date")), ymd) %>%
  filter(! grepl(" test ", post_as, ignore.case = TRUE))

