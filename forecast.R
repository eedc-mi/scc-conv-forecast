library(MASS)
library(tidyverse)
library(lubridate)
library(stringr)
library(here)

setwd(here())

tib <- read_csv("data.csv")

tib <- tib %>%
  mutate(
    is_definite = (status_code >= 30 & status_code < 80),
    lead_time = as.integer(start_date - date_booked),
    definite_time = as.integer(date_definite - date_booked),
    start_year = as.character(year(start_date)),
    days_to_year_end = as.integer(
      date_booked - ymd(paste(start_year, "12", "31", sep = "-"))))

ggplot(tib, aes(x = as.factor(year(start_date)), fill = is_definite)) + 
  geom_bar() +
  labs(title = "Convention Bookings by Year, as of Jan. 2018") +
  theme_minimal()

ggplot(tib %>% filter(date_booked < ymd("17-01-31")), 
       aes(x = as.factor(year(start_date)), fill = is_definite)) +
  geom_bar() +
  labs(title = "Convention Bookings by Year, as of Jan. 2017") +
  theme_minimal()

tib %>%
  count(year = year(start_date), is_definite) %>%
  group_by(year) %>%
  mutate(freq = n / sum(n)) %>%
  filter(is_definite)

fitModel <- function(data, fitYears) {
  toFit <- data %>%
    filter(start_year %in% fitYears) %>%
    group_by(start_year, days_to_year_end) %>%
    summarize(n = n()) %>%
    mutate(cs = cumsum(n)) %>%
    group_by(days_to_year_end) %>%
    summarize(cs = mean(cs))
  
  fitLoess <- loess(cs ~ days_to_year_end, data = toFit)
  pred <- predict(fitLoess)
  tibble(
    x = toFit$days_to_year_end, 
    y = pred,
    upper_20 = pred * 1.2,
    lower_20 = pred * 0.8)
}

graphModel <- function(data, currentYears, fitTbl) {
  toPlot <- data %>%
    filter(start_year %in% currentYears) %>%
    group_by(start_year, days_to_year_end) %>%
    summarize(n = n()) %>%
    mutate(num_events = cumsum(n))
  
  ggplot() +
    geom_line(
      data = toPlot,
      aes(x = days_to_year_end, y = num_events, colour = start_year)) +
    geom_line(
      data = fitTbl, 
      aes(x = x, y = y), 
      colour = "blue", size = 0.75) +
    geom_line(
      data = fitTbl %>% select(-y) %>% gather(key = "key", value = "value", -x), 
      aes(x = x, y = value, group = key), colour = "blue", linetype = "dashed") +
    annotate(
      "text", x = -500, y = max(fitTbl$upper_20) - (max(fitTbl$upper_20) * 0.05), 
      label = "Expected Booking Curve\n(+/- 20%)", colour = "blue", size = 3) +
    theme_minimal()
}

fit <- fitModel(tib, c(2016, 2017, 2018, 2019, 2020))
graphModel(tib, c(2019, 2020), fit) + 
  labs(
    title = "Actual Convention Bookings vs. Expected Booking Curve",
    subtitle = "All bookings")

fitDef <- fitModel(tib %>% filter(is_definite), c(2016, 2017, 2018, 2019, 2020))
graphModel(tib %>% filter(is_definite), c(2019, 2020), fitDef) +
  labs(
    title = "Actual Convention Bookings vs. Expected Booking Curve",
    subtitle = "Definite bookings only"
  )

# Other plots for explanation purposes
toPlot <- tib %>%
  filter(start_year %in% c(2016, 2017, 2018, 2019, 2020)) %>%
  group_by(start_year, days_to_year_end) %>%
  summarize(n = n()) %>%
  mutate(num_events = cumsum(n))

ggplot() +
  geom_line(
    data = toPlot,
    aes(x = days_to_year_end, y = num_events, colour = start_year))

toPlot <- tib %>%
  filter(start_year %in% c(2016, 2017, 2018, 2019, 2020), is_definite) %>%
  group_by(start_year, days_to_year_end) %>%
  summarize(n = n()) %>%
  mutate(num_events = cumsum(n))

ggplot() +
  geom_line(
    data = toPlot,
    aes(x = days_to_year_end, y = num_events, colour = start_year))
