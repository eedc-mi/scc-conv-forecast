library(tidyverse)
library(lubridate)
library(stringr)
library(here)
library(officer)
library(rvg)

setwd(here())

tib <- read_csv("data.csv")

was_def_on_date <- function(x, date) {
  (x$date_booked <= date) &
  (x$adj_date_definite <= date & ! is.na(x$adj_date_definite)) & 
  (x$adj_date_cancelled > date | is.na(x$adj_date_cancelled)) &
  (! x$is_open)
}

future_plot <- ggplot(tib, aes(x = as.factor(start_year), fill = is_definite)) + 
  geom_bar() +
  scale_fill_discrete(labels = c("Not Definite", "Definite")) +
  theme_minimal() +
  labs(
    title = "Future Convention Bookings, as of Jan. 31 2018",
    fill = "",
    x = "Start year",
    y = "Number of events")

tib_as_of_16 <- tib %>% 
  filter(
    date_booked <= ymd("16-01-31")) %>%
    #(adj_date_cancelled > ymd("16-01-31") | is.na(adj_date_cancelled))) %>%
  mutate(
    was_definite = was_def_on_date(., ymd("16-01-31")),
    as_of = "As of Jan 31, 2016")

tib_as_of_18 <- tib %>% 
  filter(
    date_booked <= ymd("18-01-31")) %>%
    #(adj_date_cancelled > ymd("18-01-31") | is.na(adj_date_cancelled))) %>%
  mutate(was_definite = was_def_on_date(., ymd("18-01-31")),
    as_of = "As of Jan. 31, 2018")

tib_future <- bind_rows(
  tib_as_of_16 %>% filter(start_year %in% c(2017, 2018)), 
  tib_as_of_18 %>% filter(start_year %in% c(2019, 2020)))

table_book_future <- tib_future %>% filter(! was_definite) %>% 
  group_by(as_of, class) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_def_future <- tib_future %>% filter(was_definite) %>% 
  group_by(as_of, class) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

as_of_plot <- ggplot(
  bind_rows(tib_as_of_16, tib_as_of_18) %>% 
    filter(start_year >= 2016, start_year <= 2020),
  aes(x = as.factor(start_year), fill = was_definite)) +
  geom_bar() +
  facet_wrap(~ as_of) +
  scale_fill_discrete(labels = c("Not Definite", "Definite")) +
  theme_bw() +
  labs(
    title = "Future Convention Bookings on January 31", 
    subtitle = "2016 vs. 2018",
    fill = "",
    x = "Start year",
    y = "Number of events")

fit_model <- function(data, fitYears) {
  toFit <- data %>%
    filter(start_year %in% fitYears) %>%
    group_by(start_year, booked_days_to_year_start) %>%
    summarize(n = n()) %>%
    mutate(cs = cumsum(n)) %>%
    group_by(booked_days_to_year_start) %>%
    summarize(cs = mean(cs))
  
  fitLoess <- loess(cs ~ booked_days_to_year_start, data = toFit)
  pred <- predict(fitLoess)
  tibble(
    x = toFit$booked_days_to_year_start, 
    y = pred,
    upper_20 = pred * 1.2,
    lower_20 = pred * 0.8)
}

plot_model <- function(data, currentYears, fitTbl) {
  toPlot <- data %>%
    filter(start_year %in% currentYears) %>%
    group_by(start_year, booked_days_to_year_start) %>%
    summarize(n = n()) %>%
    mutate(num_events = cumsum(n))
  
  ggplot() +
    geom_vline(xintercept = 0, colour = "darkgrey") +
    geom_line(
      data = toPlot,
      aes(x = booked_days_to_year_start, y = num_events, colour = as.factor(start_year))) +
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
    theme_minimal() +
    labs(x = "Days to start of year", y = "Number of events", colour = "Event year")
}

sumDefsPerDay <- function(data, years) {
  full_join(
    data %>%
      filter(
        start_year %in% years, 
        ! is.na(def_days_to_year_start),
        ! is_open) %>%
      group_by(start_year, def_days_to_year_start) %>%
      summarize(n = n()),
    data %>%
      filter(
        start_year %in% years, 
        ! is.na(def_days_to_year_start),
        ! is.na(canc_days_to_year_start)) %>%
      group_by(start_year, canc_days_to_year_start) %>%
      summarize(n = -1 * n()),
    by = c("start_year", "def_days_to_year_start" = "canc_days_to_year_start")) %>%
    rowwise() %>%
    mutate(net_n = sum(n.x, n.y, na.rm = TRUE)) %>%
    group_by(start_year) %>%
    arrange(def_days_to_year_start) %>%
    mutate(cs = cumsum(net_n))  
}

fit_def_model <- function(data, fitYears) {
  toFit <- sumDefsPerDay(data, fitYears) %>%
    group_by(def_days_to_year_start) %>%
    summarize(cs = mean(cs))
  
  fitLoess <- loess(cs ~ def_days_to_year_start, data = toFit)
  pred <- predict(fitLoess)
  tibble(
    x = toFit$def_days_to_year_start, 
    y = pred,
    upper_20 = pred * 1.2,
    lower_20 = pred * 0.8)
}

plot_def_model <- function(data, currentYears, fitTbl) {
  toPlot <- sumDefsPerDay(data, currentYears)
  
  ggplot() +
    geom_vline(xintercept = 0, colour = "darkgrey") +
    geom_line(
      data = toPlot,
      aes(x = def_days_to_year_start, y = cs, colour = as.factor(start_year))) +
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
    theme_minimal() +
    labs(x = "Days to start of year", y = "Number of events", colour = "Event year")
}

fit <- fit_model(tib, c(2016, 2017, 2018, 2019, 2020))
bookModelPlot <- plot_model(tib, c(2019, 2020), fit) + 
  labs(
    title = "Actual Convention Bookings vs. Expected Booking Curve",
    subtitle = "All bookings")

fitDef <- fit_def_model(tib, c(2016, 2017, 2018, 2019, 2020))
defModelPlot <- plot_def_model(tib, c(2019, 2020), fitDef) +
  labs(
    title = "Actual Convention Bookings vs. Expected Booking Curve",
    subtitle = "Definite bookings only"
  )

book_quarter_plot <- ggplot(
  full_join(
    tib %>% filter(year(date_booked) > 2015, ! is.na(def_quarter)) %>%
      count(def_quarter),
    tib %>% filter(year(date_booked) > 2015) %>%
      count(booked_quarter),
    by = c("def_quarter" = "booked_quarter")) %>%
    replace_na(list(n.x = 0)) %>%
    gather(key = "key", value = "value", n.x, n.y),
  aes(x = as.factor(def_quarter), y = value, fill = key)) +
  geom_bar(stat = "identity", position  = "dodge") +
  scale_fill_discrete(
    labels = c("Definite", "Not Definite")) +
  theme_minimal() +
  labs(
    title = "New Convention Bookings and Definites by Quarter", 
    x = "Quarter", 
    y = "Number of events", 
    fill = "")

ppt <- read_pptx()

ppt <- ppt %>%
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with_text(type = "ctrTitle", str = "SCC Convention Booking Analysis") %>% 
  ph_with_text(type = "subTitle", str = today()) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_gg(value = future_plot, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_gg(value = as_of_plot, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_def_future, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_gg(value = bookModelPlot, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_gg(value = defModelPlot, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_gg(value = book_quarter_plot, type = "body")
  
  

print(ppt, "booking.pptx")