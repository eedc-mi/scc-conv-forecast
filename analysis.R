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

tib_statusbreakdown <- tib[which(tib$status_code == 30 | tib$status_code == 80 |
                                   tib$status_code == 82 | tib$status_code == 28 |
                                   tib$status_code ==29),]

future_plot <- ggplot(tib_statusbreakdown, aes(x = as.factor(start_year), fill = status)) + 
  geom_bar() +
#  scale_fill_discrete(labels = c("Not Definite", "Definite")) +
  theme_minimal() +
  labs(
    title = "Future Convention Bookings, as of Jan. 31 2018",
    fill = "",
    x = "Start year",
    y = "Number of events")

tib_as_of_16 <- tib_statusbreakdown %>% 
  filter(
    date_booked <= ymd("16-01-31")) %>%
    #(adj_date_cancelled > ymd("16-01-31") | is.na(adj_date_cancelled))) %>%
  mutate(
    was_definite = was_def_on_date(., ymd("16-01-31")),
    as_of = "As of Jan 31, 2016")

tib_as_of_18 <- tib_statusbreakdown %>% 
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

# By Lead Source --------------------------------------------------------------

table_book_future_CVent <- tib_future %>% filter(! was_definite, lead_source == "CVent") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_ET <- tib_future %>% filter(! was_definite, lead_source == "Edmonton Tourism") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_EEDC <- tib_future %>% filter(! was_definite, lead_source == "EEDC") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_Hotels <- tib_future %>% filter(! was_definite, lead_source == "Hotels") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_Phone-In <- tib_future %>% filter(! was_definite, lead_source == "Phone-In") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_Repeat <- tib_future %>% filter(! was_definite, lead_source == "Repeat Business") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_SCCtoET <- tib_future %>% filter(! was_definite, lead_source == "SCC to ET") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_SCCWeb <- tib_future %>% filter(! was_definite, lead_source == "SCC Website") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_Solicitation <- tib_future %>% filter(! was_definite, lead_source == "Solicitation") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_Staff <- tib_future %>% filter(! was_definite, lead_source == "Staff Lead") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_TOU <- tib_future %>% filter(! was_definite, lead_source == "TOU") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_TravelAB <- tib_future %>% filter(! was_definite, lead_source == "Travel Alberta") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_US50 <- tib_future %>% filter(! was_definite, lead_source == "US 50") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

table_book_future_Website <- tib_future %>% filter(! was_definite, lead_source == "Website") %>% 
  group_by(as_of, class, lead_source) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

#-----------------------------------------------------------------------------------

table_def_future <- tib_future %>% filter(was_definite) %>% 
  group_by(as_of, class) %>% 
  summarize(n = n()) %>%
  spread(as_of, n)

as_of_plot <- ggplot(
  bind_rows(tib_as_of_16, tib_as_of_18) %>% 
    filter(start_year >= 2016, start_year <= 2020),
  aes(x = as.factor(start_year), fill = status)) +
  geom_bar() +
  facet_wrap(~ as_of) +
#  scale_fill_discrete(labels = c("Not Definite", "Definite")) +
  theme_bw() +
  labs(
    title = "Future Convention Bookings on January 31", 
    subtitle = "2016 vs. 2018",
    fill = "",
    x = "Start year",
    y = "Number of events")

fit_model <- function(data, fitYears) {
  to_fit <- data %>%
    filter(start_year %in% fitYears) %>%
    group_by(start_year, booked_days_to_year_start) %>%
    summarize(n = n()) %>%
    mutate(cs = cumsum(n)) %>%
    group_by(booked_days_to_year_start) %>%
    summarize(cs = mean(cs))
  
  fit_loess <- loess(cs ~ booked_days_to_year_start, data = to_fit)
  pred <- predict(fit_loess)
  tibble(
    x = to_fit$booked_days_to_year_start, 
    y = pred,
    upper_20 = pred * 1.2,
    lower_20 = pred * 0.8)
}

plot_model <- function(data, currentYears, fitTbl) {
  to_plot <- data %>%
    filter(start_year %in% currentYears) %>%
    group_by(start_year, booked_days_to_year_start) %>%
    summarize(n = n()) %>%
    mutate(num_events = cumsum(n))
  
  ggplot() +
    geom_vline(xintercept = 0, colour = "darkgrey") +
    geom_line(
      data = to_plot,
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

sum_defs_per_day <- function(data, years) {
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
  to_fit <- sum_defs_per_day(data, fitYears) %>%
    group_by(def_days_to_year_start) %>%
    summarize(cs = mean(cs))
  
  fit_loess <- loess(cs ~ def_days_to_year_start, data = to_fit)
  pred <- predict(fit_loess)
  tibble(
    x = to_fit$def_days_to_year_start, 
    y = pred,
    upper_20 = pred * 1.2,
    lower_20 = pred * 0.8)
}

plot_def_model <- function(data, currentYears, fitTbl) {
  to_plot <- sum_defs_per_day(data, currentYears)
  
  ggplot() +
    geom_vline(xintercept = 0, colour = "darkgrey") +
    geom_line(
      data = to_plot,
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
book_model_plot <- plot_model(tib, c(2019, 2020), fit) + 
  labs(
    title = "Actual Convention Bookings vs. Expected Booking Curve",
    subtitle = "All bookings")

fit_def <- fit_def_model(tib, c(2016, 2017, 2018, 2019, 2020))
def_model_plot <- plot_def_model(tib, c(2019, 2020), fit_def) +
  labs(
    title = "Actual Convention Bookings vs. Expected Booking Curve",
    subtitle = "Definite bookings only"
  )

tibjoin1 <- full_join(
  tib_statusbreakdown %>% filter(year(date_booked) > 2015, ! is.na(def_quarter)) %>%
    count(def_quarter),
  tib_statusbreakdown %>% filter(year(date_booked) > 2015) %>%
    count(booked_quarter),
  by = c("def_quarter" = "booked_quarter"))

tibjoin2 <- full_join(
  tibjoin1,
  tib_statusbreakdown %>% filter(year(date_booked) > 2015, ! is.na(cancelled_quarter)) %>%
    count(cancelled_quarter),
  by = c("def_quarter" = "cancelled_quarter"))

book_quarter_plot <- ggplot(
    tibjoin2 %>%
    replace_na(list(n.x = 0)) %>%
    gather(key = "key", value = "value", n.x, n.y, n),
  aes(x = as.factor(def_quarter), y = value, fill = key)) +
  geom_bar(stat = "identity", position  = "dodge") +
  scale_fill_discrete(
    labels = c("Definite", "Booked", "Cancelled")) +
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
  ph_with_table(value = table_book_future_CVent, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_ET, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_EEDC, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_Hotels, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_Repeat, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_SCCtoET, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_SCCWeb, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_Solicitation, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_Staff, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_TOU, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_TravelAB, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_US50, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_book_future_Website, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_table(value = table_def_future, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_gg(value = book_model_plot, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_gg(value = def_model_plot, type = "body") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(str = "", type = "title") %>%
  ph_with_gg(value = book_quarter_plot, type = "body")
  
  

print(ppt, "booking.pptx")