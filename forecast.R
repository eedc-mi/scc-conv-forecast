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

ggplot(tib, aes(x = year(start_date), fill = is_definite)) + geom_bar()

ggplot(tib %>% filter(year(date_booked) < 2018), 
       aes(x = year(start_date), fill = is_definite)) +
  geom_bar()

tib %>%
  count(year = year(start_date), is_definite) %>%
  group_by(year) %>%
  mutate(freq = n / sum(n)) %>%
  filter(is_definite)

# data with incorrect created dates filtered out
tibLt <- tib %>%
  filter(date_booked != ymd("15-01-31"))

ggplot(tibLt %>% 
         select(lead_time, definite_time) %>%
         gather(),
       aes(x = key, y = value)) + geom_boxplot()


fit <- fitdistr(tibLt$lead_time, "gamma", lower = 0.001)

bsFit <- function(data, indices) {
  d <- data[indices]
  fitMLE <- fitdistr(d, "gamma", lower = 0.001)
  fitMLE$estimate
}

test <- tibLt %>% filter(year(date_booked) != 2015)

bsParam <- boot(data = test$lead_time, statistic = bsFit, R = 1000)
plot(bsParam)

hist(test$lead_time, breaks = 30, freq = FALSE)
for (i in 1:nrow(bsParam$t)) {
  lines(
    x = seq(0, 3000, 10),
    y = dgamma(
      seq(0, 3000, 10),
      shape = bsParam$t[i, 1],
      rate = bsParam$t[i, 2]),
    col = rgb(.6, .6, .6, .1))
}

probs <- mapply(pgamma, bsParam$t[, 1], bsParam$t[, 2], MoreArgs = list(q = 715))
hist(sapply(probs, function (x) 14 + ((runif(1, 150, 170) - 140) * x) * 0.3))

ggplot(tibLt, 
       aes(x = lead_time)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_line(aes(
    y = dgamma(lead_time, shape = fit$estimate["shape"], rate = fit$estimate["rate"])),
    color = "red")

numEvents <- tib %>%
  group_by(year = year(start_date)) %>%
  count() %>%
  filter(year ==  2020) %>%
  ungroup() %>%
  select(n) %>%
  first()

(150 - 94) * pgamma(715, shape = fit$estimate["shape"], rate = fit$estimate["rate"])
(150 - 53) * pgamma(1080, shape = fit$estimate["shape"], rate = fit$estimate["rate"])

150 * pgamma(715, shape = fit$estimate["shape"], rate = fit$estimate["rate"], lower.tail = FALSE)

gam <- rgamma(150, shape = fit$estimate["shape"], rate = fit$estimate["rate"])
hist(gam, breaks = 30, freq = FALSE)

ggplot(tibLt %>% filter(is_definite), 
       aes(x = lead_time)) + 
  geom_histogram(aes(y = ..density..))


test <- tib %>%
  filter(year(date_booked) < 2016)

ggplot(test, aes(x = year(start_date), fill = is_definite)) + geom_bar()

test %>% group_by(year(start_date)) %>% count()

91 / pgamma(715, shape = fit$estimate["shape"], rate = fit$estimate["rate"], lower.tail = FALSE)

plot(sapply(
  seq(0, 200, 1), 
  function(x) x / pgamma(
    715, shape = fit$estimate["shape"], 
    rate = fit$estimate["rate"], 
    lower.tail = FALSE)))

test <- tib %>%
  filter(year(start_date) %in% c(2016, 2017, 2018, 2019, 2020)) %>%
  mutate(
    start_year = as.character(year(start_date)),
    days_to_year_end = date_booked - ymd(paste(start_year, "12", "31", sep = "-"))) %>%
  group_by(start_year, days_to_year_end) %>%
  summarize(n = n()) %>%
  mutate(cs = cumsum(n))

ggplot(test, aes(x = days_to_year_end, y = cs, color = start_year)) + geom_line()
       

test <- tib %>%
  filter(year(start_date) %in% c(2016, 2017, 2018, 2019, 2020), is_definite) %>%
  mutate(
    start_year = as.character(year(start_date)),
    days_to_year_end = date_definite - ymd(paste(start_year, "12", "31", sep = "-"))) %>%
  group_by(start_year, days_to_year_end) %>%
  summarize(n = n()) %>%
  mutate(cs = cumsum(n))

ggplot(test, aes(x = days_to_year_end, y = cs, color = start_year)) + geom_line()



test2 <- tib %>%
  filter(year(start_date) %in% c(2016, 2017, 2018, 2019, 2020)) %>%
  mutate(
    start_year = as.character(year(start_date)),
    days_to_year_end = date_booked - ymd(paste(start_year, "12", "31", sep = "-"))) %>%
  group_by(start_year, days_to_year_end) %>%
  summarize(n = n()) %>%
  mutate(cs = cumsum(n)) %>%
  group_by(days_to_year_end) %>%
  summarize(cs = mean(cs))

test2 <- test2 %>% mutate(days_to_year_end = as.integer(days_to_year_end))
fitLoess <- loess(cs ~ days_to_year_end, data = test2)

x <- seq(range(test2$days_to_year_end)[1], range(test2$days_to_year_end)[2], length(test2$days_to_year_end))
pred <- predict(fitLoess, se = TRUE)

ggplot(test %>% filter(start_year == 2019), aes(x = days_to_year_end, y = cs)) + 
  geom_line() + 
  geom_line(data = test2, aes(x = days_to_year_end, y = pred$fit), colour = "blue", size = 1)



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
    mutate(cs = cumsum(n))
  
  ggplot(toPlot,
    aes(x = days_to_year_end, y = cs, color = start_year)) +
    geom_line() +
    geom_line(data = fitTbl, aes(x = x, y = y), colour = "blue", size = 0.75) +
    geom_line(
      data = fitTbl %>% select(-y) %>% gather(key = "key", value = "value", -x), 
      aes(x = x, y = value, group = key), colour = "blue", linetype = "dashed") +
    theme_minimal()
    
    
    #geom_line(data = fitTbl, aes(x = x, y = upper), colour = "blue", linetype = "dashed") +
    #geom_line(data = fitTbl, aes(x = x, y = lower), colour = "blue", linetype = "dashed")
}

fit <- fitModel(tib, c(2016, 2017, 2018, 2019, 2020))
graphModel(tib, c(2019, 2020), fit)

fit <- fitModel(tib %>% filter(is_definite), c(2016, 2017, 2018, 2019, 2020))
graphModel(tib %>% filter(is_definite), c(2019, 2020), fit)
















fitSig <- nls(cs ~ a / (1 + exp(-b * (as.integer(days_to_year_end) - c))), data = test2, start = list(a = 170, b = 0.5, c = -1000))

sigmoid <- function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}

p <- sigmoid(coef(fitSig), as.integer(test$days_to_year_end))
plot(p, type = "l")
 
numEvents <- tib %>%
  filter(year(date_booked) < 2016) %>%
  group_by(year = year(start_date)) %>%
  count() %>%
  filter(year ==  2017) %>%
  ungroup() %>%
  select(n) %>%
  first()

yearCounts <- tib %>%
  filter(year(start_date) < 2018) %>%
  group_by(year = year(start_date)) %>%
  count()

ggplot(tibLt, aes(x = as.factor(year(date_booked)))) + geom_bar(stat = "count") + facet_wrap(~ year(start_date))
ggplot(tibLt, aes(x = lead_time)) + geom_histogram(aes(y = ..density..)) + facet_wrap(~ year(start_date))
