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
    definite_time = as.integer(date_definite - date_booked))

ggplot(tib, aes(x = year(start_date), fill = is_definite)) + geom_bar()

ggplot(tib %>% filter(year(date_booked) < 2018), 
       aes(x = year(start_date), fill = is_definite)) +
  geom_bar()

tib %>%
  #filter(year(start_date) < 2018) %>%
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

probs <- mapply(pgamma, bsParam$t[, 1], bsParam$t[, 2], MoreArgs = list(q = 2000))
hist(sapply(probs, function (x) 14 + ((runif(1, 150, 170) - 94) * x) * 0.3))

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

