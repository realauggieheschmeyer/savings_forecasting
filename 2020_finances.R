# Packages ----
library(fable.prophet)
library(googlesheets4)
library(lubridate)
library(tidyquant)
library(tidyverse)
library(timetk)
library(tsibble)

gs4_auth(email = "auggieheschmeyer@gmail.com")
theme_set(tidyquant::theme_tq())

# Data ----

spending_2020_raw <- read_sheet(
  "1lEjlNtcX_Nib786HuNfAwg3wmn5_yTp4cjNl_iUZWTQ",
  sheet = "2020",
  range = "A1:D1000"
) 

spending_2020_tbl <- spending_2020_raw %>% 
  janitor::clean_names() %>% 
  filter(!is.na(amount)) %>% 
  mutate(date = as.Date(date))

# Exploring Spending ----

spending_2020_tbl %>%
  group_by(week_of = floor_date(date, "week")) %>%
  summarize(total = sum(amount)) %>%
  ggplot(aes(week_of, cumsum(total))) +
  geom_line() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = palette_light()[3]
  ) +
  theme_tq() +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_tq() +
  labs(
    title = "2020 Net Income",
    subtitle = "Week Over Week Growth",
    x = "Week",
    y = "Net Income"
  )

spending_2020_tbl %>%
  group_by(type_of_transaction) %>% 
  summarize(total = sum(amount)) %>% 
  mutate(monthly_avg = total / 5,
         annual_projection = monthly_avg * 12) %>% 
  summarize(net = sum(annual_projection))

# Forecasting Income ----

spending_2020_tsbl <- spending_2020_tbl %>% 
  summarize_by_time(date, .by = "day", total = sum(amount)) %>% 
  as_tsibble(index = date) %>% 
  fill_gaps(total = 0)

spending_2020_tsbl %>%
  plot_time_series(
    date,
    total,
    .smooth = FALSE,
    .title = "2020 Daily Net Income",
    .y_lab = "Net Income",
    .interactive = TRUE
  )

spending_2020_tsbl %>% 
  plot_anomaly_diagnostics(
    date, 
    total,
    .interactive = TRUE
  )

spending_2020_tsbl %>%
  plot_seasonal_diagnostics(
    date, 
    total, 
    .interactive = TRUE
  )

spending_2020_tsbl %>%
  plot_acf_diagnostics(
    date, 
    total, 
    .lags = "3 months", 
    .interactive = TRUE
  )

fit_prophet <- spending_2020_tsbl %>% 
  model(
    prophet = prophet(total ~ season("month", order = 2))
  )

fit_prophet %>% 
  accuracy()

fit_prophet %>%
  components() %>% 
  autoplot()

spending_pred_tsbl <- fit_prophet %>% 
  forecast(h = lubridate::interval(
    start = max(spending_2020_tsbl$date),
    end = as_date("2020-12-31")
  ) %>%
    as.numeric("days")) %>% 
  as_tsibble() %>% 
  select(date, total)

spending_2020_tsbl %>% 
  mutate(.type = "actual") %>% 
  bind_rows(spending_pred_tsbl %>% 
              mutate(.type = "prediction")) %>% 
  ggplot(aes(date, cumsum(total), color = .type)) +
  geom_line() +
  theme_tq() +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_tq() +
  labs(
    title = "Projected 2020 Net Income",
    subtitle = "Day-Over-Day Growth",
    x = "Date",
    y = "Net Income",
    color = ""
  )
