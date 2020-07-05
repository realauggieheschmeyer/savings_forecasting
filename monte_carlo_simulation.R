# Packages ----------------------------------------------------------------

library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)

# Data --------------------------------------------------------------------

current_investment <- 14501.38

portfolio_tbl <- tribble(
 ~ symbol, ~ weight, ~ annual_fees,
    "VTI",    0.104,        0.0003, # Total Stock Market
    "VTV",    0.027,        0.0004, # Large-Cap Stocks
    "VOE",    0.022,        0.0007, # Mid-Cap Stocks
    "VBR",    0.019,        0.0007, # Small-Cap Stocks
    "VEA",    0.078,        0.0005, # International Developed Markets Stocks
    "VWO",    0.062,        0.0010, # International Emerging Markets Stocks
   "VTIP",    0.029,        0.0005, # Inflation-Protected Bonds
    "AGG",    0.034,        0.0004, # High-Quality Bonds
    "SHV",    0.204,        0.0015, # Short-Term Treasury Bonds
    "MUB",    0.155,        0.0007, # Municipal Bonds
   "JPST",    0.052,        0.0018, # Short-Term Investment-Grade Bonds
   "BNDX",    0.132,        0.0008, # International Developed Markets Bonds
    "EMB",    0.081,        0.0039  # International Emerging Markets Bonds
)

portfolio_close_price_tbl <- portfolio_tbl %>% 
  pull(symbol) %>% 
  tq_get(
    from = Sys.Date() - lubridate::years(10),
    to = Sys.Date()
  ) %>% 
  select(symbol, date, close)

# Calculate Returns -------------------------------------------------------

portfolio_returns_tbl <- portfolio_close_price_tbl %>% 
  group_by(symbol) %>% 
  tq_transmute(
    select     = close,
    mutate_fun = periodReturn,
    period     = "weekly",
    col_rename = "weekly_returns"
  ) %>% 
  tq_portfolio(
    assets_col   = symbol, 
    returns_col  = weekly_returns,
    weights      = portfolio_tbl %>% select(-annual_fees),
    col_rename   = "weekly_returns",
    rebalance_on = "months"
  )

mean_port_return <- portfolio_returns_tbl %>% 
  pull(weekly_returns) %>% 
  mean()

stddev_port_return <- portfolio_returns_tbl %>% 
  pull(weekly_returns) %>% 
  sd()

# Simulate Returns --------------------------------------------------------

simulate_returns <- function(init_value, N, mean, stdev, 
                             period_contribution = 0) {
  
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>%
    `colnames<-`("returns") %>%
    mutate(growth = accumulate(
      returns,
      function(x, y) (x + period_contribution) * y
    )
    ) %>%
    select(growth)
  
}

simulation_period <- (as.Date("2022-03-20") - Sys.Date()) %>% 
  as.numeric() %>% 
  `/`(7) %>% 
  round()

n_simulations <- 1e3

monte_carlo_tbl <- rerun(
  .n = n_simulations,
  simulate_returns(
    current_investment,
    simulation_period,
    mean_port_return,
    stddev_port_return,
    period_contribution = 300
  )
) %>%
  simplify_all() %>%
  `names<-`(paste("sim", 1:n_simulations, sep = "_")) %>%
  as_tibble() %>%
  mutate(week = seq(0, simulation_period, by = 1)) %>%
  select(week, everything()) %>% 
  pivot_longer(
    cols      = contains("sim"),
    names_to  = "simulation",
    values_to = "portfolio_value"
  )

# Visualize Simulations ---------------------------------------------------

monte_carlo_summary_tbl <- monte_carlo_tbl %>%
  group_by(week) %>%
  summarise(
    min = min(portfolio_value),
    median = median(portfolio_value),
    mean = mean(portfolio_value),
    max = max(portfolio_value)
  ) %>%
  pivot_longer(
    -week,
    names_to = "metric",
    values_to = "portfolio_value"
  )

monte_carlo_summary_tbl %>% 
  filter(week == max(week)) %>% 
  mutate(
    current_investment = current_investment + (300 * simulation_period),
    return = portfolio_value - current_investment,
    return_pct = return / current_investment
  )

monte_carlo_tbl %>% 
  ggplot(aes(week, portfolio_value, group = simulation)) +
  geom_line(alpha = .1) +
  geom_line(
    data = monte_carlo_summary_tbl,
    aes(x = week, y = portfolio_value, color = metric),
    inherit.aes = FALSE
  ) +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = "Simulated Portfolio Value",
    subtitle = str_c("After ", simulation_period, " Weeks"),
    x = "Week",
    y = "Portfolio Value",
    color = ""
  )

monte_carlo_tbl %>% 
  filter(week == max(week)) %>% 
  mutate(current_investment = current_investment + (300 * simulation_period),
         return = portfolio_value - current_investment) %>% 
  ggplot(aes(x = return)) +
  geom_histogram(bins = sqrt(n_simulations), fill = palette_light()[3]) +
  theme_tq() +
  scale_x_continuous(labels = scales::dollar)

monte_carlo_tbl %>% 
  filter(week == max(week)) %>% 
  mutate(current_investment = current_investment + (300 * simulation_period),
         return = portfolio_value - current_investment) %>% 
  summarize(pct_greater_than_zero = sum(return > 0) / n())

                 