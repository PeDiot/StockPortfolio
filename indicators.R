# --------- French Stock - Financial Indicators --------

# Moving Average
# MACD (Moving Average Convergence Divergence)
# RSI (Relative Strength Index)

# ----- Setup -----

library(tidyquant)
library(tidyverse)
library(ggplot2)
library(plotly)
library(data.table)

source("setup.R")

theme_set(theme_minimal())

companies <- data %>%
  pull(Name)

date_breaks <- c("12m" = "1 month", 
                 "9m" = "1 month", 
                 "6m" = "3 weeks", 
                 "3m" = "2 week", 
                 "1m" = "3 days", 
                 "1w" = "1 day")
periods <- c("2w" = 1, 
             "1m" = 1, 
             "3m" = 3, 
             "6m" = 6, 
             "9m" = 9, 
             "12m" = 12)

# ----- Moving Average (MA) -----

company <- "Hermès"
prices <- get_tq_data(company)

# add  short-term MA (MA20) and long-run MA (MA50)
prices <- prices %>%
  add_moving_avg(window = 20) %>%
  add_moving_avg(window = 50)

moving_avg_plot(company = "Hermès", 
                price_data = prices)

# Strategy:
  # Buy (sell) when the short-term moving average (MA20) 
  # is crossing above (below) the long-run moving average (MA50).

