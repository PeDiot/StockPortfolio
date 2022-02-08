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

# ----- Moving Average (MA) -----

company <- "Orange"
prices <- get_tq_data(company)

# add short-term MA (MA20) and long-run MA (MA50)
prices <- prices %>%
  add_moving_avg(window = 20) %>%
  add_moving_avg(window = 50) %>%
  get_ma_signals()

moving_avg_plot(company = company, 
                price_data = prices, 
                period = "6m") 

# Strategy:
  # Buy (sell) when the short-term moving average (MA20) 
  # is crossing above (below) the long-run moving average (MA50).

# ----- Moving Average Convergence Divergence (MACD) -----

# MACD calculation example
  # 12 period EMA - 26 period EMA = MACD
  # 9 period EMA of the MACD = signal line
# Strategy: buy (sell) when MACD crosses above (below) the signal line 

prices <- prices %>%
  add_macd() %>%
  get_macd_signals()

macd_plot(company = company, 
          price_data = prices, 
          period = "12m")

