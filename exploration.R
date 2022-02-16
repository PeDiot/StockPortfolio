# --------- French Stock - Data Exploration --------

# ----- Setup -----

library(tidyquant)        # retrieve financial data from Yahoo Finance API
library(tidyverse)
library(data.table)
library(ggplot2)
library(plotly)
library(TTR)

theme_set(theme_minimal())

source("setup.R")

assets <- c("LVMH",  
            "Air Liquide",
            "L'Oréal",  
            "Renault", 
            "Ubisoft", 
            "Orange", 
            "Carrefour",
            "TF1", 
            "EDF", 
            "OVH Groupe")

selection <- french_stocks %>%
  filter(Name %in% assets) %>%
  select(c(Name, Symbol))

tickers <- selection %>%
  pull(Symbol)
names(tickers) <- selection %>%
  pull(Name) 

num_shares <- c(5, 
                10, 
                2, 
                1, 
                25, 
                12, 
                3, 
                3, 
                5, 
                7)
names(num_shares) <- selection %>%
  pull(Symbol)

asset <- "OVH Groupe"
prices <- get_tq_data(tickers[asset])

assets_data <- get_tq_data(tickers = tickers)

# ----- Value per asset ------

assets_value <- compute_assets_value(data = assets_data, 
                                     num_shares = num_shares)

assets_value_evolution(assets_value)


# ----- Moving Average (MA) -----

# add short-term MA (MA20) and long-run MA (MA50)
prices <- prices %>%
  add_moving_avg(window = 20) %>%
  add_moving_avg(window = 50) %>%
  get_ma_signals()

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


# ----- Relative Strength Index (RSI) -----

prices <- prices %>%
  add_rsi() %>%
  get_rsi_signals()


# ----- Using plotly -----

start_date <- get_start_date(period = "24m", 
                             price_data = prices)
title <- get_title(asset = asset, 
                   start_date = start_date)

p <- prices %>%
  candlestick_chart() %>%
  add_moving_averages_trace() %>%
  add_macd_trace() %>% 
  add_rsi_trace() %>%
  add_layout(title = title)

p

