# --------- French Stock - Data Exploration --------

# ----- Setup -----

library(tidyquant)
library(tidyverse)
library(ggplot2)
library(plotly)

theme_set(theme_minimal())

source("setup.R")

companies <- data %>%
  pull(Name) 

# ----- First manipulation -----

company <- "OVH Groupe"
prices <- get_tq_data(company)
stock_evolution_plot(company, 
                     price_data = prices, 
                     period = "2w")
candlestick_plot(company, 
                 price_data = prices, 
                  period = "2w")

