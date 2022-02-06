# --------- French Stock - Data Exploration --------

# ----- Setup -----

library(tidyquant)
library(tidyverse)
library(ggplot2)
library(plotly)

theme_set(theme_minimal())

backup <- "./backup/"
data <- read_csv(file = paste0(backup, "FrenchStocks.csv"))

companies <- data %>%
  select(c(Name, Symbol, Rank)) 

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

# ----- Functions -----

get_start_date <- function(period){
  "Return start date based on period."
  
  if ("w" %in% period){
    start_date <- today() - weeks(periods[period])
  }
  else {
    start_date <- today() - months(periods[period])
  }
  return(start_date)
  
}

get_tq_data <- function(company, num_months = 12){
  "Return price data for given company and period."
  
  ticker <- companies %>%
    filter(Name == company) %>%
    pull(Symbol)
  tq_get(x = ticker, 
         get = "stock.prices", 
         from = today() - months(num_months),
         to = today(),
         complete_cases = T)
  
}

stock_evolution_plot <- function(
  company, 
  price_data, 
  period = "12m"
){
  "Return evolution plot for given company and period."
  
  start_date <- get_start_date(period = period)
  ticker <- companies %>%
    filter(Name == company) %>%
    pull(Symbol)
  title <- paste(ticker, "(", company, ") from", start_date, "to", today())
  
  p <- price_data %>%
    filter(date >= start_date) %>%
    ggplot(aes(x = date, 
               y = close)) +
    geom_line(size = .7, 
              color = "#709ABE") + 
    scale_x_date(breaks = date_breaks[period]) +
    labs(x = "Date", 
         y = "Close ($)", 
         title = title) +
    theme(axis.text.x = element_text(angle = 45))
  ggplotly(p)
  
}

candlestick_plot <- function(
  company, 
  price_data,
  period = "12m"
){
  "Return candlestick chart for given company and period."
  
  start_date <- get_start_date(period = period)
  ticker <- companies %>%
    filter(Name == company) %>%
    pull(Symbol)
  title <- paste(ticker, "(", company, ") from", start_date, "to", today())
  
  p <- price_data %>%
    filter(date >= start_date) %>%
    mutate(greenRed = ifelse(open - close > 0,
                             "Red",
                             "Green")) %>% 
    ggplot() +
    geom_segment(aes(x = date,
                     xend = date,
                     y = open,
                     yend = close,
                     colour = greenRed),
                 size = 3) +
    geom_segment(aes(x = date,
                     xend = date,
                     y = high,
                     yend = low,
                     colour = greenRed)) +
    geom_line(aes(x = date, 
                  y = close), 
              size = .6, 
              color = "black") +
    scale_x_date(breaks = date_breaks[period]) +
    scale_color_manual(values = c("Forest Green", "Red")) +
    labs(x = "Date", 
         y = "Close ($)", 
         title = title) +
    theme(legend.position ="none",
          axis.text.x = element_text(angle = 45))
  
  ggplotly(p)
  
}


# ----- First manipulation -----

company <- "Dior"
prices <- get_tq_data()
stock_evolution_plot(ticker = ticker, price_data = prices)
candlestick_plot(ticker = ticker, price_data = prices)

