# --------- French Stock - Data Exploration --------

# ----- Setup

library(tidyquant)
library(tidyverse)
library(ggplot2)
library(plotly)

theme_set(theme_minimal())

backup <- "./backup/"
data <- read_csv(file = paste0(backup, "FrenchStocks.csv"))
tickers <- data %>%
  pull(Symbol)

# ----- First manipulation 


ticker <- tickers[1]
prices <- tq_get(x = ticker, 
                 get = "stock.prices", 
                 from = today() - months(12),
                 to = today(),
                 complete_cases = T)

date_breaks <- c("1m" = "1 month", 
                 "2m" = "2 months", 
                 "2w" = "2 weeks", 
                 "1w" = "1 week", 
                 "3d" = "3 days")
periods <- c("1m" = 1, 
             "3m" = 3, 
             "6m" = 6, 
             "9m" = 9, 
             "12m" = 12)

# stock evolution plot
start_date <- today() - months(periods["1m"])
title <- paste(ticker, "from", start_date, "to", today())

p1 <- prices %>%
  filter(date >= start_date) %>%
  ggplot(aes(x = date, 
             y = close)) +
  geom_line(size = .7, 
            color = "#709ABE") + 
  scale_x_date(breaks = date_breaks["3d"]) +
  labs(x = "Date", 
       y = "Close ($)", 
       title = title) +
  theme(axis.text.x = element_text(angle = 45))

ggplotly(p1)

# candlestick plot 
start_date <- today() - months(periods["3m"])
title <- paste(ticker, "from", start_date, "to", today())

p2 <- prices %>%
  filter(date >= start_date) %>%
  mutate(greenRed=ifelse(open-close>0,
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
  scale_x_date(breaks = date_breaks["1w"]) +
  scale_color_manual(values = c("Forest Green", "Red")) +
  labs(x = "Date", 
       y = "Close ($)", 
       title = title) +
  theme(legend.position ="none",
        axis.text.x = element_text(angle = 45))

ggplotly(p2)
