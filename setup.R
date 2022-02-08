# --------- Setup for Financial Analysis --------

options(warn=-1) # turn warnings off

# ----- French companies data -----
backup <- "./backup/"
data <- read_csv(file = paste0(backup, "FrenchStocks.csv"), 
                 show_col_types = FALSE)

# ----- Colors -----

asset <- "#709ABE"
high <- "Forest Green"
low <- "Red"
short <- "purple"
long <- "black"


# ----- Dates -----

periods <- c("2w" = 1, 
             "1m" = 1, 
             "3m" = 3, 
             "6m" = 6, 
             "9m" = 9, 
             "12m" = 12)

date_breaks <- c("12m" = "1 month", 
                 "9m" = "3 weeks", 
                 "6m" = "2 weeks", 
                 "3m" = "1 week", 
                 "1m" = "3 days", 
                 "2w" = "2 days")

# ----- Functions -----

# --- Utils
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
  
  ticker <- data %>%
    filter(Name == company) %>%
    pull(Symbol)
  tq_get(x = ticker, 
         get = "stock.prices", 
         from = today() - months(num_months),
         to = today(),
         complete_cases = T)
  
}

# --- Data Viz
get_title <- function(ticker, start_date){
  
  title <- paste0(ticker, 
                  " (", 
                  company,
                  ") from ",
                  start_date,
                  " to ",
                  today())
  
}

stock_evolution_plot <- function(
  company, 
  price_data, 
  period = "12m"
){
  "Return evolution plot for given company and period."
  
  start_date <- get_start_date(period = period)
  ticker <- data %>%
    filter(Name == company) %>%
    pull(Symbol)
  
  p <- price_data %>%
    filter(date >= start_date) %>%
    ggplot(aes(x = date, 
               y = close)) +
    geom_line(size = .7, 
              color = asset) + 
    scale_x_date(breaks = date_breaks[period]) +
    labs(x = "Date", 
         y = "Close ($)", 
         title = get_title(ticker, start_date)) +
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
  ticker <- data %>%
    filter(Name == company) %>%
    pull(Symbol)
  
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
              size = .5, 
              color = "black", 
              linetype = "dashed") +
    scale_x_date(breaks = date_breaks[period]) +
    scale_color_manual(values = c(high, low)) +
    labs(x = "Date", 
         y = "Close ($)", 
         title = get_title(ticker, start_date)) +
    theme(legend.position ="none",
          axis.text.x = element_text(angle = 45))
  
  ggplotly(p)
  
}
  

moving_avg_plot <- function(
  company, 
  price_data, 
  MAshort = "MA20", 
  MAlong = "MA50", 
  period = "12m"
){
  "Return a chart with price and moving averages."
  
  start_date <- get_start_date(period = period)
  ticker <- data %>%
    filter(Name == company) %>%
    pull(Symbol)
  sells <- price_data %>%
    filter(date >= start_date
           & MASell == 1) 
  buys <- price_data %>%
    filter(date >= start_date
           & MABuy == 1) 
  
  p <- price_data %>%
    filter(date >= start_date) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = close, 
                  colour = "Close"), 
              size = .7) +
    geom_line(aes_string(y = MAshort, 
                         colour = shQuote(MAshort)), 
              linetype = "dashed", 
              size = .5) +
    geom_line(aes_string(y = MAlong, 
                         colour = shQuote(MAlong)),
              linetype = "dashed", 
              size = .5) +
    geom_point(data = sells, 
               aes(x = date, 
                   y = close, 
                   colour = "Selling signals"), 
               shape = 25,
               size = 3) +
    geom_point(data = buys, 
               aes(x = date, 
                   y = close, 
                   colour = "Buying signals"), 
               shape = 24,
               size = 3) +
    scale_x_date(breaks = date_breaks[period]) +
    scale_color_manual("Value",
                       values = c("Close" = asset, 
                                  MAshort = short,
                                  MAlong = long,
                                  "Selling signals" = low, 
                                  "Buying signals" = high)) +
    labs(x = "Date",
         y = "Value ($)",
         title = get_title(ticker, start_date)) +
    theme(legend.title = element_blank(), 
          legend.position = "bottom", 
          axis.text.x = element_text(angle = 45)) 
  
  ggplotly(p) %>%
    layout(legend = list(orientation = "h", 
                         x = .3, 
                         y = -0.2, 
                         title = "none"))
  
}

macd_plot <- function(
  company, 
  price_data, 
  period = "12m"
){
  "Plot MACD and signal line."
  
  start_date <- get_start_date(period = period)
  ticker <- data %>%
    filter(Name == company) %>%
    pull(Symbol)
  sells <- price_data %>%
    filter(date >= start_date
           & MACDSell == 1) 
  buys <- price_data %>%
    filter(date >= start_date
           & MACDBuy == 1)
  
  p <- price_data %>%
    filter(date >= start_date) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = MACD, 
                  color = "MACD"), 
              size = .6) +
    geom_line(aes(y = Signal, 
                  color = "Signal"), 
              size = .5, 
              linetype = "dashed") +
    geom_point(data = sells, 
               aes(x = date, 
                   y = Signal, 
                   colour = "Selling signals"), 
               shape = 25,
               size = 3) +
    geom_point(data = buys, 
               aes(x = date, 
                   y = Signal, 
                   colour = "Buying signals"), 
               shape = 24,
               size = 3) +
    scale_x_date(breaks = date_breaks[period]) +
    scale_color_manual("Value",
                       values = c("MACD" = asset,
                                  "Signal" = long, 
                                  "Selling signals" = low, 
                                  "Buying signals" = high)) +
    labs(x = "Date",
         y = "% change",
         title = get_title(ticker, start_date)) +
    theme(legend.title = element_blank(), 
          legend.position = "bottom", 
          axis.text.x = element_text(angle = 45)) 
  
  ggplotly(p) %>%
    layout(legend = list(orientation = "h", 
                         x = .35, 
                         y = -0.2, 
                         title = "none"))

}


# --- Indicators

add_moving_avg <- function(
  price_data,
  window, 
  align = "right"
){
  "Return price data with moving average for given period and prices."
  
  new_name <- paste0("MA", as.character(window))
  price_data %>%
    mutate(MA = rollmean(x = close,
                         k = window,
                         fill = NA,
                         align = align)) %>%
    as.data.table() %>%
    setnames(old = "MA", new = new_name) %>%
    as.data.frame()
  
}

get_ma_signals <- function(
  price_data, 
  MAshort = "MA20",
  MAlong = "MA50"
){
  "Identify buying and selling signals from moving average."

  col_names <- colnames(price_data)
  if (MAshort %in% col_names & MAlong %in% col_names){
    price_data %>%
      mutate(
        MABuy = case_when(
          ((!!sym(MAshort)) > (!!sym(MAlong)) & lag((!!sym(MAshort))) < lag((!!sym(MAlong)))) ~ 1, 
          TRUE ~ 0
        ),
        MASell = case_when(
          ((!!sym(MAshort)) < (!!sym(MAlong)) & lag((!!sym(MAshort))) > lag((!!sym(MAlong)))) ~ 1, 
          TRUE ~ 0
        )
      )
  }
  else{
    stop(paste0("You need to calculate ", MAshort, " and ", MAlong, "."))
  }
  
}

add_macd <- function(
  price_data, 
  ema_short = 12, 
  ema_long = 26, 
  signal = 9, 
  percent = T
){
  "Return MACD and signal values for given period and prices."
  
  if (percent == T){
    macd <- MACD(x = price_data %>% pull(close), 
                 nFast = ema_short, 
                 nSlow = ema_long, 
                 nSig = signal, 
                 percent = T)
  }
  else{
    macd <- MACD(x = price_data %>% pull(close), 
                 nFast = ema_short, 
                 nSlow = ema_long, 
                 nSig = signal, 
                 percent = F)
  }
  
  price_data %>%
    mutate(MACD = macd[, 1], 
           Signal = macd[, 2])
  
}

get_macd_signals <- function(price_data){
  
  col_names <- colnames(price_data)
  if ("MACD" %in% col_names & "Signal" %in% col_names){
    price_data %>%
      mutate(
        MACDBuy = case_when(
          MACD > Signal & lag(MACD) < lag(Signal) ~ 1, 
          TRUE ~ 0
        ), 
        MACDSell = case_when(
          MACD < Signal & lag(MACD) > lag(Signal) ~ 1, 
          TRUE ~ 0
        )
      )
  }
  else{
    stop("You need to calculate MACD and MACD signal.")
  }
  
}
