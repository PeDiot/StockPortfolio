# --------- Setup for Financial Analysis --------

options(warn=-1) # turn warnings off

# ----- French companies data -----
backup <- "./backup/"
french_stocks <- read_csv(file = paste0(backup, "FrenchStocks.csv"), 
                 show_col_types = FALSE)

# ----- Colors -----

evolution <- "#709ABE"
high <- "Forest Green"
low <- "Red"
short <- "purple"
long <- "black"
macd <- "#C47D72"
rsi <- "#F3B0DE"
pal <- "Set3"

# ----- GGplot theme -----

theme_set(theme_minimal())

# ----- Dates -----

periods <- c("2w" = 1, 
             "1m" = 1, 
             "3m" = 3, 
             "6m" = 6, 
             "9m" = 9, 
             "12m" = 12, 
             "24m" = 24)

# ----- Utils -----

get_asset <- function(ticker){
  "Return ticker for a given asset."
  
  french_stocks %>%
    filter(Symbol == ticker) %>%
    pull(Name)
}

get_ticker <- function(asset){
  "Return asset for a given ticker."
  
  french_stocks %>%
    filter(Name == asset) %>%
    pull(Symbol) 
}

get_asset_last_value <- function(ticker, assets_value){
  "Return ticker's last value given its price and number of shares."
  assets_value[assets_value$ticker == ticker,]$value %>% 
    tail(1)
}

get_start_date <- function(period, price_data){
  "Return start date based on period."
  
  if ("w" %in% period){
    start_date <- today() - weeks(periods[period])
  }
  else {
    start_date <- today() - months(periods[period])
  }
  min_date <- price_data %>%
    pull(date) %>%
    min()
  if (start_date < min_date){
    return(min_date)
  }
  else{ return(start_date) }
  
}

get_title <- function(ticker, start_date){
  "Make title."
  asset <- get_asset(ticker)
  title <- paste0(ticker, 
                 " (", 
                 asset,
                 "): ", 
                 start_date, 
                 " / ", 
                 today())
  return(title)
}

get_tq_data <- function(tickers, num_months = 12){
  "Return price data for given asset and period."
  
  if (length(tickers) == 1){
    tq_get(x = tickers, 
           get = "stock.prices", 
           from = today() - months(num_months),
           to = today(),
           complete_cases = T)
  }
  else {
    dat <- tq_get(x = tickers, 
                  get = "stock.prices", 
                  from = today() - months(num_months),
                  to = today(),
                  complete_cases = T)
    colnames(dat)[1] <- "ticker"  
    df_list <- split(dat, dat$ticker)
    return(df_list)
  }
  
}

clean_assets_value <- function(assets_value){
  "Modify assets value df for vizualisation."
  
  tickers <- assets_value %>%
    pull(ticker) %>%
    unique()
  labels <- lapply(
    tickers,
    function(ticker){
      last_val <- get_asset_last_value(ticker, 
                                        assets_value) %>%
        round(1)
      asset <- get_asset(ticker)
      paste0(asset, " (", last_val, "$)")
    }
  ) %>% unlist()
  names(labels) <- tickers
  
  assets_value %>%
    mutate(ticker = revalue(ticker, labels))
    
}

# ----- Portfolio Value -----

compute_assets_value <- function(data, num_shares){
  "Return assets' value given prices and number of shares."
  
  lapply(
    data, 
    function(df){
      ticker <- df %>%
        pull(ticker) %>%
        unique()
      df %>%
        mutate(value = close * num_shares[ticker]) 
    }
  ) %>%
    bind_rows()
  
}

get_portfolio_value <- function(assets_value){
  "Return portfolio value given assets' prices and number of shares."
  
  assets_value %>%
   group_by(date) %>%
    summarise(value = sum(value))
  
}

# ----- Financial indicators -----

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
  "Identify buying and selling signals from MACD."
  
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
    stop("You need to calculate MACD and signal.")
  }
  
}


add_rsi <- function(
  price_data, 
  rsi_period = 10, 
  ma_type = "WMA"
){
  "Compute Relative Strength Index (RSI) for given period and prices."
  
  price_data %>%
    mutate(RSI = RSI(price = close, 
                     n = 14, 
                     maType = ma_type))
  
}

get_rsi_signals <- function(
  price_data,
  lower_thresold = 30, 
  upper_thresold = 70
){
  "Identify buying and selling signals from RSI."
  
  if ("RSI" %in% colnames(price_data)){
    price_data %>%
      mutate(
        BuyRSI = case_when(
          RSI < lower_thresold ~ 1, 
          TRUE ~ 0
        ), 
        SellRSI = case_when(
          RSI > upper_thresold ~ 1, 
          TRUE ~ 0
        )
      )
  }
  else{
    stop("You need to calculate RSI.")
  }
  
}

# ----- Data Viz -----

range_selector_period <- function(x_pos = .5, y_pos){
  "Plotly buttons to select period."
  
  list(visible = TRUE, x = x_pos, y = y_pos,
       xanchor = "center", yref = "paper",
       font = list(size = 9),
       buttons = list(
         list(count=1,
              label="ALL",
              step="all"),
         list(count=6,
              label="6 MO",
              step="month",
              stepmode="backward"),
         list(count=3,
              label="3 MO",
              step="month",
              stepmode="backward"),
         list(count=1,
              label="1 MO",
              step="month",
              stepmode="backward")
       ))
  
}

candlestick_chart <- function(price_data, legend_group = "one"){
  "Build plotly candlestick chart for a given asset."
  
  p <- plot_ly(price_data) %>%
    add_trace(type = "candlestick",
              x = ~date,
              open = ~open, 
              high = ~high, 
              low = ~low, 
              close = ~close,
              name = "Close", 
              increasing = list(line = list(color = high,
                                            width = 1.5)), 
              decreasing = list(line = list(color = low,
                                            width = 1.5)), 
              legendgroup = legend_group)
}

add_moving_averages_trace <- function(plot, legend_group = "one"){
  "Add MA20 and MA50."
  
  plot %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MA20,
              name = "MA20",
              yaxis = "y1", 
              line = list(color = short, 
                          width = .7),
              legendgroup = "one", 
              hoverinfo = "none", 
              legend_group = legend_group) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MA50,
              name = "MA50",
              yaxis = "y1", 
              line = list(color = long, 
                          dash = "dot", 
                          width = .7), 
              legendgroup = legend_group)
}

add_macd_trace <- function(plot, legend_group = "two"){
  "Add MACD and MACD signal on a new chart."
  
  plot %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MACD,
              name = "MACD",
              yaxis = "y2", 
              line = list(color = macd,
                          width = 1.3), 
              legendgroup = legend_group) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~Signal,
              name = "MACD Signal",
              yaxis = "y2", 
              line = list(color = long,
                          width = 1,
                          dash = "dot"), 
              legendgroup = legend_group) 
}

add_rsi_trace <- function(plot, legend_group = "three"){
  "Add RSI signal and bounds on a new chart."
  
  plot %>% 
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~RSI,
              name = "RSI",
              yaxis = "y3", 
              line = list(color = rsi,
                          width = 1.5), 
              legendgroup = legend_group) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = c(~min(date), ~max(date)),
              y = c(70,70),
              name = "Upper RSI (70)",
              line = list(color = "red",
                          width = 0.5,
                          dash = "dot"),
              yaxis = "y3", 
              legendgroup = legend_group) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = c(~min(date), ~max(date)),
              y = c(30,30),
              name = "Lower RSI (30)",
              line = list(color = "red",
                          width = 0.5,
                          dash = "dot"),
              yaxis = "y3", 
              legendgroup = legend_group) 
}

indicators_plot <- function(price_data, title){
  "Make plotly layout."
  price_data %>%
    candlestick_chart() %>%
    add_moving_averages_trace() %>%
    add_macd_trace() %>% 
    add_rsi_trace() %>%
    layout(title = title,
           xaxis = list(rangeslider = list(visible = F), 
                        rangeselector = range_selector_period(y_pos = -0.055), 
                        title = ""),
           yaxis = list(domain = c(0.68, 1),
                        fixedrange = FALSE, 
                        title = "$"),
           yaxis2 = list(domain = c(0.32, 0.58),
                         fixedrange = FALSE, 
                         title = "%"), 
           yaxis3 = list(domain = c(0., 0.28),
                         fixedrange = FALSE, 
                         title = ""),
           legend = list(orientation = "h", x = 0.5, y = -.07,
                         xanchor = "center", yref = "paper",
                         font = list(size = 12),
                         bgcolor = "transparent", 
                         borderwidth = .2)) 
}

portfolio_evolution <- function(portfolio_data, title){
  "Plot portfolio value evolution."
  
  plot_ly(portfolio_data) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~value,
              line = list(color = evolution,
                          width = 1.5)) %>%
    layout(title = title, 
           xaxis = list(rangeslider = list(visible = F), 
                        rangeselector = range_selector_period(y_pos = -.15), 
                        title = ""), 
           yaxis = list(title = "$")) 
  
}

assets_value_evolution <- function(assets_value){
  "Plot each asset's value evolution through time."
  
  p <- assets_value %>%
    clean_assets_value() %>%
    ggplot(aes(x = date, 
               y = value, 
               color = ticker)) +
    geom_line(size = .5) +
    scale_color_brewer(palette = pal) +
    facet_wrap(~ ticker, 
               scales = "free_y", 
               nrow = 2, ncol = 5) +
    labs(x = "", 
         y = "Value ($)") +
    theme(axis.text.x = element_text(angle = 45), 
          legend.position = "none")
  ggplotly(p) 
  
}

# ----- UI -----

num_shares_input <- function(asset1, asset2){
  "Build shiny numeric input for number of shares."
  
  inputId1 <- paste("num_shares", get_ticker(asset1), sep = "_")
  inputId2 <- paste("num_shares", get_ticker(asset2), sep = "_")
  fluidRow(
    column(width = 6, 
           numericInput(inputId1,
                        label = h5(asset1), 
                        min = 0, 
                        value = 1)), 
    column(width = 6,
           numericInput(inputId2, 
                        label = h5(asset2), 
                        min = 0, 
                        value = 1))
  )
}
  