# Setup -------------------------------------------------------

options(warn = -1)

## Credentials -------------------------------------------------------
credentials <- data.frame(
  user = c("zeujahh"),
  password = c("1995poSSe*"),
  admin = TRUE,
  comment = "Simple and secure authentification mechanism for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

## Colors -------------------------------------------------------

evolution <- "#709ABE"
high <- "Forest Green"
low <- "Red"
short <- "#72C4D7"
medium <- "purple"
long <- "black"
macd <- "#C47D72"
macdHist <- "#B5CDD2"
rsi <- "#F3B0DE"
bbands <- "#CACED2"
pctBB <- "#5EB6B1"
pred <- "#A48BAB"

## GGplot theme -------------------------------------------------------

theme_set(theme_minimal())

## Assets -------------------------------------------------------

stock_tickers <- c("1QZ.F", 
                   "MC.PA",
                   "OR.PA", 
                   "AI.PA", 
                   "ORA.PA", 
                   "EDF.PA",
                   "CA.PA", 
                   "RNO.PA", 
                   "UEN.F", 
                   "OVH.PA", 
                   "TFI.PA",
                   "APC.F", 
                   "MSF.F", 
                   "ABEC.F", 
                   "AMZ.F", 
                   "ALUVI.PA", 
                   "COX.PA", 
                   "NAVYA.PA", 
                   "BRK-B", 
                   "KER.PA", 
                   "RMS.PA", 
                   "B1C.F", 
                   "NNND.F", 
                   "FB2A.F")
stock_names <- c("Coinbase", 
                 "LVMH",
                 "L'Oréal", 
                 "Air Liquide", 
                 "Orange", 
                 "EDF", 
                 "Carrefour", 
                 "Renault", 
                 "Ubisoft", 
                 "OVH Groupe", 
                 "TF1", 
                 "Apple", 
                 "Microsoft", 
                 "Alphabet", 
                 "Amazon", 
                 "UV Germi", 
                 "Nicox", 
                 "Navya", 
                 "Berkshire Hathaway", 
                 "Kering", 
                 "Hermès", 
                 "Baidu", 
                 "Tencent", 
                 "Meta Platforms")
names(stock_tickers) <- stock_names

crypto_tickers <- c("BTC-EUR", 
                    "ETH-EUR", 
                    "BNB-EUR", 
                    "SOL-EUR", 
                    "MATIC-EUR", 
                    "MANA-EUR", 
                    "DOGE-EUR", 
                    "BAT-EUR", 
                    "REP-EUR", 
                    "SC-EUR", 
                    "DOT-EUR",
                    "XRP-EUR",
                    "LTC-EUR", 
                    "USDT-EUR", 
                    "USDC-EUR")

crypto_names <- c("Bitcoin", 
                  "Ethereum", 
                  "Binance Coin", 
                  "Solana", 
                  "Polygon (MATIC)", 
                  "Decentraland (MANA)", 
                  "Dogecoin", 
                  "Basic Attention Token", 
                  "Augur", 
                  "Siacoin", 
                  "Polkadot", 
                  "Ripple", 
                  "Litecoin",
                  "Tether", 
                  "USDC")
names(crypto_tickers) <- crypto_names

etf_tickers <- c("^FCHI", 
                 "^GSPC", 
                 "E5T.F")
etf_names <- c("CAC40", 
               "S&P 500", 
               "EUROTECH")
names(etf_tickers) <- etf_names

symbols <- structure(list(
  tickers = c(crypto_tickers, 
              stock_tickers, 
              etf_tickers)
),
  class = "data.frame", 
  row.names = c(crypto_names, 
                stock_names, 
                etf_names)
)


my_tickers <- c("BTC-EUR", 
                "ETH-EUR", 
                "MATIC-EUR", 
                "MANA-EUR", 
                "1QZ.F", 
                "AMZ.F")

my_tickers_ix <- lapply(1:nrow(symbols), 
                        function(ix){
                          if (symbols[ix, "tickers"] %in% my_tickers){
                            return(ix)
                          }
                        }) %>% unlist()

names(my_tickers) <- symbols[my_tickers_ix, ] %>%
  names()

# Utils -------------------------------------------------------

save_data_list <- function(df_list){
  "Save list of dataframes."
  
  names <- names(df_list)
  n_dfs <- length(df_list)
  sapply(seq_along(1:n_dfs), 
         function(i){
           write_feather(
             x = df_list[[i]] %>%
               filter(date >= today() - months(12)), 
             path = paste0("./backup/assets/", names[i],".feather")
           )
         }
  )
  
}

save_num_shares <- function(num_shares){
  "Save number of shares to RData."
  
  dat <- data.frame(num_shares) 
  save(dat, 
       file = paste0(backup, "num_shares.RData"))
  
}

get_ticker <- function(company_name){
  "Return ticker given company_name."
  
  symbols[company_name, "tickers"]
}

get_company_name <- function(ticker){
  "Return company name given ticker."
  
  symbols %>%
    filter(tickers == ticker) %>%
    rownames()
}
get_asset_last_value <- function(ticker, assets_value){
  "Return ticker's last value given its price and number of shares."
  assets_value[assets_value$ticker == ticker,]$value %>% 
    tail(1)
}


get_indicator_plot_title <- function(ticker, indicator_type){
  "Make title ."
  asset <- get_company_name(ticker)
  title <- paste0(asset, 
                  " (", 
                  ticker,
                  ") - ", 
                  indicator_type)
  return(title)
}

clean_tq_data <- function(df){
  "Remove duplicated rows and fill NA values."
  
  df %>%
    distinct(date, 
             .keep_all = T) %>%
    mutate(ticker = na.locf(ticker), 
           close = na.locf(close)) %>%
    complete(date = seq.Date(min(date), today(), by="day")) %>%
    fill(everything())
}

get_tq_data <- function(tickers, start_date){
  "Return price data for given asset and period."
  
  if (length(tickers) == 1){
    
    dat <- tq_get(x = tickers, 
                  get = "stock.prices", 
                  from = start_date,
                  to = today(),
                  complete_cases = T) %>%
      clean_tq_data()
    
    return(dat)
    
  }
  else {
    
    dat <- tq_get(x = tickers, 
                  get = "stock.prices", 
                  from = start_date,
                  to = today(),
                  complete_cases = T)
    colnames(dat)[1] <- "ticker"  
    df_list <- split(dat, dat$ticker) %>%
      lapply(clean_tq_data)
    
    return(df_list)
    
  }
  
}

query_assets_since_buying_date <- function(
  ticker,
  assets_dat,
  buying_dates
){
  "Return asset data since buying date."
  
  asset_dat <- assets_dat[[ticker]]
  buying_date <- buying_dates[ticker]
  asset_dat %>%
    filter(date >= buying_date)
  
}

clean_assets_value <- function(assets_value, portfolio_value){
  "Modify assets value df for vizualisation."
  
  tickers <- assets_value %>%
    pull(ticker) %>%
    unique()
  new_levels <- lapply(
    tickers,
    function(ticker){
      last_val <- get_asset_last_value(ticker, 
                                       assets_value) 
      contrib <- 100 * last_val / portfolio_value
      asset <- get_company_name(ticker)
      paste0(asset, " (", round(contrib, 1), "%)")
    }
  ) %>% unlist()
  names(new_levels) <- tickers
  
  assets_value %>%
    mutate(ticker = revalue(ticker, new_levels))
  
}

cumret_to_percent <- function(cr){
  "Convert floating cumulative return to percent."
  
  if (cr >= 1){
    pct_cr <- 100*(cr - 1) %>%
      round(3)
  }
  else{
    pct_cr <- - 100*(1 - cr) %>%
      round(3)
  }
  return(pct_cr)
  
}

# Value -------------------------------------------------------

compute_assets_value <- function(data, num_shares){
  "Return assets' value given prices and number of shares."
  
  res <- lapply(
    data, 
    function(df){
      ticker <- df %>%
        pull(ticker) %>%
        unique()
      n <- nrow(df)
      n_shares <- num_shares[ticker]
      df %>%
        mutate(n_shares = rep(n_shares, n)) %>%
        mutate(value = close * n_shares) 
    }
  ) 
  
  return(res) 
  
}

get_portfolio_value <- function(assets_value){
  "Return portfolio value given assets' prices and number of shares."
  
  assets_value %>%
    group_by(date) %>%
    summarise(value = sum(value))
  
}

get_current_value <- function(data){
  "Return value (price * number of shares) at last date." 
  
  data %>%
    filter(date == max(date)) %>%
    head(n = 1) %>%
    pull(value) %>%
    round(2) %>%
    format(big.mark = ",", 
           decimal.mark = ".", 
           scientific = F)
}

get_current_price <- function(data){
  "Return price at last date." 
  
  data %>%
    filter(date == max(date)) %>%
    head(n = 1) %>%
    pull(close) %>%
    round(2) %>%
    format(big.mark = ",", 
           decimal.mark = ".", 
           scientific = F)
}


# Assets performance -------------------------------------------------------

calculate_total_returns <- function(p0, p1){
  r <- (p1 - p0) / p0
  return(r)
}

compute_daily_returns <- function(asset_dat, portfolio_dat = NULL){
  "Calculate the daily returns and for our assets."
  
  if ( !(is.null(asset_dat)) ){
    n_tickers <- asset_dat %>% 
      pull(ticker) %>%
      unique() %>%
      length()
    if (n_tickers == 1){
      returns <- asset_dat %>%
        tq_mutate(select = close,
                  mutate_fun = periodReturn,
                  period = "daily",
                  col_rename = "ret") %>%
        select(c(ticker,
                 date, 
                 ret))
    }
    else{
      returns <- asset_dat %>%
        group_by(ticker) %>%
        tq_mutate(select = close,
                  mutate_fun = periodReturn,
                  period = "daily",
                  col_rename = "ret") %>%
        select(c(ticker,
                 date, 
                 ret))
    }
    
  }
  if ( !(is.null(portfolio_dat)) ){
    returns <- portfolio_dat %>%
      tq_transmute(select = value,
                   mutate_fun = periodReturn,
                   period = "daily",
                   col_rename = "ret")
  }
  
  return(returns)
  
}

compute_weighted_returns <- function(ret_data, num_shares){
  "Calculate the weighted average of our asset returns."
  
  n_shares <- sum(num_shares)
  wts_dat <- data.frame(ticker = names(num_shares), 
                        num_shares = num_shares) %>%
    mutate(wts = num_shares / n_shares)
  
  ret_data <- left_join(x = ret_data,
                        y = wts_dat, 
                        by = "ticker")
  
  ret_data %>%
    mutate(wt_return = wts * ret)
  
}

compute_cumulative_returns <- function(ret_data, all = T){
  "Calculate the cumulative returns for the entire portfolio or a specific ticker. "
  
  if (all == T){
    cum_returns <- ret_data %>%
      mutate(cr = cumprod(1 + ret)) 
  }
  else{
    cum_returns <- ret_data %>%
      group_by(ticker) %>%
      mutate(cr = cumprod(1 + ret)) 
  }
  
  return(cum_returns)
  
}

get_current_cumret <- function(cumret_data){
  "Return cumulative returns at last date." 
  
  last_cr <- cumret_data %>%
    filter(date == max(date)) %>%
    pull(cr) %>%
    cumret_to_percent()
  return(last_cr)
  
}

get_best_asset <- function(assets_cumret){
  "Return asset with best cumulative returns as of today."
  
  d <- assets_cumret %>% 
    filter(date == max(date)) %>% 
    ungroup() %>% 
    filter(cr == max(cr))
  l <- list(asset = get_company_name(d$ticker), 
            pct_cr = cumret_to_percent(d$cr))
  return(l) 
  
}

get_worst_asset <- function(assets_cumret){
  "Return asset with worst cumulative returns as of today."
  
  d <- assets_cumret %>% 
    filter(date == max(date)) %>% 
    ungroup() %>% 
    filter(cr == min(cr))
  l <- list(asset = get_company_name(d$ticker), 
            pct_cr = cumret_to_percent(d$cr))
  return(l) 
  
}

# Financial indicators -------------------------------------------------------

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

get_last_ma <- function(price_data, ma){
  "Return last moving average."
  
  price_data %>%
    filter(date == max(date)) %>%
    pull(ma)
  
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

calculate_bbands <- function(
  price_data,
  n_periods = 20,
  sd = 2
){
  "Compute bollinger bands and %B."
  
  closes <- price_data %>% pull(close)
  dates <- price_data %>%
    pull(date)
  
  BBands(HLC = closes, 
         n = n_periods, 
         sd = sd) %>%
    data.frame() %>%
    mutate(date = dates, 
           close = closes) 
  
}

add_macd <- function(
  price_data, 
  ema_short = 12, 
  ema_long = 26, 
  signal = 9, 
  percent = T
){
  "Return MACD and signal values for given period and prices."
  
  days_diff <- today() - min(price_data %>%
                               pull(date))
  
  if (days_diff < ema_long){
    price_data <- price_data %>%
      mutate( MACD = rep(NA, nrow(.)) ) 
  }
  
  else{
    if (percent == T){
      macd_ <- MACD(x = price_data %>% pull(close), 
                    nFast = ema_short, 
                    nSlow = ema_long, 
                    nSig = signal, 
                    percent = T)
    }
    else{
      macd_ <- MACD(x = price_data %>% pull(close), 
                    nFast = ema_short, 
                    nSlow = ema_long, 
                    nSig = signal, 
                    percent = F)
    }
    price_data <- price_data %>%
      mutate(MACD = macd_[, 1], 
             MACDSignal = macd_[, 2], 
             MACDHist = MACD - MACDSignal)
    
  }
  
  return(price_data)
  
}

get_macd_signals <- function(price_data){
  "Identify buying and selling signals from MACD."
  
  col_names <- colnames(price_data)
  if ("MACD" %in% col_names & "MACDSignal" %in% col_names){
    price_data %>%
      mutate(
        MACDBuy = case_when(
          MACDHist > 0 & lag(MACDHist) < 0 ~ 1, 
          TRUE ~ 0
        ), 
        MACDSell = case_when(
          MACDHist < 0 & lag(MACDHist) > 0 ~ 1, 
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
  
  days_diff <- today() - min(price_data %>%
                               pull(date))
  
  if (days_diff < rsi_period){
    price_data <- price_data %>%
      mutate( RSI = rep(NA, nrow(.)) ) 
  }
  
  else{
    price_data <- price_data %>%
      mutate(RSI = RSI(price = close, 
                       n = 14, 
                       maType = ma_type))
  }

  return(price_data)
  
}

get_rsi_signals <- function(
  price_data,
  lower_thresold = 30, 
  upper_thresold = 70, 
  ma_window = 100
){
  "Identify buying and selling signals from RSI for days where closing price > MAlong."
  
  start_date <- price_data %>% 
    pull(date) %>%
    min()
  
  if ( (today() - start_date) > ma_window ) {
    
    if ("RSI" %in% colnames(price_data)){
        price_data %>%
        add_moving_avg(window = 100) %>% 
        filter(close >= MA100) %>% 
        mutate(
          RSIBuy = case_when(
            RSI < lower_thresold ~ 1, 
            TRUE ~ 0
          ), 
          RSISell = case_when(
            RSI > upper_thresold ~ 1, 
            TRUE ~ 0
          )
        )
    }
    
    else{
      stop("You need to calculate RSI.")
    }
    
  }
  
  else{
    stop(paste("You need more than", ma_window, "days."))
  }
  
}


# Data Viz -------------------------------------------------------

range_selector_period <- function(
  x_pos = .5, 
  y_pos
){
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

plotly_legend <- function(x.pos = .5, y.pos = -.15, size = 12){
  list(orientation = "h", x = x.pos, y = y.pos,
       xanchor = "center", yref = "paper",
       font = list(size = 12),
       bgcolor = "transparent", 
       borderwidth = .2)
}

plotly_layout <- function(
  p,
  title,
  title.y, 
  range_selector = T
){
  
  if (range_selector == T){
    p_layout <-p %>%
      layout(title = title,
             xaxis = list(rangeslider = list(visible = F), 
                          rangeselector = range_selector_period(y_pos = -.1),
                          title = ""),
             yaxis = list(fixedrange = FALSE, 
                          title = title.y),
             legend = plotly_legend()) 
  }
  else {
    p_layout <- p %>%
      layout(title = title,
             xaxis = list(rangeslider = list(visible = F), 
                          title = ""),
             yaxis = list(fixedrange = FALSE, 
                          title = title.y),
             legend = plotly_legend(y.pos = -.1)) 
  }
  return(p_layout)
  
}


plot_daily_returns <- function(
  plotly_obj, 
  trace_col, 
  title, 
  legend_group,
  yaxis = NULL
){
  "Plot daily returns evolution."
  
  if (is.null(yaxis)){
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ret,
                name = "Daily returns", 
                yaxis = yaxis, 
                line = list(width = 1.7, 
                            color = trace_col,
                            dash = "dot"), 
                legendgroup = legend_group) %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = c(~min(date), ~max(date)),
                y = c(0, 0), 
                yaxis = yaxis, 
                line = list(color = "black",
                            width = 2), 
                showlegend = F)
  }
  else{
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~ret,
                name = "Daily returns", 
                yaxis = yaxis, 
                line = list(width = 1.7, 
                            color = trace_col,
                            dash = "dot"), 
                legendgroup = legend_group) %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = c(~min(date), ~max(date)),
                y = c(0, 0), 
                yaxis = yaxis, 
                line = list(color = "black",
                            width = 2), 
                showlegend = F)
  }
  
  return(p)
  
}

plot_cumulative_returns <- function(
  plotly_obj, 
  multiple = F, 
  title = "", 
  trace_col = evolution, 
  legend_group,
  yaxis = NULL
){
  "Plot cumulative returns evolution."

  if (multiple == F){
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~cr,
                name = "Cumulative returns", 
                yaxis = yaxis, 
                line = list(width = 1.7, 
                            color = trace_col,
                            dash = "dot"), 
                legendgroup = legend_group)
  }
  else{
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~cr,
                group = ~ticker,
                color = ~ticker, 
                name = ~ticker, 
                yaxis = yaxis, 
                line = list(width = 1.7,
                            dash = "dot"), 
                legendgroup = legend_group)
  }
   
  p <- p %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = c(~min(date), ~max(date)),
              y = c(1, 1), 
              yaxis = yaxis, 
              line = list(color = "black",
                          width = 2), 
              showlegend = F) %>%
    layout(title = title,
           xaxis = list(title = ""),
           yaxis = list(title = ""), 
           legend = plotly_legend())
  
  return(p)
  
}

plot_price_evolution <- function(
  plotly_obj, 
  title, 
  legend_group, 
  trace_name = "Value (€)", 
  yaxis = NULL
){
  "Plot price evolution."
  
  if (is.null(yaxis)){
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~value,
                name = trace_name, 
                line = list(color = evolution,
                            width = 2), 
                legendgroup = legend_group)
  }
  else{
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~value,
                name = trace_name,
                yaxis = yaxis, 
                line = list(color = evolution,
                            width = 1.7), 
                legendgroup = legend_group)
  }
  
  return(p)
  
  
}

plot_evolution <- function(
  price_dat,
  cum_ret_dat = NULL, 
  daily_ret_dat = NULL
){
  "Combine price evolution and cumulative/daily returns plots."
  
  if (is.null(daily_ret_dat)){
    
    last_ret <- cum_ret_dat %>%
      filter(date == max(date)) %>% 
      pull(cr)
    trace_col <- if_else(last_ret < 1, 
                        "red", 
                        "green")
    
    data <- merge(x = price_dat, 
                  y = cum_ret_dat, 
                  by = "date") 
    
    p <- plot_ly(data) %>%
      plot_price_evolution(title = "", 
                           legend_group = "one") %>%
      plot_cumulative_returns(trace_col = trace_col, 
                              title = "", 
                              yaxis = "y2", 
                              legend_group = "two")
    
  }
  if (is.null(cum_ret_dat)){
    
    last_ret <- daily_ret_dat %>%
      filter(date == max(date)) %>% 
      pull(ret)
    trace_col <- if_else(last_ret < 0,
                        "red", 
                        "green")
    
    data <- merge(x = price_dat, 
                  y = daily_ret_dat, 
                  by = "date") 
    
    p <- plot_ly(data) %>%
      plot_price_evolution(title = "", 
                           legend_group = "one") %>%
      plot_daily_returns(trace_col = trace_col, 
                         title = "", 
                         yaxis = "y2", 
                         legend_group = "two")
    
  }
  
  p %>%
    layout(title = "",
           xaxis = list(rangeslider = list(visible = F), 
                        rangeselector = range_selector_period(y_pos = -0.1), 
                        title = ""),
           yaxis = list(domain = c(0.45, 1),
                        fixedrange = FALSE,
                        tickfont = list(color = evolution), 
                        title = "€"), 
           yaxis2 = list(domain = c(0, 0.35),
                         fixedrange = FALSE, 
                         tickfont = list(color = macd), 
                         title = ""), 
           legend = plotly_legend())
  
}

portfolio_composition <- function(assets_value){
  "Return a pie chart with each asset's value."
  
  d <- assets_value %>%
    filter(date == max(date)) %>%
    mutate(ticker = as.factor(ticker)) %>%
    mutate(asset = lapply(ticker, get_company_name))
  tot_val <- sum(d$value)
  d <- d %>%
    mutate(pct = 100 * value / tot_val)
  
  gradient <- colorRampPalette(c("#C9E4EA", "#567FA4"))
  numeric_cut <- cut(d$pct, breaks = nrow(d)) %>%
    as.numeric()
  d <- d %>%
    mutate( colors = gradient(nrow(d))[numeric_cut] )
  
  fig <- d %>%
    plot_ly(labels = ~ticker, 
            values = ~value, 
            type = "pie", 
            textposition = "inside",
            textinfo = "label+percent",
            insidetextfont = list(color = "black"),
            hoverinfo = "text",
            text = ~paste0(asset, " (€", value %>%
                             round(2) %>%
                             format(big.mark = ",", 
                                    decimal.mark = ".", 
                                    scientific = F), 
                           ")"),
            marker = list(colors = ~ colors, 
                          line = list(color = "#FFFFFF", width = 1)),
            showlegend = FALSE)
  
  options <- list(showgrid = FALSE,
                  zeroline = FALSE, 
                  showticklabels = FALSE)
  fig %>%
    layout(title = "", 
           xaxis = options,
           yaxis = options)
}

# --- Indicators


candlestick_chart <- function(ticker, price_data){
  "Build plotly candlestick chart with moving averages for a given asset."
  
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
                                            width = 1.5))) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MA20,
              name = "MA20",
              line = list(color = short, 
                          width = 1.5),
              hoverinfo = "none") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MA50,
              name = "MA50",
              line = list(color = medium,
                          width = 1.5), 
              hoverinfo = "none") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MA100,
              name = "MA100",
              line = list(color = long, 
                          dash = "dot", 
                          width = 1.5), 
              hoverinfo = "none")
  
  title <- get_indicator_plot_title(ticker, 
                                    indicator_type = "Candlestick & Moving Averages")
  p <- p %>%
    plotly_layout(title = title, title.y = "€")
  
  return(p)
  
  
}

bbands_chart <- function(bbands_dat, ticker){
  "Build plotly chart whith prices, bollinger bands and %B."
  p <- plot_ly(bbands_dat) %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~close,
              name = "Close", 
              line = list(color = evolution,
                          width = 1.2), 
              yaxis = "y1") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~mavg,
              name = "MA20", 
              line = list(color = short,
                          dash = "dot", 
                          width = 1.7), 
              yaxis = "y1") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~dn, 
              name = "Lower", 
              line = list(color = bbands,
                          width = .9), 
              yaxis = "y1") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~up, 
              name = "Upper", 
              line = list(color = bbands,
                          width = .9), 
              yaxis = "y1") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~pctB, 
              name = "%B", 
              line = list(color = pctBB,
                          width = 1), 
              yaxis = "y2")
  
  title <- get_indicator_plot_title(ticker, 
                                    indicator_type = "Bollinger Bands")
  p %>%
    layout(title = title,
           xaxis = list(rangeslider = list(visible = F), 
                        rangeselector = range_selector_period(y_pos = -0.1), 
                        title = ""),
           yaxis = list(domain = c(.40, 1),
                        fixedrange = FALSE,
                        tickfont = list(color = evolution), 
                        title = "€"),
           yaxis2 = list(domain = c(0, .30),
                         fixedrange = FALSE,
                         tickfont = list(color = pctBB), 
                         title = ""), 
           legend = plotly_legend())
  
}

macd_chart <- function(ticker, price_data){
  "Build plotly chart for MACD and MACD signal."
  
  p <- price_data %>%
    select(c(date, 
             close, 
             MACD, 
             MACDSignal, 
             MACDHist)) %>%
    rename(value = close) %>%
    plot_ly() %>%
    plot_price_evolution(title = "", 
                         legend_group = "one", 
                         yaxis = "y1") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MACD,
              name = "MACD",
              line = list(color = macd,
                          width = 1.3), 
              legend_group = "two", 
              yaxis = "y2") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MACDSignal,
              name = "MACD Signal",
              line = list(color = long,
                          width = 1,
                          dash = "dot"), 
              hoverinfo = "none", 
              legend_group = "two", 
              yaxis = "y2") %>%
    add_trace(type = "bar", 
              x = ~date,
              y = ~MACDHist,
              name = "MACD Histogram",
              marker = list(color = macdHist),
              hoverinfo = "none", 
              legend_group = "two", 
              yaxis = "y2")
  
  title <- get_indicator_plot_title(ticker, 
                                    indicator_type = "Prices & Moving Average Convergence Divergence")
  p <- p %>%
    layout(title = title,
           xaxis = list(rangeslider = list(visible = F), 
                        rangeselector = range_selector_period(y_pos = -0.1), 
                        title = ""),
           yaxis = list(domain = c(0.45, 1),
                        fixedrange = FALSE,
                        tickfont = list(color = evolution), 
                        title = "€"), 
           yaxis2 = list(domain = c(0, 0.35),
                         fixedrange = FALSE, 
                         tickfont = list(color = macd), 
                         title = "%"), 
           legend = plotly_legend())
  
  return(p)
  
}

rsi_chart <- function(ticker, price_data){
  "Build plotly chart for RSI signal and bounds."
  
  p <- price_data %>%
    select(c(date, 
             close, 
             RSI)) %>%
    rename(value = close) %>%
    plot_ly() %>%
    plot_price_evolution(title = "", 
                         legend_group = "one", 
                         yaxis = "y1") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~RSI,
              name = "RSI",
              line = list(color = rsi,
                          width = 1.2), 
              legend_group = "two",  
              yaxis = "y2") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = c(~min(date), ~max(date)),
              y = c(70,70),
              name = "Upper RSI (70)",
              line = list(color = "red",
                          width = 0.5,
                          dash = "dot"), 
              hoverinfo = "none",  
              legend_group = "two", 
              yaxis = "y2") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = c(~min(date), ~max(date)),
              y = c(30,30),
              name = "Lower RSI (30)",
              line = list(color = "red",
                          width = 0.5,
                          dash = "dot"), 
              hoverinfo = "none", 
              legend_group = "two",  
              yaxis = "y2")
  
  title <- get_indicator_plot_title(ticker, 
                                    indicator_type = "Prices & Relative Strength Index")
  p <- p %>%
    layout(title = title,
           xaxis = list(rangeslider = list(visible = F), 
                        rangeselector = range_selector_period(y_pos = -0.1), 
                        title = ""),
           yaxis = list(domain = c(0.45, 1),
                        fixedrange = FALSE,
                        tickfont = list(color = evolution), 
                        title = "€"), 
           yaxis2 = list(domain = c(0, 0.35),
                         fixedrange = FALSE, 
                         tickfont = list(color = rsi), 
                         title = ""), 
           legend = plotly_legend())
  
  return(p)
  
}

ic_alpha <- function(alpha, acf_res){
  "Confidence interval for ACF."
  
  ic <- qnorm((1 + (1 - alpha))/2)/sqrt(acf_res$n.used)
  return(ic)
  
}


# Stock Recommender -------------------------------------------------------

## Compute all indicators --------------------------------------------------------------

add_indicators <- function(
  price_dat, 
  start_date, 
  ema_short = 12, 
  ema_long = 26, 
  macd_signal = 9, 
  rsi_period = 10
){
  "Add MACD, RSI and signals to price data."
  
  price_dat %>% 
    filter(date >= start_date) %>%
    add_macd(ema_short = ema_short, 
             ema_long = ema_long, 
             signal = macd_signal) %>%
    get_macd_signals() %>% 
    add_rsi(rsi_period = rsi_period) %>% 
    get_rsi_signals()
  
}

## Potential buying points ------------------------------------------------------------------

return_buying_points <- function(indicators_dat, criterion = "MACD"){
  "Return buying points based on a criterion (MACD, RSI, both)."
  
  if (criterion == "MACD"){
    buying_dat <- indicators_dat %>% 
      filter(MACDBuy == 1) %>% 
      select(date:MACDHist)
  }
  
  else{
    
    if (criterion == "RSI"){
      buying_dat <- indicators_dat %>% 
        filter(RSIBuy == 1) %>% 
        select(c(date:adjusted, RSI))
    }
    
    else{
      
      if (criterion == "MACD+RSI"){
        buying_dat <- indicators_dat %>%
          filter(MACDBuy == 1 & RSIBuy == 1) %>% 
          select(c(date:MACDHist, RSI))
      }
      
      else{
        stop("'criterion' must take on of the following values: 'MACD', 'RSI', 'MACD+RSI'.")
      }
      
    }
    
  }
  
  return(buying_dat)
  
}

return_last_signal_point <- function(signal_points_dat){
  "Return the last buying point for a given stock."
  signal_points_dat %>% 
    filter(date == today())
}


## Potential selling points -----------------------------------------------------------------

return_selling_points <- function(indicators_dat, criterion = "MACD"){
  "Return selling points based on a criterion (MACD, RSI, both)."
  
  if (criterion == "MACD"){
    buying_dat <- indicators_dat %>% 
      filter(MACDSell == 1) %>% 
      select(date:MACDHist)
  }
  
  else{
    
    if (criterion == "RSI"){
      buying_dat <- indicators_dat %>% 
        filter(RSISell == 1) %>% 
        select(c(date:adjusted, RSI))
    }
    
    else{
      
      if (criterion == "MACD+RSI"){
        buying_dat <- indicators_dat %>%
          filter(MACDSell == 1 & RSISell == 1) %>% 
          select(c(date:MACDHist, RSI))
      }
      
      else{
        stop("'criterion' must take on of the following values: 'MACD', 'RSI', 'MACD+RSI'.")
      }
      
    }
    
  }
  
  return(buying_dat)
  
}

add_returns <- function(
  selling_points_dat,
  buying_date, 
  num_shares
){
  "Add global returns for selling points."
  ticker <- selling_points_dat %>%
    pull(ticker) %>% 
    unique()
  buying_val <- yf_data[[ticker]] %>%
    filter(date == buying_date) %>% 
    pull(close)
  
  selling_points_dat %>% 
    filter(date >= buying_date) %>% 
    mutate( returns = calculate_total_returns(p0 = buying_val*num_shares, p1 = close*num_shares) )

}

## Stock recommendation process -------------------------------------------------------

stock_recommender <- function(
  stock_data, 
  start_date, 
  action = "Buy", 
  ema_short = 12, 
  ema_long = 26, 
  macd_signal = 9, 
  rsi_period = 10, 
  criterion = "MACD",
  num_shares = NULL,  
  buying_dates = NULL
){
  "Build the recommendation system."
  process <- function(indicators_dat, ticker){
    
    if (action == "Buy"){
      
      dat <- indicators_dat %>%
        return_buying_points(criterion = criterion) %>% 
        return_last_signal_point()
      
      return(dat)
    }
    
    else{
      if (action == "Sell"){
        
        if (nrow(indicators_dat) > 0){
          ticker <- indicators_dat %>% 
            head(1) %>% 
            pull(ticker)
        }
        
        if (ticker %in% my_tickers){
          dat <- indicators_dat %>%
            return_selling_points(criterion = criterion) 
          
          if (nrow(dat) > 0){
            dat <- dat %>% 
              add_returns(buying_date = buying_dates[ticker], 
                          num_shares = num_shares[ticker]) %>% 
              return_last_signal_point()
            return(dat)
          }
        }
        
      }
      
      else{
        stop("'action' must take one of the following values: 'Buy', 'Sell'.")
      }
      
    }
    
  }
  
  dat <- lapply(X = stock_data, 
                FUN = function(dat){
                  ticker <- dat %>% 
                    head(1) %>% 
                    pull(ticker)
                  dat %>%
                    add_indicators(start_date = start_date, 
                                   ema_short = ema_short, 
                                   ema_long = ema_long, 
                                   macd_signal = macd_signal, 
                                   rsi_period = rsi_period) %>%
                    process(ticker = ticker)
                }) %>% 
    bind_rows()
  
  if (ncol(dat) == 0){
    dat <- data.frame(date = as.Date(as.character()),
                      open = as.numeric(), 
                      high = as.numeric(), 
                      low = as.numeric(), 
                      close = as.numeric(),
                      volume = as.numeric(),
                      adjusted = as.numeric()) 
  }
  
  return(dat)
}

# UI -------------------------------------------------------

infoBox_dims <- function(
  box_height = "35px",
  icon_height = "45px", 
  icon_line_height = "35px"
){
  "Define dimensions for infoBox."
  
  dims <- paste0(".info-box {min-height: ",
                 box_height,
                 ";} .info-box-icon {height:",
                 icon_height,
                 "; line-height:", 
                 icon_line_height, 
                 ";} .info-box-content {padding-top: 0px; padding-bottom: 0px;}")
  return(dims)
} 


infoBox_last_price <- function(last_price){
  "Return infoBox for last price."
  
  infoBox(title = "Current price",
          value = last_price,
          color = "light-blue", 
          icon = tags$i(class = "fas fa-euro-sign", 
                        style = "font-size: 20px"), 
          fill = F)
}

infoBox_last_value <- function(last_val){
  "Return infoBox for last value"
  
  infoBox(title = "Current value",
          value = paste(last_val, "€"), 
          color = "purple", 
          icon = tags$i(class = "fas fa-money-check", 
                        style = "font-size: 20px"), 
          fill = F)
}

infoBox_avg_pred <- function(avg_pred, ticker){
  "Return infoBox for average predicted value."
  
  if (ticker == "All"){
    title <- "Average predicted value"
    value <- paste(avg_pred, "€")
    color <- "purple"
    icon <- tags$i(class = "fas fa-money-check", 
                   style = "font-size: 20px")
  }
  else {
    title <- "Average predicted price"
    value <- avg_pred
    color <- "light-blue"
    icon <- tags$i(class = "fas fa-dollar-sign", 
                   style = "font-size: 20px")
  }
  infoBox(title = title,
          value = value, 
          color = color, 
          icon = icon, 
          fill = F)
}

infoBox_num_shares <- function(num_shares){
  "Return infoBox for number of shares."
  
  infoBox(title = "Number of shares",
          value = num_shares, 
          color = "aqua", 
          icon = tags$i(class = "fas fa-wallet", 
                        style = "font-size: 20px"), 
          fill = F)
}


infoBox_last_cumret <- function(last_cr){
  "Return infoBox for current cumulative returns."
  
  if (last_cr < 0){
    ib <- infoBox(title = "Cumulative returns", 
                  value = paste("-", abs(last_cr)), 
                  icon = tags$i(class = "fas fa-percent", 
                                style="font-size: 20px"), 
                  color = "red", 
                  fill = F)
  }
  else{
    ib <- infoBox(title = "Cumulative returns", 
                  value = paste("+", last_cr), 
                  icon = tags$i(class = "fas fa-percent", 
                                style="font-size: 20px"), 
                  color = "green", 
                  fill = F)
  }
  return(ib)
}

infoBox_asset_cumret <- function(asset, type = "best"){
  "Return infoBox to display best asset's cumulative returns."
  
  val <- ifelse(asset$pct_cr > 0, 
                paste("+", asset$pct_cr), 
                paste("-", abs(asset$pct_cr)))
  
  if (type == "best"){
    icon <- tags$i(class = "fas fa-thumbs-up", 
                   style="font-size: 20px")
    color <- "green"
  }
  if (type == "worst"){
    icon <- tags$i(class = "fas fa-thumbs-down", 
                   style="font-size: 20px")
    color <- "red"
  }
  
  infoBox(title = asset$asset, 
          value = paste0(val, "%"),  
          icon = icon, 
          color = color, 
          fill = F)
  
}

asset_inputs <- function(
  asset, 
  val = 1, 
  buying_date = today() - months(6)
){
  "Build shiny inputs for number of shares and buying date."
  
  numInputId <- paste("num_shares", get_ticker(asset), sep = "_")
  dateInputId <- paste("buying_date", get_ticker(asset), sep = "_")
  
  fluidRow(
    style = "height:80px;", 
    column(width = 6, 
           numericInput(numInputId,
                        label = h5("Number of shares"), 
                        min = 0, 
                        value = val)), 
    column(width = 6,
           airDatepickerInput(
             inputId = dateInputId,
             value = buying_date,
             minDate = date_init, 
             maxDate = today(),
             label = h5("Buying date"),
             placeholder = "",
             multiple = F, 
             clearButton = F))
  )
}

picker_inputs_font_weight <- function(){
  font_weights <- rep(x = "font-weight: plain;", nrow(symbols))
  font_weights[my_tickers_ix] <- "font-weight: bold;"
  return(font_weights)
}
