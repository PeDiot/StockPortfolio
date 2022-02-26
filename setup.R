options(warn = -1)

# ----- French companies data -----
backup <- "./backup/"
french_stocks <- read_csv(file = paste0(backup, "FrenchStocks.csv"), 
                 show_col_types = FALSE)

# ----- Colors -----

evolution <- "#709ABE"
high <- "Forest Green"
low <- "Red"
short <- "#72C4D7"
medium <- "purple"
long <- "black"
macd <- "#C47D72"
macdHist <- "#B5CDD2"
rsi <- "#F3B0DE"

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


get_indicator_plot_title <- function(ticker, indicator_type){
  "Make title ."
  asset <- get_asset(ticker)
  title <- paste0(asset, 
                 " (", 
                 ticker,
                 ") - ", 
                 indicator_type)
  return(title)
}

get_tq_data <- function(tickers, start_date){
  "Return price data for given asset and period."
  
  if (length(tickers) == 1){
    tq_get(x = tickers, 
           get = "stock.prices", 
           from = start_date,
           to = today(),
           complete_cases = T)
  }
  else {
    dat <- tq_get(x = tickers, 
                  get = "stock.prices", 
                  from = start_date,
                  to = today(),
                  complete_cases = T)
    colnames(dat)[1] <- "ticker"  
    df_list <- split(dat, dat$ticker)
    return(df_list)
  }
  
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
      asset <- get_asset(ticker)
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
      round(2)
  }
  else{
    pct_cr <- - 100*(1 - cr) %>%
      round(2)
  }
  return(pct_cr)
  
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
      n <- nrow(df)
      n_shares <- num_shares[ticker]
      df %>%
        mutate(n_shares = rep(n_shares, n)) %>%
        mutate(value = close * n_shares) 
    }
  ) %>%
    bind_rows()
  
}

get_portfolio_value <- function(assets_value){
  "Return portfolio value given assets' prices and number of shares."
  
  assets_value %>%
    group_by(ticker) %>%
    complete(date = seq.Date(min(date), max(date), by="day")) %>%
    fill(value) %>%
    group_by(date) %>%
    summarise(value = sum(value))
  
}

get_portfolio_current_value <- function(portfolio_value){
  "Return portfolio value at last date." 
  
  portfolio_value %>%
    filter(date == max(date)) %>%
    pull(value) %>%
    round(2) %>%
    format(big.mark = ",", 
           decimal.mark = ".", 
           scientific = F)
}


# ----- Assets performance -----

compute_daily_returns <- function(assets_value){
  "Calculate the daily returns and for our assets."
  
  assets_value %>%
    group_by(ticker) %>%
    complete(date = seq.Date(min(date), max(date), by="day")) %>%
    fill(value) %>%
    group_by(ticker) %>%
    tq_transmute(select = close,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "ret") 
  
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
      group_by(date) %>%
      summarise(port_ret = sum(wt_return)) %>%
      mutate(cr = cumprod(1 + port_ret)) %>%
      mutate(move = case_when(cr > 1 ~ "Up", 
                              TRUE ~ "Down") %>%
               as.factor())
  }
  else{
    cum_returns <- ret_data %>%
      group_by(ticker) %>%
      mutate(cr = cumprod(1 + wt_return)) %>%
      mutate(move = case_when(cr > 1 ~ "Up", 
                              TRUE ~ "Down") %>%
               as.factor())
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
  l <- list(asset = get_asset(d$ticker), 
            pct_cr = cumret_to_percent(d$cr))
  return(l) 
  
}

get_worst_asset <- function(assets_cumret){
  "Return asset with worst cumulative returns as of today."
  
  d <- assets_cumret %>% 
    filter(date == max(date)) %>% 
    ungroup() %>% 
    filter(cr == min(cr))
  l <- list(asset = get_asset(d$ticker), 
            pct_cr = cumret_to_percent(d$cr))
  return(l) 
  
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
  
  price_data %>%
    mutate(MACD = macd_[, 1], 
           MACDSignal = macd_[, 2], 
           MACDHist = MACD - MACDSignal)
  
}

get_macd_signals <- function(price_data){
  "Identify buying and selling signals from MACD."
  
  col_names <- colnames(price_data)
  if ("MACD" %in% col_names & "MACDSignal" %in% col_names){
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

# --- Plotly settings

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

plotly_legend <- function(x.pos = .5, y.pos = -.2, size = 12){
  list(orientation = "h", x = x.pos, y = y.pos,
       xanchor = "center", yref = "paper",
       font = list(size = 12),
       bgcolor = "transparent", 
       borderwidth = .2)
}

plotly_layout <- function(p, title, title.y){
  
  p %>%
    layout(title = title,
           xaxis = list(rangeslider = list(visible = F), 
                        rangeselector = range_selector_period(y_pos = -0.15), 
                        title = ""),
           yaxis = list(fixedrange = FALSE, 
                        title = title.y),
           legend = plotly_legend()) 
}


# --- Performance

plot_cumulative_returns <- function(
  plotly_obj, 
  title, 
  legend_group,
  yaxis = NULL
){
  "Plot cumulative returns evolution."
  
  if (is.null(yaxis)){
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~cr,
                name = "Cumulative returns", 
                yaxis = yaxis, 
                line = list(width = 1.7, 
                            color = macd), 
                legendgroup = legend_group) %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = c(~min(date), ~max(date)),
                y = c(1, 1), 
                yaxis = yaxis, 
                line = list(color = "black",
                            width = 0.5,
                            dash = "dot"), 
                showlegend = F)
  }
  else{
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~cr,
                name = "Cumulative returns", 
                yaxis = yaxis, 
                line = list(width = 1.7, 
                            color = macd), 
                legendgroup = legend_group) %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = c(~min(date), ~max(date)),
                y = c(1, 1), 
                yaxis = yaxis, 
                line = list(color = "black",
                            width = 0.5,
                            dash = "dot"), 
                showlegend = F)
  }
  
  return(p)
  
}

plot_price_evolution <- function(
  plotly_obj, 
  title, 
  legend_group, 
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
                name = "Value ($)", 
                line = list(color = evolution,
                            width = 1.7), 
                legendgroup = legend_group)
  }
  else{
    p <- plotly_obj %>%
      add_trace(type = "scatter", 
                mode = "lines",
                marker = NULL,
                x = ~date,
                y = ~value,
                name = "Value ($)",
                yaxis = yaxis, 
                line = list(color = evolution,
                            width = 1.7), 
                legendgroup = legend_group)
  }
  
  return(p)
  
  
}

# --- Portfolio

portfolio_evolution <- function(portfolio_value, portfolio_returns){
  
  data <- merge(x = portfolio_value, 
                y = portfolio_returns, 
                by = "date") 
  
  plot_ly(data) %>%
    plot_price_evolution(title = "", 
                         legend_group = "one") %>%
    plot_cumulative_returns(title = "", 
                            yaxis = "y2", 
                            legend_group = "two") %>%
    layout(title = "",
           xaxis = list(rangeslider = list(visible = F), 
                        rangeselector = range_selector_period(y_pos = -0.15), 
                        title = ""),
           yaxis = list(domain = c(0.55, 1),
                        fixedrange = FALSE,
                        tickfont = list(color = evolution), 
                        title = "$"), 
           yaxis2 = list(domain = c(0, 0.45),
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
    mutate(asset = lapply(ticker, get_asset))
  tot_val <- sum(d$value)
  d <- d %>%
    mutate(pct = 100 * value / tot_val)
  
  colors <- colorRampPalette(brewer.pal(9, "Blues"))(100)[d$pct]
  
  fig <- d %>%
    plot_ly(labels = ~ticker, 
            values = ~value, 
            type = "pie", 
            textposition = "inside",
            textinfo = "label+percent",
            insidetextfont = list(color = "black"),
            hoverinfo = "text",
            text = ~paste0(asset, " ($", value %>%
                            round(2) %>%
                            format(big.mark = ",", 
                                   decimal.mark = ".", 
                                   scientific = F), 
                           ")"),
            marker = list(colors = colors,
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
                          width = 1),
              hoverinfo = "none") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MA50,
              name = "MA50",
              line = list(color = medium,
                          width = 1), 
              hoverinfo = "none") %>%
    add_trace(type = "scatter", 
              mode = "lines",
              marker = NULL,
              x = ~date,
              y = ~MA100,
              name = "MA100",
              line = list(color = long, 
                          dash = "dot", 
                          width = 1), 
              hoverinfo = "none")
  
  title <- get_indicator_plot_title(ticker, 
                                    indicator_type = "Candlestick & Moving Averages")
  p <- p %>%
    plotly_layout(title = title, title.y = "$")
  
  return(p)
    
  
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
                      rangeselector = range_selector_period(y_pos = -0.15), 
                      title = ""),
         yaxis = list(domain = c(0.55, 1),
                      fixedrange = FALSE,
                      tickfont = list(color = evolution), 
                      title = "$"), 
         yaxis2 = list(domain = c(0, 0.45),
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
                          width = 1.5), 
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
                        rangeselector = range_selector_period(y_pos = -0.15), 
                        title = ""),
           yaxis = list(domain = c(0.55, 1),
                        fixedrange = FALSE,
                        tickfont = list(color = evolution), 
                        title = "$"), 
           yaxis2 = list(domain = c(0, 0.45),
                         fixedrange = FALSE, 
                         tickfont = list(color = rsi), 
                         title = ""), 
           legend = plotly_legend())
  
  return(p)
  
}

# ----- UI -----

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

infoBox_port_cumret <- function(last_cr){
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
    icon <- tags$i(class = "fas fa-arrow-up", 
                  style="font-size: 20px")
    color <- "green"
  }
  if (type == "worst"){
    icon <- tags$i(class = "fas fa-arrow-down", 
                  style="font-size: 20px")
    color <- "red"
  }
  
  infoBox(title = asset$asset, 
          value = paste0(val, "%"),  
          icon = icon, 
          color = color, 
          fill = F)
  
}

num_shares_input <- function(
  asset1, 
  asset2, 
  val1 = 1, 
  val2 = 1
){
  "Build shiny numeric input for number of shares."
  
  inputId1 <- paste("num_shares", get_ticker(asset1), sep = "_")
  inputId2 <- paste("num_shares", get_ticker(asset2), sep = "_")
  if (asset2 == "OVH Groupe"){
    asset2 <- "OVH"
  }
  fluidRow(
    column(width = 6, 
           numericInput(inputId1,
                        label = h5(asset1), 
                        min = 0, 
                        value = val1)), 
    column(width = 6,
           numericInput(inputId2, 
                        label = h5(asset2), 
                        min = 0, 
                        value = val2))
  )
}
  