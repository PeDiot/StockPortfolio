# --------- French Stock - Data Exploration --------

# ----- Setup -----

source("Rpackages.R")

theme_set(theme_minimal())

source("setup.R", 
       encoding = "UTF-8")

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
ticker <- get_ticker(asset)
prices <- get_tq_data(ticker)

assets_data <- get_tq_data(tickers = tickers, 
                           start_date = "2021-09-01")

# ----- Value per asset ------

assets_value <- compute_assets_value(data = assets_data, 
                                     num_shares = num_shares)

portfolio_data <- get_portfolio_value(assets_value) 

ret_data <- assets_value %>%
  compute_daily_returns() %>%
  compute_weighted_returns(num_shares = num_shares) 

port_ret <- ret_data %>%
  compute_cumulative_returns()

assets_cumret <- ret_data %>%
  compute_cumulative_returns(all= F)

data <- merge(x = portfolio_data, 
              y = port_ret, 
              by = "date") 

ay <- list(
  tickfont = list(color = macd),
  titlefont = list(color = macd),
  overlaying = "y",
  side = "right",
  title = "Cumulative returns"
)

plot_ly(data) %>%
  plot_price_evolution(title = "") %>%
  plot_cumulative_returns(title = "", 
                          yaxis = "y2") %>%
  layout(title = "Portfolio value and cumulative returns",
         xaxis = list(rangeslider = list(visible = F), 
                      rangeselector = range_selector_period(y_pos = -0.15), 
                      title = ""),
         yaxis = list(tickfont = list(color = evolution), 
                      title = ""), 
         yaxis2 = ay)

ticker <- get_ticker("OVH Groupe")

ret_data %>%
  compute_cumulative_returns(ticker = ticker) %>%
  View()

portfolio_cumulative_returns(port_ret)

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

prices %>%
  candlestick_chart(ticker = ticker) 

prices %>%
  macd_chart(ticker = ticker)

prices %>%
  rsi_chart(ticker = ticker)
