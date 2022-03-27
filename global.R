# Setup -------------------------------------------------------------------

# setwd("C:/Users/pemma/OneDrive/Bureau/StockPortfolio")

source("Rpackages.R")
source("setup.R", 
       encoding = "UTF-8")

# Data --------------------------------------------------------------------

date_init <- today() - years(5)
yf_data <- get_tq_data(tickers = symbols$tickers, 
                       start_date = date_init) 