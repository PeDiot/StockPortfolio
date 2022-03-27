# Setup -------------------------------------------------------------------

source("Rpackages.R")
source("setup.R", 
       encoding = "UTF-8")

# Data --------------------------------------------------------------------

date_init <- today() - years(5)
yf_data <- get_tq_data(tickers = symbols %>%
                         pull(tickers), 
                       start_date = date_init) 

# save_data_list(df_list = yf_data)

# Predict new values for each stock --------------------------------------------------------------------

## Launch Python script --------------------------------------------------------------------

# reticulate::py_run_file("stock_prediction.py")

## Load predictions --------------------------------------------------------------------

load(file = "./backup/stock_predictions.RData") 