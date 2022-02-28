library(reticulate)

setwd("./ML/")
backup <- "../backup/"

# NOT RUN {

  # virtualenv_create(envname = "virtual_env",
                    # python= "C:\\Users\\pemma\\AppData\\Local\\Programs\\Python\\Python39\\python.exe")

  # virtualenv_install("virtual_env",
                     # packages = c("numpy", "matplotlib", Pillow", "yfinance", "tensorflow"))
# }

use_virtualenv("virtual_env", 
               required = T)

df <- data.frame(c(1:11))

write.csv(df, paste0(backup, 
                    "df_temp.csv")) 

source_python(file = "final_pred_data.py")

#pred_for_each_data()
