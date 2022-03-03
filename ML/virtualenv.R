library(reticulate)

setwd("./ML/")

# NOT RUN {

  virtualenv_create(envname = "./stockPrediction_virtualenv",
                    python= "C:\\Users\\pemma\\AppData\\Local\\Programs\\Python\\Python39\\python.exe")

  virtualenv_install("stockPrediction_virtualenv",
                     packages = c("numpy", 
                                  "matplotlib", 
                                  "datetime", 
                                  "pyreadr", 
                                  "typing", 
                                  "tensorflow"))

# }