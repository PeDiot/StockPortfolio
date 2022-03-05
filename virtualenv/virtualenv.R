library(reticulate)

setwd("./ML/")

# NOT RUN {

  virtualenv_create(envname = "./stockPrediction_virtualenv",
                    python= "C:\\Users\\pemma\\AppData\\Local\\Programs\\Python\\Python39\\python.exe")

  virtualenv_install(envname = "./stockPrediction_virtualenv",
                     packages = c("numpy", 
                                  "pandas",
                                  "matplotlib", 
                                  "datetime", 
                                  "pyreadr", 
                                  "typing", 
                                  "tensorflow"))

# }