

reticulate::virtualenv_create(envname = 'virtual_env',
                              python= 'C:\\Users\\marin\\anaconda3\\python.exe')

reticulate::virtualenv_install('virtual_env',
                               packages = c('numpy',"tensorflow","matplotlib","Pillow","yfinance"))

reticulate::use_virtualenv('virtual_env', required = T)

###############
# sélection des actions
df = data.frame(c(1:11))

write.csv(df,"df_temp.csv")
###############

library(reticulate)

source_python("final_pred_data.py")

#pred_for_each_data()
