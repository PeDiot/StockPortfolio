library(tidyquant)

backup <- "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/02 - Big Data/Project/StockPortfolio/backup/"

#lvmh
getSymbols("MC.PA", from = '2021-02-01')
temp = data.frame(MC.PA)
write.csv(temp, paste0(backup, "lvmh.csv"))

#renault
getSymbols("RNO.PA", from = '2021-02-01')
temp = data.frame(RNO.PA)
write.csv(temp, paste0(backup, "renault.csv"))

#air liquide
getSymbols("AI.PA", from = '2021-02-01')
temp = data.frame(AI.PA)
write.csv(temp, paste0(backup, "air_liquide.csv"))

#loreal
getSymbols("OR.PA", from = '2021-02-01')
temp = data.frame(OR.PA)
write.csv(temp, paste0(backup, "loreal.csv"))

# ubisoft
getSymbols("UBI.PA", from = '2021-02-01')
temp = data.frame(UBI.PA)
write.csv(temp, paste0(backup, "ubi.csv"))

#orange
getSymbols("ORA.PA", from = '2021-02-01')
temp = data.frame(ORA.PA)
write.csv(temp, paste0(backup, "orange.csv"))

#carrefour
getSymbols("CA.PA", from = '2021-02-01')
temp = data.frame(CA.PA)
write.csv(temp, paste0(backup, "car.csv"))

#tf1
getSymbols("TFI.PA", from = '2021-02-01')
temp = data.frame(TFI.PA)
write.csv(temp, paste0(backup, "tf1.csv"))

#edf
getSymbols("EDF.PA", from = '2021-02-01')
temp = data.frame(EDF.PA)
write.csv(temp, paste0(backup, "edf.csv"))

#ovh
getSymbols("OVH.PA", from = '2021-02-01')
temp = data.frame(OVH.PA)
write.csv(temp, paste0(backup, "ovh.csv"))