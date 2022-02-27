library(tidyquant)

#lvmh
getSymbols("MC.PA", from = '2021-02-01')
temp = data.frame(MC.PA)
write.csv(temp, "lvmh.csv")

#renault
getSymbols("RNO.PA", from = '2021-02-01')
temp = data.frame(RNO.PA)
write.csv(temp, "renault.csv")

#air france
getSymbols("AI.PA", from = '2021-02-01')
temp = data.frame(AI.PA)
write.csv(temp, "air_france.csv")

#airbus
getSymbols("AIR.PA", from = '2021-02-01')
temp = data.frame(AIR.PA)
write.csv(temp, "airbus.csv")

#loreal
getSymbols("OR.PA", from = '2021-02-01')
temp = data.frame(OR.PA)
write.csv(temp, "loreal.csv")

# ubisoft
getSymbols("UBI.PA", from = '2021-02-01')
temp = data.frame(UBI.PA)
write.csv(temp, "ubi.csv")

#orange
getSymbols("ORA.PA", from = '2021-02-01')
temp = data.frame(ORA.PA)
write.csv(temp, "orange.csv")

#carrefour
getSymbols("CA.PA", from = '2021-02-01')
temp = data.frame(CA.PA)
write.csv(temp, "car.csv")

#tf1
getSymbols("TFI.PA", from = '2021-02-01')
temp = data.frame(TFI.PA)
write.csv(temp, "tf1.csv")

#edf
getSymbols("EDF.PA", from = '2021-02-01')
temp = data.frame(EDF.PA)
write.csv(temp, "edf.csv")

#ovh
getSymbols("OVH.PA", from = '2021-02-01')
temp = data.frame(OVH.PA)
write.csv(temp, "ovh.csv")