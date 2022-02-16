import yfinance as yf


def data_base():
    data_lvmh = yf.download("MC.PA", start="2021-01-01", progress=False)
    d_renault = yf.download("RNO.PA", start="2021-01-01", progress=False)
    d_air = yf.download("AI.PA", start="2021-01-01", progress=False)
    d_airbus = yf.download("AIR.PA", start="2021-01-01", progress=False)
    d_loreal = yf.download("OR.PA", start="2021-01-01", progress=False)
    d_ubi = yf.download("UBI.PA", start="2021-01-01", progress=False)
    d_orange = yf.download("ORA.PA", start="2021-01-01", progress=False)
    d_car = yf.download("CA.PA", start="2021-01-01", progress=False)
    d_tf1 = yf.download("TFI.PA", start="2021-01-01", progress=False)
    d_edf = yf.download("EDF.PA", start="2021-01-01", progress=False)
    d_ovh = yf.download("OVH.PA", start="2021-01-01", progress=False)

    return data_lvmh, d_renault, d_air, d_airbus, d_loreal, d_ubi, d_orange, d_car, d_tf1, d_edf, d_ovh
