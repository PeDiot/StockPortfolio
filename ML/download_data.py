import yfinance as yf
import pandas as pd

tmp = "./tmp/"

def data_base():

    data_lvmh = pd.read_csv(tmp + 'lvmh.csv')
    d_renault = pd.read_csv(tmp + 'renault.csv')
    d_air_liquide = pd.read_csv(tmp + 'air_liquide.csv')
    d_loreal = pd.read_csv(tmp + 'loreal.csv')
    d_ubi = pd.read_csv(tmp + 'ubi.csv')
    d_orange = pd.read_csv(tmp + 'orange.csv')
    d_car = pd.read_csv(tmp + 'car.csv')
    d_tf1 = pd.read_csv(tmp + 'tf1.csv')
    d_edf = pd.read_csv(tmp + 'edf.csv')
    d_ovh = pd.read_csv(tmp + 'ovh.csv')

    return data_lvmh, d_renault, d_air_liquide, d_loreal, d_ubi, d_orange, d_car, d_tf1, d_edf, d_ovh

# print(data_base()[7])


