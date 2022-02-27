import yfinance as yf
import pandas as pd

def data_base():

    data_lvmh = pd.read_csv('lvmh.csv')
    d_renault = pd.read_csv('renault.csv')
    d_air = pd.read_csv('air_france.csv')
    d_airbus = pd.read_csv('airbus.csv')
    d_loreal = pd.read_csv('loreal.csv')
    d_ubi = pd.read_csv('ubi.csv')
    d_orange = pd.read_csv('orange.csv')
    d_car = pd.read_csv('car.csv')
    d_tf1 = pd.read_csv('tf1.csv')
    d_edf = pd.read_csv('edf.csv')
    d_ovh = pd.read_csv('ovh.csv')

    return data_lvmh, d_renault, d_air, d_airbus, d_loreal, d_ubi, d_orange, d_car, d_tf1, d_edf, d_ovh

# print(data_base()[7])


