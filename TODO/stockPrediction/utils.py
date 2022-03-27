"""Description.

Useful functions and directories."""

import pandas as pd
from datetime import date, timedelta
from pyreadr import read_r, write_rdata
from os import listdir
from typing import List, Dict

def get_tickers(file_path: str) -> List:
    """Return a list of tickers to handle."""
    d = read_r(file_path)
    tickers = d["symbols"].values.tolist() 
    return [ticker[0] for ticker in tickers] 

def init_prediction_dict(tickers: List) -> Dict: 
    """Initialize a dictionnary to fill with predicted data."""
    d = dict()
    d["date"] = [date.today() + timedelta(d) for d in range(1, 6)]
    return d 

def load_assets_data(dir: str) -> List: 
    """return a list of price dataframes."""
    files = listdir(dir) 
    return [
        pd.read_feather(dir+file) 
        for file in files
    ]

def save_to_rdata(
    pydat, 
    file_path: str, 
    df_name: str = "predictions"
): 
    """Save python data to Rdata."""
    write_rdata(
        path=file_path, 
        df=pydat, 
        df_name=df_name
    )
