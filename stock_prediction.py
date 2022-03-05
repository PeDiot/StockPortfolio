"""Description.

Using stockPrediction module: 
    - get assets to predict, 
    - predict using Keras model, 
    - save predictions to Rdata.
"""

from stockPrediction import (
    get_tickers, 
    load_assets_data, 
    save_to_rdata,
)
from stockPrediction import predict_assets

BACKUP = "./backup/"

tickers = get_tickers(file_path=BACKUP+"symbols.RData")
assets = load_assets_data(dir=BACKUP+"assets/") 
predictions = predict_assets(
    assets_data=assets, 
    tickers=tickers, 
    model_dir=BACKUP+"model.h5"
) 
save_to_rdata(
    pydat=predictions,
    file_path=BACKUP+"stock_predictions.RData"
)