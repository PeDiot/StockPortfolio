from train_test import transform_train_test
from parameters import params
from utils import (
    tmp, 
    get_tickers, 
    init_prediction_dict,
    load_assets_data,
    save_to_rdata, 
) 

import pandas as pd
from pandas.core.frame import DataFrame
from typing import (
    Tuple, 
    Dict, 
    List
)

import tensorflow as tf

TICKERS = get_tickers()

def _predict_keras(data: DataFrame) -> Tuple:
    "5-day stock prediction for on asset using keras neural network."
    
    model = tf.keras.models.load_model("model.h5")
    
    test_df = transform_train_test(data)[0]
    window_size = params(data)[3]
    test = transform_train_test(data)[4]
    
    next_clo = test[-window_size:]
    df_pred_next = tf.data.Dataset.from_tensor_slices(next_clo)
    df_pred_next = df_pred_next.window(window_size,shift=1,drop_remainder=True)
    df_pred_next = df_pred_next.flat_map(lambda window : window.batch(10))
    df_pred_next = df_pred_next.batch(1)

    last_val = test[-1]

    for i in range(5):
        test_df = transform_train_test(data)[0]

        next_clo = test[-10:]
        df_pred_next = tf.data.Dataset.from_tensor_slices(next_clo)
        df_pred_next = df_pred_next.window(10,shift=1,drop_remainder=True)
        df_pred_next = df_pred_next.flat_map(lambda window : window.batch(10))
        df_pred_next = df_pred_next.batch(1)

        model.predict(df_pred_next)[0][0]
        test.append(model.predict(df_pred_next)[0][0])
        
    return test[-5:], last_val

def predict_assets(assets_data: List[DataFrame]) -> Tuple: 
    """Output 5-day price prediction and last observed value for a list of assets."""
    predictions = init_prediction_dict(tickers=TICKERS)
    for dat in assets_data: 
        ticker = dat.loc[0, "ticker"]
        preds, last_val = _predict_keras(dat) 
        predictions[ticker] = [last_val] + preds
    return pd.DataFrame(predictions)

assets = load_assets_data() 
predictions = predict_assets(assets) 
save_to_rdata(
    pydat=predictions,
    file_path=tmp+"stock_predictions.RData"
)


