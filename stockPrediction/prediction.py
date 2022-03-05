from .train_test import transform_train_test
from .parameters import params
from .utils import init_prediction_dict 

import pandas as pd
from pandas.core.frame import DataFrame
from typing import (
    Tuple, 
    Dict, 
    List
)

import tensorflow as tf
from keras.engine.sequential import Sequential

def _predict_keras(
    model: Sequential,
    data: DataFrame
) -> Tuple:
    "5-day stock prediction for on asset using keras neural network."
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

def _load_model(dir: str) -> Sequential: 
    """Load keras model."""
    return tf.keras.models.load_model(dir)

def predict_assets(
    assets_data: List[DataFrame], 
    tickers: List, 
    model_dir: str
) -> Tuple: 
    """Output 5-day price prediction and last observed value for a list of assets."""
    predictions = init_prediction_dict(tickers)
    model = _load_model(dir=model_dir)
    for dat in assets_data: 
        ticker = dat.loc[0, "ticker"]
        preds, last_val = _predict_keras(model=model, data=dat) 
        predictions[ticker] = [last_val] + preds
    return pd.DataFrame(predictions)