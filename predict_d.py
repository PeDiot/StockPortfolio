import wget
from train_test import transform_train_test
from parametre import params
import tensorflow as tf
import os


def prediction(data_autre):
    try:
        model = tf.keras.models.load_model(path)
    except:
        url = wget.download("https://dl.dropboxusercontent.com/s/wfcw56dmg1w6169/model.h5?dl=0")
        path = os.getcwd() + "\\" + url
        model = tf.keras.models.load_model(path)
    
    test_df = transform_train_test(data_autre)[0]
    window_size = params(data_autre)[3]
    der_val = list()
    test = transform_train_test(data_autre)[4]
    
    next_clo = test[-window_size:]
    df_pred_next = tf.data.Dataset.from_tensor_slices(next_clo)
    df_pred_next = df_pred_next.window(window_size,shift=1,drop_remainder=True)
    df_pred_next = df_pred_next.flat_map(lambda window : window.batch(10))
    df_pred_next = df_pred_next.batch(1)

    der_val.append(test[-1])
    for i in range(5):#prediction 5 jours
        test_df = transform_train_test(data_autre)[0]

        next_clo = test[-10:]
        df_pred_next = tf.data.Dataset.from_tensor_slices(next_clo)
        df_pred_next = df_pred_next.window(10,shift=1,drop_remainder=True)
        df_pred_next = df_pred_next.flat_map(lambda window : window.batch(10))
        df_pred_next = df_pred_next.batch(1)

        model.predict(df_pred_next)[0][0]
        test.append(model.predict(df_pred_next)[0][0])
    

    return test[-5:], der_val
