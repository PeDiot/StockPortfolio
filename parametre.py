def params(df):
    
    data = df[['Close']]
    time = [i for i in range(len(data))]
    split_time = round(len(data)*0.8)
    data_ = [i for i in data.Close]

    window_size = 10# on prend 10 valeurs pour prédire la 11eme
    batch_size = 8
    
    return data_, time, split_time, window_size, batch_size

