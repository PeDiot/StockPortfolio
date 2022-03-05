def params(data):
    
    data.index = data["date"]
    data = data.loc[:, "close"]
    time = [i for i in range(len(data))]
    split_time = round(len(data)*0.8)
    data_ = [i for i in data]

    window_size = 10            # 10 values are taken to predict the 11th
    batch_size = 8
    return data_, time, split_time, window_size, batch_size, data

