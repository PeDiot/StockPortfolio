import tensorflow as tf
from parametre import params

def transform_train_test(data_autre):
    
    data_d = params(data_autre)[0]
    split_time = params(data_autre)[2]
    time = params(data_autre)[1]
    window_size = params(data_autre)[3]
    batch_size = params(data_autre)[4]

    test = data_d[split_time:]
    time_test = time[split_time:]

    train = data_d[:split_time]
    time_train = time[:split_time]

    train_df = tf.data.Dataset.from_tensor_slices(train)
    train_df = train_df.window(window_size + 1,shift=1,drop_remainder=True)
    train_df = train_df.flat_map(lambda window : window.batch(window_size + 1))
    train_df = train_df.map(lambda x : (x[:-1],x[-1]))
    train_df = train_df.batch(batch_size,drop_remainder=True).prefetch(1)

    test_df = tf.data.Dataset.from_tensor_slices(test)
    test_df = test_df.window(window_size,shift=1,drop_remainder=True)
    test_df = test_df.flat_map(lambda window : window.batch(window_size))
    test_df = test_df.batch(1)

    return test_df, train_df, time_test, time_train, test
