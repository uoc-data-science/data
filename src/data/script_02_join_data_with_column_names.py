import pandas as pd
import os
import os.path

def read_data_with_columns(data_file: str, column_names_file: str) -> pd.DataFrame:
    """ Joins a CSV data file with its corresponding column names and returns the pandas.DataFrame """
    if os.path.isfile(data_file):
        if os.path.isfile(column_names_file):
            with open(column_names_file) as cf:
                column_names = list(map(lambda column : column.split(':')[0].replace(' ', '_'), cf))
                #column_names.replace(' ', '_')
                return pd.read_csv(data_file, names = column_names, encoding = 'latin-1')
        else:
            print('The columns file does not exist!')
    else:
        print('The data file does not exist!')


def read_order_data() -> pd.DataFrame:
    """ Reads in the order data and returns it in a pandas.DataFrame """
    return read_data_with_columns(r'data/raw/orders/order_data.csv', r'data/raw/orders/order_columns.txt')

def read_clickstream_data() -> pd.DataFrame:
    """ Reads in the clickstream data and returns it in a pandas.DataFrame """
    df1 = read_data_with_columns(r'data/interim/clickstream/clickstream_data.csv', r'data/raw/clickstream/clickstream_columns.txt')
    df2 = read_data_with_columns(r'data/raw/clickstream/clickstream_data_part_2.csv', r'data/raw/clickstream/clickstream_columns.txt')
    combined_clickstream_df = pd.concat([df1, df2], axis = 1, sort = False)
    return combined_clickstream_df


print('Reading order data...')
order_data_df = read_order_data()
print('order_data_df.describe()...')
print(order_data_df.describe(include = 'all'))
print('order_data_df.to_csv()...')
order_data_df.to_csv(r'data/interim/orders/orders_with_headers_py.csv', encoding = 'latin-1')

print('Reading clickstream data...')
clickstream_data_df = read_clickstream_data()
print('clickstream_data_df.describe()...')
print(clickstream_data_df.describe(include = 'all'))
print('clickstream_data_df.to_csv()...')
clickstream_data_df.to_csv(r'data/interim/clickstream/clickstream_with_headers_py.csv', encoding = 'latin-1')
