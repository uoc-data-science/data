import pandas as pd
import os
import os.path

def clean_file(file_path: str) -> pd.DataFrame:
    """ Cleans a given CSV file from NaN values etc. and returns the data frame. """
    print('Loading file ' + file_path)
    if os.path.isfile(file_path):
        df = pd.read_csv(file_path, encoding = "latin-1")
        print('1. Removing empty rows and columns')
        df_without_na_rows = df.dropna(how = "all")
        df_without_na_rows_and_cols = df_without_na_rows.dropna(axis = 1, how = "all")
        return df_without_na_rows_and_cols
    else:
        print('The data file does not exist!')


def clean_order_data() -> pd.DataFrame:
    """ Reads and cleans the order data. """
    return clean_file(r'data/interim/orders/orders_with_headers_py.csv')

def clean_clickstream_data() -> pd.DataFrame:
    """ Reads and cleans the clickstream data. """
    return clean_file(r'data/interim/clickstream/clickstream_with_headers_py.csv')

cleaned_clickstream_data_df = clean_clickstream_data()
cleaned_clickstream_data_df.to_csv(r'data/interim/clickstream/clickstream_cleaned_py.csv', encoding = 'latin-1')

cleaned_order_data_df = clean_order_data()
cleaned_order_data_df.to_csv(r'data/interim/orders/orders_cleaned_py.csv', encoding = 'latin-1')
