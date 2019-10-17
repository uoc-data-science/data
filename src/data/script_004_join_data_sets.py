import pandas as pd
import os
import os.path

def merge_files(clickstream_file_path: str, orders_file_path: str) -> pd.DataFrame:
    """ Cleans a given CSV file from NaN values etc. and returns the data frame. """
    print('Loading file ' + clickstream_file_path)
    if os.path.isfile(clickstream_file_path):
        clickstream_df = pd.read_csv(clickstream_file_path, encoding = 'latin-1')
        print('Loading file ' + orders_file_path)
        if os.path.isfile(orders_file_path):
            orders_df = pd.read_csv(orders_file_path, encoding = 'latin-1')

            # force certain variables to be categorical:
            clickstream_df['Session_ID'] = clickstream_df['Session_ID'].astype('category')
            orders_df['Order_Session_ID'] = orders_df['Order_Session_ID'].astype('category')

            # join data:
            return pd.merge(clickstream_df, orders_df, how = 'inner', left_on='Session_ID', right_on='Order_Session_ID')
        else:
            print('The data file does not exist!')
    else:
        print('The data file does not exist!')

print('Merging data files...')
joined_df = merge_files(r'data/interim/clickstream/clickstream_cleaned_py.csv', r'data/interim/orders/orders_cleaned_py.csv')
print('joined_df.describe()...')
print(joined_df.describe(include = 'all'))
