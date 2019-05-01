import pandas as pandas
from typing import List
import os

def read_order_data() -> pandas.DataFrame:
    """ Reads in the order data and returns it in a pandas.DataFrame """
    
    # get current directory and compute path to file
    dirname = os.path.dirname(__file__)
    orderColumnsFileName = os.path.join(dirname, r'../../data/raw/orders/order_columns.txt')

    rawColumnNames: List[str] = [];
    # read in line by line into list
    with open(orderColumnsFileName, "r") as f:
        rawColumnNames = list(f)

    # split each column at the ':' and take the first part so we have the pure column name
    columnNames = list(map(lambda column : column.split(':')[0], rawColumnNames))

    # transform csv into data frame with the column names
    orderDataFileName = os.path.join(dirname, r'../../data/raw/orders/order_data.csv')
    df = pandas.read_csv(orderDataFileName, names = columnNames)
    
    return df