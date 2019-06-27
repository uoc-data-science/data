import pandas as pd
import numpy as np

pathClicks = '../../0 Data/clickstream_data_cleaned_P.csv'
pathCustomers = "../../3 Data Modelling/customers.csv"
pathMerge = "../../0 Data/merged_P.csv"

#read data
clicks = pd.read_csv(pathClicks, sep=",", dtype=str)
merge = pd.read_csv(pathMerge, sep=",", dtype=str)

#filter for unique customer IDs
customers = clicks['Customer ID'].unique()
customers = customers[~pd.isnull(customers)] #delete empty customer ID

#read all customer who ordered
oCustomers = merge['Customer ID'].unique()

#mark customers who orderer
orderer = []
for customer in customers:
    if customer in oCustomers:
        orderer.append(1)
    else:
        orderer.append(0)

customers = pd.concat([pd.DataFrame(customers), pd.DataFrame(orderer)], axis=1)
customers.columns = ['Customer ID', 'Orderer']

customers.to_csv(pathCustomers, index=False)
