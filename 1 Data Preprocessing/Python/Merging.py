import pandas as pd

pathIds = '../../1 Data Preprocessing/mergeIDs.csv'
pathOrders = '../../0 Data/order_data_cleaned_P.csv'
pathClicks = '../../0 Data/clickstream_data_cleaned_P.csv'

clicks = pd.read_csv(pathClicks, sep=",", dtype=str)
orders = pd.read_csv(pathOrders, sep=",", dtype=str)
ids = pd.read_csv(pathIds, sep=",", dtype=str)

shapes = []
for index, row in ids.iterrows():
    print(row)
    # Merging
    mergedData = pd.merge(clicks, orders, how='inner', left_on=[row['Clicks']], right_on=[row['Orders']])
    shapes.append(str(list(mergedData.shape)))

ids['Shape'] = shapes

ids.to_csv(path_or_buf=pathIds, index=False)
print(ids)