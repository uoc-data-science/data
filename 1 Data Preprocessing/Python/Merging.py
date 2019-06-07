import pandas as pd

pathOrders = '../../0 Data/order_data_cleaned_P.csv'
pathClicks = '../../0 Data/clickstream_data_cleaned_P.csv'

pathMerge = "../../0 Data/merged_P.csv"
pathMergeSmall = "../../0 Data/merged_small_P.csv"

clicks = pd.read_csv(pathClicks, sep=",", dtype=str)
orders = pd.read_csv(pathOrders, sep=",", dtype=str)

# Merging
mergedData = pd.merge(clicks, orders, how='inner', left_on=['Customer ID'], right_on=['Customer ID'])

print(list(mergedData.columns.values))
print(list(mergedData.shape))
mergedData.to_csv(path_or_buf=pathMerge, index=False)
(mergedData.head(10000)).to_csv(path_or_buf=pathMergeSmall, index=False)