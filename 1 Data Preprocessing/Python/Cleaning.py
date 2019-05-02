import shutil
import pandas as pd

#variables
pathOrders = "../../orders/order_data.csv"
newPathOrders = "../../0 Data/order_data_P.csv"
pathOrdersClean = "../../0 Data/order_data_cleaned_P.csv"
pathOrdersSmall = "../../0 Data/order_data_small_P.csv"
pathOrdersHeaders = "../../orders/order_columns.txt"

pathClicks = "../../clickstream/clickstream_data.csv"
newPathClicks = "../../0 Data/clickstream_data_P.csv"
pathClicksClean = "../../0 Data/clickstream_data_cleaned_P.csv"
pathClicksSmall = "../../0 Data/clickstream_data_small_P.csv"
pathClicksHeaders = "../../clickstream/clickstream_columns.txt"

pathMerge = "../../0 Data/merged_P.csv"
pathMergeSmall = "../../0 Data/merged_small_P.csv"

nan = float('nan')

def headers(path):
    text_file = open(path)
    headers = text_file.read().split("\n")
    for counter, header in enumerate(headers):
        headers[counter] = (header.split(":"))[0]
        if not header: #for empty rows
            headers.pop(counter)
    return headers

# Copy files
shutil.copy(pathClicks, newPathClicks)
shutil.copy(pathOrders, newPathOrders)

pathOrders = newPathOrders
pathClicks = newPathClicks

# Read headers
clicksHeaders = headers(pathClicksHeaders)
ordersHeaders = headers(pathOrdersHeaders)

# Read data
clicks = pd.read_csv(pathClicks, sep=",", names=clicksHeaders, dtype=str, encoding="ISO-8859-1")
clicks = clicks.replace(to_replace="?", value=nan)
clicks = clicks.replace(to_replace="NULL", value=nan)
orders = pd.read_csv(pathOrders, sep=",", names=ordersHeaders, dtype=str, encoding="utf-8")
orders = orders.replace(to_replace="?", value=nan)
orders = orders.replace(to_replace="NULL", value=nan)

#Drop nan columns
clicks=clicks.dropna(axis=1)
order=orders.dropna(axis=1)

# Save to CSV
clicks.to_csv(path_or_buf=pathClicksClean, index=False)
(clicks.head(200)).to_csv(path_or_buf=pathClicksSmall, index=False)
print ("Clickstream:" + str(clicks.shape))
orders.to_csv(path_or_buf=pathOrdersClean, index=False)
(orders.head(200)).to_csv(path_or_buf=pathOrdersSmall, index=False)
print ("Orders:" + str(orders.shape))

#Merging
orders.rename(columns={"Order Line Session ID":"Session ID"}, inplace=True)

print(list(orders.columns.values))
print(list(clicks.columns.values))

mergedData = pd.merge(clicks, orders, on='Session ID', how='outer', suffixes=("_clicks","_orders"))
print(list(mergedData.columns.values))
print(list(mergedData.shape))
mergedData.to_csv(path_or_buf=pathMerge, index=False)
(mergedData.head(200)).to_csv(path_or_buf=pathMergeSmall, index=False)