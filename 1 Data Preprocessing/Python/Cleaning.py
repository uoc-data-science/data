import shutil
import pandas as pd

#paths
pathOrders = "../../orders/order_data.csv"
newPathOrders = "../../0 Data/order_data_P.csv"
pathOrdersClean = "../../0 Data/order_data_cleaned_P.csv"
pathOrdersSmall = "../../0 Data/order_data_small_P.csv"
pathOrdersHeaders = "../../orders/order_columns.txt"

pathClicks = "../../clickstream/clickstream_data.csv"
pathClicks2 = "../../clickstream/clickstream_data_part_2.csv"
newPathClicks = "../../0 Data/clickstream_data_P.csv"
pathClicksClean = "../../0 Data/clickstream_data_cleaned_P.csv"
pathClicksSmall = "../../0 Data/clickstream_data_small_P.csv"
pathClicksHeaders = "../../clickstream/clickstream_columns.txt"

pathMerge = "../../0 Data/merged_P.csv"
pathMergeSmall = "../../0 Data/merged_small_P.csv"

#null values
nan = float('nan')

def headers(path):
    headers = open(path).\
              read().\
              split("\n")
    for counter, header in enumerate(headers):
        headers[counter] = (header.split(":"))[0]
        if not header: #for empty rows
            headers.pop(counter)
    return headers

def clean(df):
    #write NANs, delete empty columns
    df = df. \
         replace(to_replace="?", value=nan). \
         replace(to_replace="NULL", value=nan). \
         dropna(axis=1, how="all")
    #Clean Time and Date
    for column in df.columns:
        if "Time" in column:
            df[column] = df[column].\
                         str.\
                         replace('\\', '', regex=True)
    return df

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
clicks2 = pd.read_csv(pathClicks2, sep=",", names=clicksHeaders, dtype=str, encoding="ISO-8859-1")
clicks = clean(clicks.append(clicks2))
orders = clean(pd.read_csv(pathOrders, sep=",", names=ordersHeaders, dtype=str, encoding="utf-8"))

# Save to CSV
clicks.to_csv(path_or_buf=pathClicksClean, index=False)
(clicks.head(1000)).to_csv(path_or_buf=pathClicksSmall, index=False)
print("Clickstream:" + str(clicks.shape))
orders.to_csv(path_or_buf=pathOrdersClean, index=False)
(orders.head(1000)).to_csv(path_or_buf=pathOrdersSmall, index=False)
print("Orders:" + str(orders.shape))

#Merging
mergedData = pd.merge(clicks, orders,  how='inner', left_on=['Session ID'], right_on=['Order Session ID'])
print(list(mergedData.columns.values))
print(list(mergedData.shape))
mergedData.to_csv(path_or_buf=pathMerge, index=False)
(mergedData.head(10000)).to_csv(path_or_buf=pathMergeSmall, index=False)