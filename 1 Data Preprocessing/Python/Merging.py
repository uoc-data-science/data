import pandas as pd

# sources
pathOrders = '../../0 Data/order_data_cleaned_P.csv'
pathClicks = '../../0 Data/clickstream_data_cleaned_P.csv'

pathClickCustomer1 = '../../4 Data Overview/Tables/ ClickstreamDataCustomer_Factors.csv'
pathClickCustomer2 = '../../4 Data Overview/Tables/ ClickstreamDataCustomer_Numerical.csv'
pathOrderCustomer1 = '../../4 Data Overview/Tables/ CustomerData_Factors.csv'
pathOrderCustomer2 = '../../4 Data Overview/Tables/ CustomerData_Numerical.csv'

# results
pathMerge = "../../0 Data/merged_P.csv"
pathCustomers = "../../0 Data/merged_Customers_P.csv"

#############################################################################
# normal merge

# read data
clicks = pd.read_csv(pathClicks, sep=",", dtype=str)
orders = pd.read_csv(pathOrders, sep=",", dtype=str)

# merging
mergedData = pd.merge(clicks, orders, how='inner', left_on=['Customer ID'], right_on=['Customer ID'])

# save data
print(list(mergedData.shape))
mergedData.to_csv(path_or_buf=pathMerge, index=False)

#############################################################################
# customer data merge

# read data
clickCustomer1 = pd.read_csv(pathClickCustomer1, sep=",", dtype=str)
clickCustomer2 = pd.read_csv(pathClickCustomer2, sep=",", dtype=str)
orderCustomer1 = pd.read_csv(pathOrderCustomer1, sep=",", dtype=str)
orderCustomer2 = pd.read_csv(pathOrderCustomer2, sep=",", dtype=str)

# build subset on Customer Columns
clickCustomer1 = [w.replace('.', ' ') for w in clickCustomer1['Variable']]
clickCustomer1 = [w.replace('   ', '\. ') for w in clickCustomer1]
clickCustomer2 = [w.replace('.', ' ') for w in clickCustomer2['Variable']]
clickCustomer2 = [w.replace('   ', '\. ') for w in clickCustomer2]
clickCustomer1.extend(clickCustomer2)
clickCustomer1.append('Customer ID')
clicks = clicks[clickCustomer1]
clicks = clicks.drop_duplicates(subset=['Customer ID'])

orderCustomer1 = [w.replace('.', ' ') for w in orderCustomer1['Variable']]
orderCustomer2 = [w.replace('.', ' ') for w in orderCustomer2['Variable']]
orderCustomer1.extend(orderCustomer2)
orderCustomer1.append('Customer ID')
orders = (orders[orderCustomer1])
orders = orders.drop_duplicates(subset=['Customer ID'])

# merging
mergedCustomer = pd.merge(clicks, orders, how='inner', left_on=['Customer ID'], right_on=['Customer ID'], suffixes=('', '_y'))
mergedCustomer.drop(list(mergedCustomer.filter(regex='_y$')), axis=1, inplace=True) #delete duplicates
print(list(mergedCustomer.shape))
mergedCustomer.to_csv(path_or_buf=pathCustomers, index=False)

