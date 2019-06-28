import pandas as pd
import datetime
import numpy as np

pathClicks = '../../0 Data/clickstream_data_cleaned_P.csv'
pathMerge = "../../0 Data/merged_P.csv"
pathCustomers = "../customers.csv"
pathClickCustomer1 = '../../4 Data Overview/Tables/ ClickstreamDataCustomer_Factors.csv'
pathClickCustomer2 = '../../4 Data Overview/Tables/ ClickstreamDataCustomer_Numerical.csv'

#############################################################################

# read data
clicksOrig = pd.read_csv(pathClicks, sep=",", dtype=str)
merge = pd.read_csv(pathMerge, sep=",", dtype=str)
clickCustomer1 = pd.read_csv(pathClickCustomer1, sep=",", dtype=str)
clickCustomer2 = pd.read_csv(pathClickCustomer2, sep=",", dtype=str)

# build subset on Customer Columns
clickCustomer1 = [w.replace('.', ' ') for w in clickCustomer1['Variable']]
clickCustomer1 = [w.replace('   ', '\. ') for w in clickCustomer1]
clickCustomer2 = [w.replace('.', ' ') for w in clickCustomer2['Variable']]
clickCustomer2 = [w.replace('   ', '\. ') for w in clickCustomer2]
clickCustomer1.extend(clickCustomer2)
clickCustomer1.extend(['REQUEST_DAY_OF_WEEK','REQUEST_HOUR_OF_DAY', 'Customer ID'])
clicks = ((clicksOrig[clickCustomer1]).drop_duplicates(subset=['Customer ID']))[pd.notnull(clicksOrig['Customer ID'])]

#calculate session duration and click number
clicksOrig = clicksOrig[pd.notnull(clicksOrig['Customer ID'])]
df = pd.DataFrame(columns=['Customer ID','Clicks','Duration'])
maxClick = 0
minTime = 0
maxTime = 0
customerID = 0
for index, row in clicksOrig.iterrows():
    if row["Request Sequence"] == '1':
        if maxClick != 0:
            duration = maxTime - minTime
            df.loc[len(df)] = [str(customerID), str(maxClick), str(duration)]
        #new minTime
        dt = row['Request Date'] + ' ' + row['Request Date_Time']
        minTime = datetime.datetime.strptime(dt, '%Y-%m-%d %H:%M:%S')
    customerID = row["Customer ID"]
    maxClick = row["Request Sequence"]
    dt = row['Request Date'] + ' ' + row['Request Date_Time']
    maxTime = datetime.datetime.strptime(dt, '%Y-%m-%d %H:%M:%S')
df = df.drop_duplicates(subset=['Customer ID'])
clicks = pd.merge(clicks, df, how='left', left_on=['Customer ID'], right_on=['Customer ID'])

# read all customers who ordered
oCustomers = merge['Customer ID'].unique()

# mark customers who ordered
orderer = []
for index, row in clicks.iterrows():
    if row['Customer ID'] in oCustomers:
        orderer.append(1)
    else:
        orderer.append(0)

# save dataset
clicks["Orderer"] = orderer
clicks.to_csv(pathCustomers, index=False)




