import pandas as pd
import datetime
import numpy as np

pathClicks = '../../0 Data/clickstream_data_cleaned_P.csv'
pathResult = "../prediction_input_data.csv"

#############################################################################

# read data
clicksOrig = pd.read_csv(pathClicks, sep=",", dtype=str)

df = pd.DataFrame(columns=['Clicks','Duration_in_Seconds','REQUEST_DAY_OF_WEEK','REQUEST_HOUR_OF_DAY','Ordered'])
clickCount = '1'
minTime = 0
maxTime = 0
ordered = "No"
init = 1
for index, row in clicksOrig.iterrows():
    if row["Request Sequence"] == '1':
        if init == 0 and clickCount != '1':
            duration = (maxTime - minTime).total_seconds()
            df.loc[len(df)] = [str(clickCount), str(duration), str(day), str(hour), str(ordered)]
        if init == 1:
            init = 0
        ordered = "No"
        #new minTime
        dt = row['Request Date'] + ' ' + row['Request Date_Time']
        minTime = datetime.datetime.strptime(dt, '%Y-%m-%d %H:%M:%S')
        day = row["REQUEST_DAY_OF_WEEK"]
        hour = row["REQUEST_HOUR_OF_DAY"]
    if row["Request Template"] == "checkout/thankyou\.jhtml":
        ordered = "Yes"
    clickCount = row["Request Sequence"]
    dt = row['Request Date'] + ' ' + row['Request Date_Time']
    maxTime = datetime.datetime.strptime(dt, '%Y-%m-%d %H:%M:%S')

df.to_csv("../prediction_data.csv", index=False)

# one hot encode where needed
df['REQUEST_HOUR_OF_DAY'] = df['REQUEST_HOUR_OF_DAY'].apply(lambda x: "{}{}".format('Hour_', x))
one_hot = pd.get_dummies(df['REQUEST_HOUR_OF_DAY'])
df = df.drop('REQUEST_HOUR_OF_DAY',axis = 1)
df = df.join(one_hot)
df['REQUEST_DAY_OF_WEEK'] = df['REQUEST_DAY_OF_WEEK'].apply(lambda x: "{}{}".format('Weekday_', x))
one_hot = pd.get_dummies(df['REQUEST_DAY_OF_WEEK'])
df = df.drop('REQUEST_DAY_OF_WEEK',axis = 1)
df = df.join(one_hot)
df.to_csv(pathResult, index=False)



