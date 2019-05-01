import read_data
import pandas as pd



df = read_data.load_dataframe("order.pkl")


# drop rows where Oder Line Session ID is empty 1 column
df = df.dropna(subset=["Order Line Session ID"])
# drop rows where Order Line Subassortment ID is empty 1 column
df = df.dropna(subset=["Order Line Subassortment ID"])

#print(df)
# print(df.describe())
# print(df.dtypes)

# drop columns with double values
df = df.drop(columns=["Order System Number","Order ID","Order Line Hour of Day","Order Source","Order Visa Payment Amount",
                        "Order Master Card Payment Amount","Order Amex Payment Amount","Order Day of Week","Order Hour of Day","Household Status"])

# mearge columns Registration Gender and Gender to Gender and drop Registration Gender
for index, row in df.iterrows():
    if row["Registration Gender"] != row["Gender"]:        
        if pd.isnull(row["Registration Gender"])==True:
            print(row["Registration Gender"]," ",row["Gender"])
            df.at[index,"Registration Gender"] = row["Gender"]
        if pd.isnull(row["Gender"])==True:
            print(row["Registration Gender"]," ",row["Gender"])
            df.at[index,"Gender"] = row["Registration Gender"]
df = df.drop(columns=["Registration Gender"])
df = df.dropna(subset=["Gender"])
    




print(df[["Gender"]])

print(df.iloc[:,9])

# for index, row in df.iterrows():
#     if row[0] == 1:
#         print(row[0])

