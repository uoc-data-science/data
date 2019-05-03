import read_data
import pandas as pd

def delete_columns_with_nullonly_or_morethan99p(df):
    drop_column_list = []
    print(df)
    #print(df.count())
    for column in df:
        count_null = 0
        count_value = 0
        for row in df[column]:
            if pd.isnull(row)==True:
                count_null = count_null + 1
            elif pd.isnull(row)==False:
                count_value = count_value + 1
        #print(column)
        #print("Null rows: ",count_null, " Value rows", count_value)
        rowtotal = count_null + count_value
        #print(count_null/rowtotal) 
        if (count_value == 0 or count_null/rowtotal > 0.9999):
            drop_column_list.append([column,count_value])
    print(drop_column_list)
    df = drop_column_from_list(df,drop_column_list)
    print(df)
    return df
        

def drop_column_from_list(df,droplist):
    for row in droplist:
        df = df.drop(columns=row[0])   
    return df
    

        
        




df = read_data.load_dataframe("order.pkl")
delete_columns_with_nullonly_or_morethan99p(df)

df2 = read_data.load_dataframe("click.pkl")
delete_columns_with_nullonly_or_morethan99p(df2)


# # drop rows where Oder Line Session ID is empty 1 column
# df = df.dropna(subset=["Order Line Session ID"])
# # drop rows where Order Line Subassortment ID is empty 1 column
# df = df.dropna(subset=["Order Line Subassortment ID"])

# #print(df)
# # print(df.describe())
# # print(df.dtypes)

# # drop columns with double values
# df = df.drop(columns=["Order System Number","Order ID","Order Line Hour of Day","Order Source","Order Visa Payment Amount",
#                         "Order Master Card Payment Amount","Order Amex Payment Amount","Order Day of Week","Order Hour of Day","Household Status"])

# # mearge columns Registration Gender and Gender to Gender and drop Registration Gender
# for index, row in df.iterrows():
#     if row["Registration Gender"] != row["Gender"]:        
#         if pd.isnull(row["Registration Gender"])==True:
#             print(row["Registration Gender"]," ",row["Gender"])
#             df.at[index,"Registration Gender"] = row["Gender"]
#         if pd.isnull(row["Gender"])==True:
#             print(row["Registration Gender"]," ",row["Gender"])
#             df.at[index,"Gender"] = row["Registration Gender"]
# df = df.drop(columns=["Registration Gender"])
# df = df.dropna(subset=["Gender"])
    




# print(df[["Gender"]])

# print(df.iloc[:,9])

# # for index, row in df.iterrows():
# #     if row[0] == 1:
# #         print(row[0])

