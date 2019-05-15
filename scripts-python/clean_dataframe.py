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
        if (count_value == 0 or count_null/rowtotal > 0.999):
            drop_column_list.append([column,count_value])
    print(drop_column_list)
    df = drop_column_from_list(df,drop_column_list)
    print(df)
    return df


def drop_column_from_list(df,droplist):
    for row in droplist:
        df = df.drop(columns=row[0])   
    return df


def delete_empty_indexorcolumns_in_df(df,index_or_columns,threshold):
    df = df.dropna(axis=index_or_columns, thresh=threshold)
    return df


def clean_df_generic(df,threshold):
    print(df)
    index_or_columns = "columns"
    df = delete_empty_indexorcolumns_in_df(df,index_or_columns,threshold)
    index_or_columns = "index"
    df = delete_empty_indexorcolumns_in_df(df,index_or_columns,threshold)
    print(df)
    return df




def merge_columns(df,column_name1,column_name2):
    print(df[[column_name1,column_name2]])
    for index, row in df.iterrows():
        if row[column_name1] != row[column_name2]:        
            if pd.isnull(row[column_name1])==True:
                #print(row[column_name1]," ",row[column_name2])
                df.at[index,column_name1] = row[column_name2]
            if pd.isnull(row[column_name2])==True:
                #print(row[column_name1]," ",row[column_name2])
                df.at[index,column_name2] = row[column_name1]
    print(df[[column_name1,column_name2]])
    df = df.drop(columns=[column_name1])
    df = df.dropna(subset=[column_name2])
    print(df)
    return df


##generic cleaning 

#order 
df = read_data.load_dataframe("order.pkl")
df = clean_df_generic(df,1)

#clickstream
#df2 = read_data.load_dataframe("click.pkl")
#df2 = clean_df_generic(df2,1)



##semiauto_cleaning 

#order
df = merge_columns(df,"Registration Gender","Gender")



# merge columns Registration Gender and Gender to Gender and drop Registration Gender

    






