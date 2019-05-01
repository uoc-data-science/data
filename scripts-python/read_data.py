import pandas as pd
import numpy as np 
import os.path
import csv
import pickle
import datetime


def read_raw_data(filename,column_file,samplename,dataset_name):
    filename = "../00_raw_data/"+filename
    columns, dtypes = read_raw_columns(column_file)
    df = pd.read_csv(filename, names=columns,low_memory=False, encoding="ansi")
    df = recreate_dtypes(df, dtypes)
    create_sampel_excel(df,samplename)
    save_dataframe(df,dataset_name)
    print(df)

def read_raw_columns(filename):
    filename = "../01_data_unterstanding/"+filename
    textfull= open(filename,"r")
    line_list = textfull.readlines()
    column_list = []
    dtype_list = []
    for line in line_list:
        string = line
        string_list = string.split(":")
        column_list.append(string_list[0])
        dtype_list.append(identify_column_type(string_list[1]))
    return column_list, dtype_list

def recreate_dtypes(dataframe, dtypes):
    for i in np.arange(0,len(dtypes),1):
        dataframe.T.iloc[i] = pd.Series(dataframe.T.iloc[i], dtype=dtypes[i])
    return dataframe

def identify_column_type(type):
    # continuous
	# date
	# time
	# ignore
	# element1,element2,element3.
	# [ordered] element1, element2, element3.
    type = type[1:len(type)-2]
    
    if(type == "continuous"):
        return np.float64
    if(type == "date"):
        return object
    if(type == "time"):
        return object
    if(type.startswith("[ordered]")):
        categories = split_ordered_categories(type)
        return pd.CategoricalDtype(categories=categories, ordered=True)
    category_list = type.split(',')
    if(len(category_list) > 1):
        return pd.CategoricalDtype(categories=category_list, ordered=False)
    
    return np.object
	
def split_ordered_categories(categories):
    category_list = []
    clean = categories[9:]
    splits = clean.split(',')
    for split in splits:
        category_list.append(split[1:])
    return category_list

def create_sampel_excel(dataframe,samplename):
    from pandas import ExcelWriter
    #create a Excelsheet with the heads and the first 100 lines for data unterstanding
    df = dataframe.iloc[:1000]
    samplename = "../01_data_unterstanding/"+samplename
    xlsx_writer = ExcelWriter(samplename)
    df.to_excel(xlsx_writer,"Sheet1",index=False)
    xlsx_writer.save()




def create_csv_from_dataframe(dataframe,filename):
    filename = "../00_raw_data/"+filename
    df = dataframe
    df.to_csv(path_or_buf=filename,index=False)




def save_dataframe(dataframe,filename):
    filename = "../00_raw_data/data_sets/"+filename
    dataframe.to_pickle(filename)




def load_dataframe(filename):
    filename = "../00_raw_data/data_sets/"+filename
    df= pd.read_pickle(filename)
    return df




